namespace FSharp.Plc.Ads.Experimental

[<AutoOpen>]
module Builder = 
  open TwinCAT.Ads
  open System.Collections.Generic
  open Microsoft.FSharp.Reflection
  open System.Collections.Concurrent
  open System
  open TwinCAT.Ads.Internal
  open System.Diagnostics

  type Unsubscriber<'T> =
    private {
      Observers: List<IObserver<'T>>
      Observer: IObserver<'T>
    }
    interface IDisposable with
      member this.Dispose() =
        match this.Observer with
        | null -> ()
        | observer ->
          if this.Observers.Contains(observer) then
            this.Observers.Remove(observer) |> ignore
    
  type INotify =
    abstract member Notify: obj -> unit
    abstract member Error: exn -> unit

  type AdsObservable<'T> = 
    private {
      Observers: List<IObserver<'T>>
    }
    member this.Notify (value: 'T) = 
      this.Observers
      |> Seq.iter (fun ob -> 
        ob.OnNext value
      )
    interface INotify with
      member this.Error err =
        this.Observers
        |> Seq.iter (fun ob -> 
          ob.OnError err
        )

      member this.Notify value =
        match typeof<'T> with
        | t when t = typeof<DateTime> ->
          value :?> uint32 |> DateBase.ValueToDate :> obj :?> 'T
        | t when t = typeof<TimeSpan> ->
          value :?> uint32 |> TimeBase.ValueToTime :> obj :?> 'T
        | t when t = typeof<TimeSpan array> ->
          value
          :?> uint32 array 
          |> Array.map TimeBase.ValueToTime 
          :> obj 
          :?> 'T
        | t when t = typeof<DateTime array> ->
          value
          :?> uint32 array 
          |> Array.map DateBase.ValueToDate
          :> obj 
          :?> 'T
        | _ -> value :?> 'T
        |> this.Notify

    interface IObservable<'T> with
      member this.Subscribe(observer) =
        if this.Observers.Contains(observer) |> not then
          this.Observers.Add(observer)
        {
          Observer = observer;
          Observers = this.Observers
        }
        :> IDisposable
  module AdsObservable =
    let create<'T> = 
      {
        Observers = List<IObserver<'T>>()
      }

  

  type Result<'T> = Choice<'T,string,AdsErrorCode * string>
  
  module Result =
    let isOk: Result<'T> -> bool = function | Choice1Of3 _ -> true | _ -> false
    let isNok: Result<'T> -> bool = function | Choice2Of3 _ -> true | _ -> false  
    let isAdsNok: Result<'T> -> bool = function | Choice3Of3 _ -> true | _ -> false  
    let value: Result<'T> -> 'T = function 
      | Choice1Of3 v -> v 
      | Choice2Of3 _ -> 
        InvalidOperationException "Result is error" |> raise 
      | Choice3Of3 _ ->
        InvalidOperationException "Result is ADS error" |> raise
    let error: Result<'T> -> string = function 
      | Choice1Of3 _ -> 
        InvalidOperationException "Result is value" |> raise
      | Choice2Of3 err -> err 
      | Choice3Of3 _ -> 
        InvalidOperationException "Result is ADS error" |> raise

    let adsCode: Result<'T> -> AdsErrorCode = function 
      | Choice1Of3 _ -> InvalidOperationException "Result is ok" |> raise
      | Choice2Of3 _ -> InvalidOperationException "Result is non-ADS error" |> raise
      | Choice3Of3 (code,_) -> code

    let adsError: Result<'T> -> string = function 
      | Choice1Of3 _ -> InvalidOperationException "Result is ok" |> raise
      | Choice2Of3 _ -> InvalidOperationException "Result is non-ADS error" |> raise
      | Choice3Of3 (_,err) -> err

  module Rail =
    let ok (e:'T) : Result<'T> = Choice1Of3 e
    let nok err : Result<'T> = Choice2Of3 err
    let ads code text : Result<'T> = Choice3Of3 (code,text)

    let bind<'I, 'T> (f:'I -> Result<'T>) (e: Result<'I>) = 
      match e with
        | Choice1Of3 e -> f e
        | Choice2Of3 e -> nok e
        | Choice3Of3 (c,t) -> ads c t

    let map<'I, 'T> (f: 'I -> Result<'T>) (collection: 'I seq) = 

      collection
      |> Seq.map f
      |> Seq.fold (fun acc ele ->
        match acc with
          | Choice1Of3 l ->
            match ele with
              | Choice1Of3 a -> ok (a::l)
              | Choice2Of3 a -> nok a
              | Choice3Of3 (c,t) -> ads c t
          | Choice2Of3 e -> nok e
          | Choice3Of3 (c,t) -> ads c t
      ) (ok [])
      |> bind (function e -> e |> List.rev |> Seq.ofList |> ok)
    

  module Helpers =
    //open TwinCAT.PlcOpen
    open TwinCAT.Ads.Internal
    open System.Runtime.InteropServices
    
    let inline retype<'T,'U> (x:'T) : 'U = (# "" x : 'U #)
    let parseErrorCodes op (code: AdsErrorCode) = sprintf "%sAMS ERROR: %A" op code
    let readAny<'T> (client: TcAdsClient) (symName,handle,ig,io,size,sym) =
      let arrDim = sym |> Seq.length
      try
        match typeof<'T> with
          | str when str = typeof<TimeSpan> ->
            client.ReadAny(handle,typeof<uint32>) :?> uint32 |>  TimeBase.ValueToTime :> obj :?> 'T
          | str when str = typeof<DateTime> ->
            client.ReadAny(handle,typeof<uint32>) :?> uint32 |>  DateBase.ValueToDate :> obj :?> 'T
          | str when str = typeof<TimeSpan array> ->
            client.ReadAny(handle,typeof<uint32 array>, [|arrDim|])
            :?> uint32 array 
            |> Array.map TimeBase.ValueToTime 
            :> obj 
            :?> 'T
          | str when str = typeof<DateTime array> ->
            client.ReadAny(handle,typeof<uint32 array>, [|arrDim|])
            :?> uint32 array 
            |> Array.map DateBase.ValueToDate
            :> obj 
            :?> 'T
          | str when str = typeof<string> -> 
            client.ReadAny(handle,typeof<'T>, [|size|]) :?> 'T
          | str when str = typeof<string array> -> 
            client.ReadAny(handle,typeof<'T>, [| size / arrDim - 1; arrDim |]) :?> 'T
          | arr when arr.IsArray && arr.GetElementType().IsValueType ->
            client.ReadAny(handle,typeof<'T>, [|arrDim|]) :?> 'T
          | arr when arr.IsArray && FSharpType.IsRecord <| arr.GetElementType() ->
            use adsStream = new AdsStream(size);
            client.Read(handle,adsStream)
            adsStream.Position <- 0L
            use reader = new AdsBinaryReader(adsStream);
            //let res = 
            sym
            |> Seq.map (fun (s: TcAdsSymbolInfo) ->
              //let fields = 
              Seq.zip 
                (arr.GetElementType() |> FSharpType.GetRecordFields)
                (s.SubSymbols |> Seq.cast<TcAdsSymbolInfo>)
              |> Seq.map (fun (pt,si) ->
                match si.Datatype with
                | AdsDatatypeId.ADST_INT8 -> reader.ReadSByte()    :> obj
                | AdsDatatypeId.ADST_INT16 -> reader.ReadInt16()   :> obj
                | AdsDatatypeId.ADST_INT32 -> reader.ReadInt32()   :> obj
                | AdsDatatypeId.ADST_INT64 -> reader.ReadInt64()   :> obj
                | AdsDatatypeId.ADST_UINT8 -> reader.ReadByte()    :> obj
                | AdsDatatypeId.ADST_UINT16 -> reader.ReadUInt16() :> obj
                | AdsDatatypeId.ADST_UINT32 -> reader.ReadUInt32() :> obj
                | AdsDatatypeId.ADST_UINT64 -> reader.ReadUInt64() :> obj
                | AdsDatatypeId.ADST_STRING -> String.Join("",reader.ReadBytes(si.Size) |> Seq.takeWhile ((<>) 0uy) |> Seq.map (char >> string)) :> obj
              )
              |> Array.ofSeq
              |> (fun a -> 
                arr.GetElementType(),a,true
              )
              |> FSharpValue.MakeRecord
              |> fun o -> Convert.ChangeType(o,arr.GetElementType())
            )
            |> Array.ofSeq
            |> retype
          | _ -> 
            client.ReadAny(handle, typeof<'T>) :?> 'T
        |> Rail.ok
      with
        | :? AdsErrorException as adsEx ->
          sprintf "attempt to read %s" symName |> Rail.ads adsEx.ErrorCode 

        | ex -> ex.Message |> sprintf "attempt to read %s: %s" symName |> Rail.nok

    let observe<'T> (client: TcAdsClient) observer (cycleTime,maxDelay) (symName:string,_,_,_,size,sym) =
      let arrDim = sym |> Seq.length
      try
        let _ =
          match typeof<'T> with
            | str when str = typeof<TimeSpan> ->
              client.AddDeviceNotificationEx(symName.ToUpperInvariant(),AdsTransMode.OnChange,cycleTime,maxDelay, observer,typeof<uint32>)
            | str when str = typeof<DateTime> ->
              client.AddDeviceNotificationEx(symName.ToUpperInvariant(),AdsTransMode.OnChange,cycleTime,maxDelay, observer,typeof<uint32>)
            | str when str = typeof<TimeSpan array> ->
              client.AddDeviceNotificationEx(symName.ToUpperInvariant(),AdsTransMode.OnChange,cycleTime,maxDelay, observer,typeof<uint32 array>,[|arrDim|])
            | str when str = typeof<DateTime array> ->
              client.AddDeviceNotificationEx(symName.ToUpperInvariant(),AdsTransMode.OnChange,cycleTime,maxDelay, observer,typeof<uint32 array>,[|arrDim|])
            | str when str = typeof<string> -> 
              client.AddDeviceNotificationEx(symName.ToUpperInvariant(),AdsTransMode.OnChange,cycleTime,maxDelay, observer,typeof<'T>, [|size|])
            | str when str = typeof<string array> -> 
              client.AddDeviceNotificationEx(symName.ToUpperInvariant(),AdsTransMode.OnChange,cycleTime,maxDelay, observer,typeof<'T>, [| size / arrDim - 1; arrDim |])
            | arr when arr.IsArray && arr.GetElementType().IsValueType ->
              client.AddDeviceNotificationEx(symName.ToUpperInvariant(),AdsTransMode.OnChange,cycleTime,maxDelay, observer,typeof<'T>, [| arrDim |])
            | _ -> 
              client.AddDeviceNotificationEx(symName.ToUpperInvariant(),AdsTransMode.OnChange,cycleTime,maxDelay, observer,typeof<'T>)
        observer
        :> IObservable<'T>
        |> Rail.ok
      with
        | :? AdsErrorException as adsEx ->
          sprintf "attempt to read %s" symName |> Rail.ads adsEx.ErrorCode 
        | ex -> ex.Message |> sprintf "attempt to read %s: %s" symName |> Rail.nok

    let writeAny (client: TcAdsClient) (value: 'T) (symName, handle,_,_,size,_) =
      let type' = typeof<'T>
      try
        match type' with
          | str when str = typeof<TimeSpan> ->
            client.WriteAny(handle, value :> obj :?> TimeSpan |> TimeBase.TimeToValue |> uint32 ) 
          | str when str = typeof<DateTime> ->
            client.WriteAny(handle,value :> obj :?> DateTime |> DateBase.DateToValue  |> uint32)
          | str when str = typeof<string> ->
            client.WriteAny(handle, value, [| size |])
          | _ ->
            client.WriteAny(handle, value)
        Rail.ok ()
      with
        | :? AdsErrorException as adsEx ->
          sprintf "attempt to write %s" symName |> Rail.ads adsEx.ErrorCode 

        | ex -> ex.Message |> Rail.nok

    let readWrite (client: TcAdsClient) (cmd: int) len adsStream respStream =
      try
        client.ReadWrite(cmd,len,respStream,adsStream) |> ignore
        Rail.ok respStream
      with
        | :? AdsErrorException as adsEx ->
          sprintf "attempt to read/write many" |> Rail.ads adsEx.ErrorCode 

        | ex -> ex.Message |> Rail.nok

    let writeHeader (writer:AdsBinaryWriter) (vars: (string*int*int64*int64*int*TcAdsSymbolInfo seq) seq) =
      vars
      |> Seq.iter (fun (_,handle,_,_,size,_) ->
        AdsReservedIndexGroups.SymbolValueByHandle |> int |> writer.Write
        handle |> writer.Write
        size |> writer.Write
      )
      Rail.ok (writer,vars)

        

  type AdsWrapper = internal {
    Client: TcAdsClient
    Symbols: IDictionary<string,string*int*int64*int64*int*TcAdsSymbolInfo seq>
    SymbolLoader: TcAdsSymbolInfoLoader
  }
  with
    member __.Yield (_) = 
      ()

    member this.GetSymbolInfo symName =
      (fun () ->
        match this.Symbols.ContainsKey symName with
          | false ->
            try 
              let handle = this.Client.CreateVariableHandle symName
              let info = this.SymbolLoader.FindSymbol(symName) 
                
              this.Symbols.Add (symName,(symName,handle,info.IndexGroup,info.IndexOffset,info.Size,info.SubSymbols |> Seq.cast<TcAdsSymbolInfo>))
              Rail.ok (symName,handle,info.IndexGroup,info.IndexOffset,info.Size,info.SubSymbols |> Seq.cast<TcAdsSymbolInfo>)
            with 
              | :? AdsErrorException as adsEx ->
                sprintf "creating handle for %s" symName |> Rail.ads adsEx.ErrorCode 

              | ex -> ex.Message |> Rail.nok
            
          | true -> 
            Rail.ok this.Symbols.[symName]
      )
      |> lock this.Symbols

    [<CustomOperation("readAny")>] 
    member this.ReadAny<'T> (_, symName) : Result<'T>  =
      this.GetSymbolInfo symName
      |> Rail.bind (Helpers.readAny this.Client) 

    [<CustomOperation("readMany")>] 
    member this.ReadMany<'T> (_, symNames: obj) =
      use adsStream = new AdsStream()
      use writer = new AdsBinaryWriter(adsStream)
      let symNames =  FSharpValue.GetTupleFields symNames |> Seq.cast<string>
      let fields = 
        symNames
        |> Rail.map this.GetSymbolInfo
        
    
      fields 
      |> Rail.bind (fun vars ->
        
        let streamLength = 
          vars
          |> Seq.fold (fun acc (_,handle,_,_,size,_) ->    
              AdsReservedIndexGroups.SymbolValueByHandle |> int |> writer.Write
              handle |> writer.Write
              size |> writer.Write
              acc + size
          ) (vars |> Seq.length |> (*) 4)
        new AdsStream(streamLength)
        |> Rail.ok
      )
      |> Rail.bind (Helpers.readWrite this.Client 0xF080 (symNames |> Seq.length) adsStream)
      |> Rail.bind (fun rdStream ->
        use reader = new AdsBinaryReader(rdStream)
        symNames
        |> Rail.map (fun symName ->
          match  reader.ReadInt32 () |> LanguagePrimitives.EnumOfValue with
          | AdsErrorCode.NoError -> Rail.ok ()
          | code -> sprintf "error while reading %s in read many" symName |> Rail.ads code
        )
        |> Rail.bind (fun _ -> Rail.ok reader)
      )
      |> Rail.bind (fun reader ->
        FSharpType.GetTupleElements typeof<'T>
        |> Rail.map (fun type' ->
          match type' with
            | t when t = typeof<BOOL>->    reader.ReadBoolean() :> obj |> Rail.ok
            | t when t = typeof<BYTE>->    reader.ReadByte()    :> obj |> Rail.ok
            | t when t = typeof<INT >->    reader.ReadInt16()   :> obj |> Rail.ok
            | t when t = typeof<LINT>->    reader.ReadInt64()   :> obj |> Rail.ok
            | t when t = typeof<DINT>->    reader.ReadInt32()   :> obj |> Rail.ok
            | t when t = typeof<SINT>->    reader.ReadSByte()   :> obj |> Rail.ok
            | t when t = typeof<WORD>->    reader.ReadUInt16()  :> obj |> Rail.ok
            | t when t = typeof<DWORD>->   reader.ReadUInt32()  :> obj |> Rail.ok
            | t when t = typeof<LWORD>->   reader.ReadUInt64()  :> obj |> Rail.ok
            | t when t = typeof<REAL >->   reader.ReadSingle()  :> obj |> Rail.ok
            | t when t = typeof<LREAL>->   reader.ReadDouble()  :> obj |> Rail.ok
            | t when t = typeof<string> -> reader.ReadString()  :> obj |> Rail.ok
            | t when t = typeof<TimeSpan> -> reader.ReadPlcTIME()  :> obj |> Rail.ok
            
            | t -> sprintf "Unexpected type: %A" t |> Rail.nok 
        )
      )
      |> Rail.bind (Seq.toArray >> Rail.ok)
      |> Rail.bind (fun ele -> FSharpValue.MakeTuple(ele,typeof<'T>) :?> 'T |> Rail.ok)
    
    [<CustomOperation("writeAny")>] 
    member this.WriteAny<'T> (_, symName, value: 'T) =
      this.GetSymbolInfo symName 
      |> Rail.bind (Helpers.writeAny this.Client value)
        
      
    [<CustomOperation("writeMany")>] 
    member this.WriteMany (_: obj, values: (string * obj) seq) =
      use adsStream = new AdsStream()
      use writer = new AdsBinaryWriter(adsStream)
      values 
      |> Rail.map (fun (name,_) -> this.GetSymbolInfo name)
      |> Rail.bind (Helpers.writeHeader writer)
      |> Rail.bind (fun (writer,vars) ->
        vars
        |> Seq.zip (Seq.map snd values)
        |> Rail.map (fun (value,(_,_,_,_,size,_)) ->
          match value with
            | :? BOOL as v-> writer.Write(v)  |> Rail.ok
            | :? BYTE as v-> writer.Write(v)  |> Rail.ok
            | :? INT as v-> writer.Write(v)   |> Rail.ok
            | :? LINT as v-> writer.Write(v)  |> Rail.ok
            | :? DINT as v-> writer.Write(v)  |> Rail.ok
            | :? SINT as v-> writer.Write(v)  |> Rail.ok
            | :? WORD as v-> writer.Write(v)  |> Rail.ok
            | :? DWORD as v-> writer.Write(v) |> Rail.ok
            | :? LWORD as v-> writer.Write(v) |> Rail.ok
            | :? REAL as v-> writer.Write(v)  |> Rail.ok
            | :? LREAL as v-> writer.Write(v) |> Rail.ok
            | :? string as s -> 
              s |> Seq.iter writer.Write
              seq { 1 .. (size  - Seq.length s) }
              |> Seq.iter (fun _ ->
                writer.Write(0uy)
              )
              Rail.ok ()
            | _ -> sprintf "Type %s not yet supported" (value.GetType().FullName) |> Rail.nok
        )
      )
      |> Rail.bind (fun _ ->
        let rdStream = new AdsStream(values |> Seq.length |> (*) 4)
        Helpers.readWrite this.Client 0xF081 (values |> Seq.length) adsStream rdStream
        //this.Client.ReadWrite(0xF081,(values |> Seq.length),adsStream,rdStream)
        //Rail.ok rdStream
      )
      |> Rail.bind (fun rdStream ->
        use reader = new AdsBinaryReader(rdStream)
        values
        |> Rail.map (fun (symName,_) ->
          match  reader.ReadInt32 () |> LanguagePrimitives.EnumOfValue with
          | AdsErrorCode.NoError -> Rail.ok ()
          | code -> sprintf "error while writing %s in write many" symName |> Rail.ads code
        )
        |> Rail.bind (ignore >> Rail.ok)
      )
      
    [<CustomOperation("observe")>] 
    member this.Observe<'T> (_, symName:string,cycleTime,maxDelay) : Result<IObservable<'T>> =
      let observer = AdsObservable.create<'T>
      
      this.GetSymbolInfo symName
      |> Rail.bind (Helpers.observe this.Client observer (cycleTime,maxDelay)) 

    [<CustomOperation("observeStatus")>] 
    member this.ObserveStatus (_:obj)  =
      this.Client.IsConnected
      |> function
      | true ->
        try
          this.Client.AdsStateChanged 
          |> Rail.ok
        with 
        | ex -> Rail.nok ex.Message
      | false -> Rail.nok "Client not connected"
      
    [<CustomOperation("readState")>] 
    member this.ReadState (_:obj)  =
      try
        this.Client.ReadState() |> Rail.ok
      with 
      | ex -> Rail.nok ex.Message

    [<CustomOperation("removeHandles")>] 
    member this.RemoveHandles (_:obj)  =
      (fun () ->
        this.Symbols.Values
        |> Seq.iter (fun (_,handle,_,_,_,_) -> this.Client.DeleteVariableHandle(handle))
        this.Symbols.Clear()
      )
      |> lock this.Symbols

  let createClient (amsNetId: string) port sync =
    let client = new TcAdsClient()
    client.Connect(amsNetId,port)
    client.Synchronize <- sync
    client.AdsNotificationEx
    |> Observable.subscribe (fun e -> 
      let uData = e.UserData :?> INotify
      try
        e.Value
        |> uData.Notify 
        ()
      with
      | e -> uData.Error e
    )
    |> ignore
    {
      Client = client
      Symbols = ConcurrentDictionary<string,string*int*int64*int64*int*TcAdsSymbolInfo seq>()
      SymbolLoader = client.CreateSymbolInfoLoader()
    }