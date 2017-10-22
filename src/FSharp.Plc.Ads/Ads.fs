namespace FSharp.Plc.Ads.Experimental

[<AutoOpen>]
module Builder = 
  open TwinCAT.Ads
  open System.Collections.Generic
  open Microsoft.FSharp.Reflection
  open System.Collections.Concurrent
  open System

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
    let ok (e:'T) = Choice1Of3 e
    let nok err = Choice2Of3 err
    let ads code text = Choice3Of3 (code,text)

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
    open TwinCAT.PlcOpen
    

    let parseErrorCodes op (code: AdsErrorCode) = sprintf "%sAMS ERROR: %A" op code

    let readAny<'T> (client: TcAdsClient) (symName,handle,_,_,size,arrDim) =
      try
        match typeof<'T> with
          | str when str = typeof<TimeSpan> ->
            client.ReadAny(handle,typeof<uint32>) :?> uint32 |>  TimeBase.ValueToTime :> obj :?> 'T
          | str when str = typeof<DateTime> ->
            client.ReadAny(handle,typeof<uint32>) :?> uint32 |>  DateBase.ValueToDate :> obj :?> 'T
          | str when str = typeof<string> -> 
            client.ReadAny(handle,typeof<'T>, [|size|]) :?> 'T
          | arr when arr.IsArray && arr.GetElementType().IsValueType ->
            client.ReadAny(handle,typeof<'T>, [|arrDim|]) :?> 'T
          | _ -> 
            client.ReadAny(handle, typeof<'T>) :?> 'T
        |> Rail.ok
      with
        | :? AdsErrorException as adsEx ->
          sprintf "attempt to read %s" symName |> Rail.ads adsEx.ErrorCode 

        | ex -> ex.Message |> Rail.nok

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
        client.ReadWrite(cmd,len,respStream,adsStream)
        Rail.ok respStream
      with
        | :? AdsErrorException as adsEx ->
          sprintf "attempt to read/write many" |> Rail.ads adsEx.ErrorCode 

        | ex -> ex.Message |> Rail.nok

    let writeHeader (writer:AdsBinaryWriter) (vars: (string*int*int64*int64*int*int) seq) =
      vars
      |> Seq.iter (fun (_,handle,_,_,size,_) ->
        AdsReservedIndexGroups.SymbolValueByHandle |> int |> writer.Write
        handle |> writer.Write
        size |> writer.Write
      )
      Rail.ok (writer,vars)

        

  type AdsWrapper = internal {
    Client: TcAdsClient
    Symbols: IDictionary<string,string*int*int64*int64*int*int>
    SymbolLoader: TcAdsSymbolInfoLoader
  }
  with
    member __.Yield (x) = 
      ()

    member this.GetSymbolInfo symName =
      match this.Symbols.ContainsKey symName with
        | false ->
          try 
            let handle = this.Client.CreateVariableHandle symName
            let info = 
              fun () -> this.SymbolLoader.FindSymbol(symName) 
              |> lock this.SymbolLoader
            
            this.Symbols.Add (symName,(symName,handle,info.IndexGroup,info.IndexOffset,info.Size,info.SubSymbols |> Seq.cast<TcAdsSymbolInfo> |> Seq.length))
            Rail.ok (symName,handle,info.IndexGroup,info.IndexOffset,info.Size,info.SubSymbols |> Seq.cast<TcAdsSymbolInfo> |> Seq.length)
          with 
            | :? AdsErrorException as adsEx ->
              sprintf "creating handle for %s" symName |> Rail.ads adsEx.ErrorCode 

            | ex -> ex.Message |> Rail.nok
            
        | true -> 
          Rail.ok this.Symbols.[symName]

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
        use rdStream = new AdsStream(values |> Seq.length |> (*) 4)
        Helpers.readWrite this.Client 0xF081 (values |> Seq.length) adsStream rdStream
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

  let createClient (amsNetId: string) port =
    let client = new TcAdsClient()
    client.Connect(amsNetId,port)
    {
      Client = client
      Symbols = ConcurrentDictionary<string,string*int*int64*int64*int*int>()
      SymbolLoader = client.CreateSymbolInfoLoader()
    }


  
