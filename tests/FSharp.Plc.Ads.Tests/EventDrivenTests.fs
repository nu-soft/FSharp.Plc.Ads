module EventDrivenTests

open FSharp.Plc.Ads.Experimental
open NUnit.Framework
open TwinCAT.Ads
open System
open System.Linq
open AutoFixture
open System.Threading

let writeOnce<'T when 'T : struct> (client:TcAdsClient) symName (value: 'T) =
  let handle = client.CreateVariableHandle symName
  client.WriteAny(handle, value)
  client.DeleteVariableHandle handle

let writeArrOnce<'T when 'T : struct> (client:TcAdsClient) symName (value: 'T array) =
  let handle = client.CreateVariableHandle symName
  client.WriteAny(handle, value)
  client.DeleteVariableHandle handle

let writeStrArrOnce (client:TcAdsClient) symName  arrLen strLen (value: string array) =
  let handle = client.CreateVariableHandle symName
  client.WriteAny(handle, value, [| strLen ; arrLen|])
  client.DeleteVariableHandle handle

let writeTSArrOnce (client:TcAdsClient) symName (value: TimeSpan array) =
  use adsStream = new AdsStream(value |> Seq.length |> (*) 4)
  use writer = new AdsBinaryWriter(adsStream)
  value |> Seq.iter writer.WritePlcType
  
  let handle = client.CreateVariableHandle symName
  client.Write(handle,adsStream)
  client.DeleteVariableHandle handle

let writeDTArrOnce (client:TcAdsClient) symName (value: DateTime array) =

  use adsStream = new AdsStream(value |> Seq.length |> (*) 4)
  use writer = new AdsBinaryWriter(adsStream)
  value |> Seq.iter writer.WritePlcType
  
  let handle = client.CreateVariableHandle symName
  client.Write(handle,adsStream)
  client.DeleteVariableHandle handle
  

let writeStringOnce (client:TcAdsClient) symName len (value: string) =
  let handle = client.CreateVariableHandle symName
  client.WriteAny(handle, value, [| len |])
  client.DeleteVariableHandle handle

let writeDTOnce (client:TcAdsClient) symName (value:DateTime) =
  use adsStream = new AdsStream(4)
  use writer = new AdsBinaryWriter(adsStream)
  writer.WritePlcType(value)
  
  let handle = client.CreateVariableHandle symName
  client.Write(handle,adsStream)
  client.DeleteVariableHandle handle


let writeTSOnce (client:TcAdsClient) symName  (value:TimeSpan) =
  use adsStream = new AdsStream(4)
  use writer = new AdsBinaryWriter(adsStream)
  writer.WritePlcType(value)
  
  let handle = client.CreateVariableHandle symName
  client.Write(handle,adsStream)
  client.DeleteVariableHandle handle

let readDTOnce (client:TcAdsClient) symName  =
  use adsStream = new AdsStream(4)
  use reader = new AdsBinaryReader(adsStream)
  
  let handle = client.CreateVariableHandle symName
  client.Read(handle,adsStream) |> ignore
  
  let res = reader.ReadPlcDATE()
  client.DeleteVariableHandle handle
  res


let assertEqual (expected:'T) (act:Result<'T>) = 
  match act with
    | Choice1Of3 a -> Assert.AreEqual(expected,a)
    | Choice2Of3 err -> Assert.Fail err
    | Choice3Of3 (code,err) -> sprintf "%A: %s" code err |> Assert.Fail

let assertObserveEqual (expected:'T) (ob:Result<IObservable<'T>>) =
  let act = ref Unchecked.defaultof<'T>
  match ob with
    | Choice1Of3 a -> 
      a
      |> Observable.subscribe (fun v -> 
        act := v
      )
      |> ignore
      async {
        Thread.Sleep 1000
        Assert.AreEqual(expected,!act)
      } |>Async.StartAsTask |> ignore
    | Choice2Of3 err -> Assert.Fail err
    | Choice3Of3 (code,err) -> sprintf "%A: %s" code err |> Assert.Fail

let assertArraysEqual (expected:'T array) (act:Result<'T array>) = 
  match act with
    | Choice1Of3 a -> 
      Assert.AreEqual(expected|>Seq.length,a|>Seq.length)
      expected
      |> Seq.zip a
      |> Seq.iter Assert.AreEqual

    | Choice2Of3 err -> Assert.Fail err
    | Choice3Of3 (code,err) -> sprintf "%A: %s" code err |> Assert.Fail

let assertObserveArraysEqual (expected:'T array) (ob:Result<IObservable<'T array>>) = 
  let act = ref Array.empty<'T>
  match ob with
    | Choice1Of3 a -> 
      a
      |> Observable.subscribe (fun v -> 
        act := v
      )
      |> ignore
      async {
        Thread.Sleep 1000
        Assert.AreEqual(expected|>Seq.length,!act|>Seq.length)
        expected
        |> Seq.zip !act
        |> Seq.iter Assert.AreEqual
      } 
      |> Async.StartAsTask
      |> ignore

    | Choice2Of3 err -> Assert.Fail err
    | Choice3Of3 (code,err) -> sprintf "%A: %s" code err |> Assert.Fail

let fixture = new Fixture()
let spareFixture = new Fixture()

[<SetUp>]
let AutoFixtureSetup () =
  fixture.Register<TimeSpan>(fun _ -> spareFixture.Create<uint32>() |> float |> TimeSpan.FromMilliseconds)
  fixture.Register<DateTime>(fun _ -> spareFixture.Create<DateTime>().Date + (spareFixture.Create<uint32>() * 1000u |> float |> TimeSpan.FromMilliseconds))
  
  
(*
  Do not test for LINT and LWORD in TC2 as those types are not supported
*)
[<Test>]
let ``primitive types read`` () = 
  

  let setupClient = new TcAdsClient()
  setupClient.Connect("192.168.68.132.1.1", 801)
  let plc = createClient "192.168.68.132.1.1" 801
  
  let inline write symName  = writeOnce setupClient symName 
  
  let boolExp = fixture.Create<BOOL>()
  let byteExp = fixture.Create<BYTE>()
  let wordExp = fixture.Create<WORD>()
  let dwordExp = fixture.Create<DWORD>()
  let sintExp = fixture.Create<SINT>()
  let intExp = fixture.Create<INT>()
  let dintExp = fixture.Create<DINT>()
  let realExp = fixture.Create<REAL>()
  let lrealExp = fixture.Create<LREAL>()

  plc { observe ".boolVar" 100 0 } |> assertObserveEqual boolExp
  write  ".boolVar"  boolExp 
  plc { observe ".byteVar" 100 0 } |> assertObserveEqual byteExp
  write  ".byteVar"  byteExp 
  plc { observe ".wordVar" 100 0 } |> assertObserveEqual wordExp
  write  ".wordVar"  wordExp 
  plc { observe ".dwordVar" 100 0 } |> assertObserveEqual dwordExp
  write  ".dwordVar" dwordExp
  plc { observe ".sintVar" 100 0 } |> assertObserveEqual sintExp
  write  ".sintVar"  sintExp 
  plc { observe ".intVar" 100 0 } |> assertObserveEqual intExp
  write  ".intVar"   intExp 
  plc { observe ".dintVar" 100 0 } |> assertObserveEqual dintExp
  write  ".dintVar"  dintExp 
  plc { observe ".realVar" 100 0 } |> assertObserveEqual realExp
  write  ".realVar"  realExp 
  plc { observe ".lrealVar" 100 0 } |> assertObserveEqual lrealExp
  write  ".lrealVar" lrealExp
  Thread.Sleep 2000
  
      
[<Test>]
let ``Reading and writing of string variables - read`` () = 

  let setupClient = new TcAdsClient()
  setupClient.Connect("192.168.68.132.1.1", 801)
  let plc = createClient "192.168.68.132.1.1" 801
  
  let inline writeStr len symName  = writeStringOnce setupClient symName len
  
  let stringExp = fixture.Create<string>()
  let string1Exp = fixture.Create<string>() |> Seq.head |> string 

  plc { observe ".string80Var" 100 0} |> assertObserveEqual stringExp
  writeStr 80 ".string80Var" stringExp 
  plc { observe ".string1Var" 100 0} |> assertObserveEqual string1Exp
  writeStr 1 ".string1Var" string1Exp 
  Thread.Sleep 2000
 
[<Test>]
let ``Reading and writing of date and time variables - read`` () = 

  let setupClient = new TcAdsClient()
  setupClient.Connect("192.168.68.132.1.1", 801)
  let plc = createClient "192.168.68.132.1.1" 801
  
  
  let timeExp = fixture.Create<TimeSpan>()
  let todExp = fixture.Create<TimeSpan>()
  let dateTimeExp = fixture.Create<DateTime>()
  let dateExp = fixture.Create<DateTime>().Date
  

  
  plc { observe ".timeVar" 100 0 } |> assertObserveEqual timeExp
  writeTSOnce setupClient ".timeVar" timeExp
  plc { observe ".todVar"  100 0} |> assertObserveEqual todExp
  writeTSOnce setupClient ".todVar" todExp
  plc { observe ".dateVar" 100 0 } |> assertObserveEqual dateExp
  writeDTOnce setupClient ".dateVar" dateExp
  plc { observe ".dtVar" 100 0 } |> assertObserveEqual dateTimeExp
  writeDTOnce setupClient ".dtVar" dateTimeExp
  Thread.Sleep 2000
  
[<Test>]
let ``1-dim arrays read`` () = 

  let setupClient = new TcAdsClient()
  setupClient.Connect("192.168.68.132.1.1", 801)
  let plc = createClient "192.168.68.132.1.1" 801
  
  let inline write symName  = writeArrOnce setupClient symName 

  
  
  let boolExp = fixture.CreateMany<BOOL>(21).ToArray()
  let byteExp = fixture.CreateMany<BYTE>(1).ToArray()
  let wordExp = fixture.CreateMany<WORD>(19).ToArray()
  let dwordExp = fixture.CreateMany<DWORD>(18).ToArray()
  let sintExp = fixture.CreateMany<SINT>(17).ToArray()
  let intExp = fixture.CreateMany<INT>(15).ToArray()
  let dintExp = fixture.CreateMany<DINT>(count = 13).ToArray()
  let realExp = fixture.CreateMany<REAL>(11).ToArray()
  let lrealExp = fixture.CreateMany<LREAL>(10).ToArray()
  let stringExp = fixture.CreateMany<string>(9).ToArray()
  let timeExp = fixture.CreateMany<TimeSpan>(6).ToArray()
  let todExp = fixture.CreateMany<TimeSpan>(5).ToArray()
  let dateExp = fixture.CreateMany<DateTime>(4).ToArray()
  let dtExp = fixture.CreateMany<DateTime>(3).ToArray() |> Array.map (fun dt -> dt.Date)


  plc { observe ".arrboolVar" 100 0 }    |> assertObserveArraysEqual boolExp
  write  ".arrboolVar"  boolExp 
  plc { observe ".arrbyteVar" 100 0 }    |> assertObserveArraysEqual byteExp
  write  ".arrbyteVar"  byteExp 
  plc { observe ".arrwordVar" 100 0 }    |> assertObserveArraysEqual wordExp
  write  ".arrwordVar"  wordExp 
  plc { observe ".arrdwordVar" 100 0 }    |> assertObserveArraysEqual dwordExp
  write  ".arrdwordVar" dwordExp
  plc { observe ".arrsintVar" 100 0 }    |> assertObserveArraysEqual sintExp
  write  ".arrsintVar"  sintExp 
  plc { observe ".arrintVar"   100 0  }    |> assertObserveArraysEqual intExp
  write  ".arrintVar"   intExp 
  plc { observe ".arrdintVar" 100 0 }    |> assertObserveArraysEqual dintExp
  write  ".arrdintVar"  dintExp 
  plc { observe ".arrrealVar" 100 0 }    |> assertObserveArraysEqual realExp
  write  ".arrrealVar"  realExp 
  plc { observe ".arrlrealVar" 100 0 }    |> assertObserveArraysEqual lrealExp
  write  ".arrlrealVar" lrealExp
  plc { observe ".arrstringVar" 100 0 }    |> assertObserveArraysEqual stringExp
  writeStrArrOnce setupClient ".arrstringVar" 9 80 stringExp 
  plc { observe ".arrtimeVar" 100 0 }    |> assertObserveArraysEqual timeExp
  writeTSArrOnce setupClient  ".arrtimeVar" timeExp
  plc { observe ".arrtodVar" 100 0 }    |> assertObserveArraysEqual todExp
  writeTSArrOnce setupClient  ".arrtodVar" todExp
  plc { observe ".arrdateVar" 100 0 }    |> assertObserveArraysEqual dateExp
  writeDTArrOnce setupClient  ".arrdateVar" dateExp
  plc { observe ".arrdtVar" 100 0 }    |> assertObserveArraysEqual dtExp
  writeDTArrOnce setupClient  ".arrdtVar" dtExp
  Thread.Sleep 2000
  
