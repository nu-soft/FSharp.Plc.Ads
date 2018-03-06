module Tests

open FSharp.Plc.Ads.Experimental
open NUnit.Framework
open TwinCAT.Ads
open System
open System.Linq
open AutoFixture

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


let readTSOnce (client:TcAdsClient) symName  =
  use adsStream = new AdsStream(4)
  use reader = new AdsBinaryReader(adsStream)
  
  let handle = client.CreateVariableHandle symName
  client.Read(handle,adsStream) |> ignore
  
  let res = reader.ReadPlcTIME()
  client.DeleteVariableHandle handle
  res

let readOnce<'T when 'T : struct> (client:TcAdsClient) symName  =
  let handle = client.CreateVariableHandle symName
  let res = client.ReadAny(handle, typeof<'T>) :?> 'T
  client.DeleteVariableHandle handle
  res
let readArrOnce<'T when 'T : struct> (client:TcAdsClient) symName len  =
  let handle = client.CreateVariableHandle symName
  let res = client.ReadAny(handle, typeof<'T array>, [| len |]) :?> 'T array
  client.DeleteVariableHandle handle
  res
let readStrOnce (client:TcAdsClient) symName len =
  let handle = client.CreateVariableHandle symName
  let res = client.ReadAny(handle, typeof<string>, [| len |] ) :?> string
  client.DeleteVariableHandle handle
  res

let assertEqual (expected:'T) (act:Result<'T>) = 
  match act with
    | Choice1Of3 a -> Assert.AreEqual(expected,a)
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

let fixture = new Fixture()
let spareFixture = new Fixture()

[<SetUp>]
let AutoFixtureSetup () =
  fixture.Register<TimeSpan>(fun _ -> spareFixture.Create<uint32>() |> float |> TimeSpan.FromMilliseconds)
  fixture.Register<DateTime>(fun _ -> spareFixture.Create<DateTime>().Date + (spareFixture.Create<uint32>() * 1000u |> float |> TimeSpan.FromMilliseconds))
  

[<Test>]
let ``Try create handle to nonexisting symbolic name`` () = 

    let plc = createClient "192.168.68.132.1.1" 801
    let res = plc { readAny ".nonexistingVar" }

    res |> Result.isAdsNok |> Assert.IsTrue

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

  write  ".boolVar"  boolExp 
  write  ".byteVar"  byteExp 
  write  ".wordVar"  wordExp 
  write  ".dwordVar" dwordExp
  write  ".sintVar"  sintExp 
  write  ".intVar"   intExp 
  write  ".dintVar"  dintExp 
  write  ".realVar"  realExp 
  write  ".lrealVar" lrealExp

  plc { readAny ".boolVar" }    |> assertEqual boolExp
  plc { readAny ".byteVar" }    |> assertEqual byteExp
  plc { readAny ".wordVar" }    |> assertEqual wordExp
  plc { readAny ".dwordVar"}    |> assertEqual dwordExp
  plc { readAny ".sintVar" }    |> assertEqual sintExp
  plc { readAny ".intVar"  }    |> assertEqual intExp
  plc { readAny ".dintVar" }    |> assertEqual dintExp
  plc { readAny ".realVar" }    |> assertEqual realExp
  plc { readAny ".lrealVar"}    |> assertEqual lrealExp
  
(*
  Do not test for LINT and LWORD in TC2 as those types are not supported
*)
[<Test>]
let ``primitive types write`` () = 

  //Set ADS in Start
  let setupClient = new TcAdsClient()
  
  setupClient.Connect("192.168.68.132.1.1", 801)
  let plc = createClient "192.168.68.132.1.1" 801

  let boolExp = fixture.Create<BOOL>()
  let byteExp = fixture.Create<BYTE>()
  let wordExp = fixture.Create<WORD>()
  let dwordExp = fixture.Create<DWORD>()
  
  let sintExp = fixture.Create<SINT>()
  let intExp = fixture.Create<INT>()
  let dintExp = fixture.Create<DINT>()
  let realExp = fixture.Create<REAL>()
  let lrealExp = fixture.Create<LREAL>()
    
  plc { writeAny ".boolVar" boolExp }   |> Result.isOk |> Assert.IsTrue
  plc { writeAny ".byteVar" byteExp }   |> Result.isOk |> Assert.IsTrue
  plc { writeAny ".wordVar" wordExp }   |> Result.isOk |> Assert.IsTrue
  plc { writeAny ".dwordVar" dwordExp } |> Result.isOk |> Assert.IsTrue
  plc { writeAny ".sintVar" sintExp }   |> Result.isOk |> Assert.IsTrue
  plc { writeAny ".intVar" intExp }     |> Result.isOk |> Assert.IsTrue
  plc { writeAny ".dintVar" dintExp }   |> Result.isOk |> Assert.IsTrue
  plc { writeAny ".realVar" realExp }   |> Result.isOk |> Assert.IsTrue
  plc { writeAny ".lrealVar" lrealExp } |> Result.isOk |> Assert.IsTrue

  let boolAct:BOOL =  readOnce setupClient ".boolVar"
  let byteAct:BYTE =  readOnce setupClient ".byteVar"
  let wordAct:WORD =  readOnce setupClient ".wordVar"
  let dwordAct:DWORD = readOnce setupClient  ".dwordVar"
  let sintAct:SINT =  readOnce setupClient ".sintVar"
  let intAct:INT =   readOnce setupClient ".intVar"
  let dintAct:DINT =  readOnce setupClient ".dintVar"
  let realAct:REAL =  readOnce setupClient ".realVar"
  let lrealAct:LREAL = readOnce setupClient  ".lrealVar"

  Assert.AreEqual(boolExp, boolAct)
  Assert.AreEqual(byteExp, byteAct)
  Assert.AreEqual(wordExp, wordAct)
  Assert.AreEqual(dwordExp, dwordAct)
  Assert.AreEqual(sintExp, sintAct)
  Assert.AreEqual(intExp, intAct)
  Assert.AreEqual(dintExp, dintAct)
  Assert.AreEqual(realExp, realAct)
  Assert.AreEqual(lrealExp, lrealAct)

[<Test>]
let ``Accessing an array in the PLC - read`` () =
  let setupClient = new TcAdsClient()
  
  setupClient.Connect("192.168.68.132.1.1", 801)
  let plc = createClient "192.168.68.132.1.1" 801
  let intPlcExp = fixture.CreateMany<INT>(100).ToArray()

  writeArrOnce setupClient ".PLCVar" intPlcExp
  
  plc { readAny ".PLCVar" }
  |> function
    | Choice2Of3 err -> Assert.Fail err
    | Choice3Of3 (code,err) -> sprintf "%s with code %A" err code |> Assert.Fail
    | Choice1Of3 (arr: INT array) ->
      Assert.AreEqual(intPlcExp.Length, arr.Length, "arrays have different lengths")
      arr
      |> Seq.zip intPlcExp
      |> Seq.iter Assert.AreEqual

[<Test>]
let ``Accessing an array in the PLC - write`` () =
  //Set ADS in Start
  let setupClient = new TcAdsClient()
  
  setupClient.Connect("192.168.68.132.1.1", 801)
  let plc = createClient "192.168.68.132.1.1" 801
  let intPlcExp = fixture.CreateMany<INT>(100).ToArray()

  writeArrOnce setupClient ".PLCVar" intPlcExp
  
  plc { writeAny ".PLCVar" intPlcExp } |> Result.isOk |> Assert.IsTrue
  readArrOnce<INT> setupClient ".PLCVar" 100
  |> Seq.zip intPlcExp
  |> Seq.iter Assert.AreEqual

[<Test>]
let ``Reading and writing of string variables - read`` () = 

  let setupClient = new TcAdsClient()
  setupClient.Connect("192.168.68.132.1.1", 801)
  let plc = createClient "192.168.68.132.1.1" 801
  
  let inline writeStr len symName  = writeStringOnce setupClient symName len
  
  let stringExp = fixture.Create<string>()
  let string1Exp = fixture.Create<string>() |> Seq.head |> string 

  
  writeStr 80 ".string80Var" stringExp 
  writeStr 1 ".string1Var" string1Exp 

  
  plc { readAny ".string80Var"} |> assertEqual stringExp
  plc { readAny ".string1Var"} |> assertEqual string1Exp
  
[<Test>]
let ``Reading and writing of string variables - write`` () = 
  //Set ADS in Start
  let setupClient = new TcAdsClient()
  
  setupClient.Connect("192.168.68.132.1.1", 801)
  let plc = createClient "192.168.68.132.1.1" 801

  
  let stringExp = fixture.Create<string>()
  let string1Exp = fixture.Create<string>()
    
  
  plc { writeAny ".string80Var" stringExp } |> Result.isOk |> Assert.IsTrue
  let stringAct = readStrOnce setupClient  ".string80Var" 80
  Assert.AreEqual(stringExp, stringAct)

  plc { writeAny ".string1Var" string1Exp } |> Result.isAdsNok |> Assert.IsTrue

[<Test>]
let ``Reading and writing of date and time variables - read`` () = 

  let setupClient = new TcAdsClient()
  setupClient.Connect("192.168.68.132.1.1", 801)
  let plc = createClient "192.168.68.132.1.1" 801
  
  
  let timeExp = fixture.Create<TimeSpan>()
  let todExp = fixture.Create<TimeSpan>()
  let dateTimeExp = fixture.Create<DateTime>()
  let dateExp = fixture.Create<DateTime>().Date
  
  writeTSOnce setupClient ".timeVar" timeExp
  writeTSOnce setupClient ".todVar" todExp
  writeDTOnce setupClient ".dateVar" dateExp
  writeDTOnce setupClient ".dtVar" dateTimeExp

  
  plc { readAny ".timeVar" } |> assertEqual timeExp
  plc { readAny ".todVar" } |> assertEqual todExp
  plc { readAny ".dateVar" } |> assertEqual dateExp
  plc { readAny ".dtVar" } |> assertEqual dateTimeExp
  
[<Test>]
let ``Reading and writing of date and time variables - write`` () = 
  //Set ADS in Start
  let setupClient = new TcAdsClient()
  
  setupClient.Connect("192.168.68.132.1.1", 801)
  let plc = createClient "192.168.68.132.1.1" 801
  
  
  let timeExp = fixture.Create<TimeSpan>()
  let todExp = fixture.Create<TimeSpan>()
  let dateTimeExp = fixture.Create<DateTime>()
  let dateExp = fixture.Create<DateTime>().Date

  plc { writeAny ".timeVar" timeExp}   |> Result.isOk |> Assert.IsTrue
  plc { writeAny ".todVar" todExp}     |> Result.isOk |> Assert.IsTrue
  plc { writeAny ".dateVar" dateExp}   |> Result.isOk |> Assert.IsTrue
  plc { writeAny ".dtVar" dateTimeExp} |> Result.isOk |> Assert.IsTrue
  
  
  let timeAct = readTSOnce setupClient ".timeVar" 
  let todAct = readTSOnce setupClient ".todVar" 
  let dateTimeAct=readDTOnce setupClient ".dtVar" 
  let dateAct = readDTOnce setupClient ".dateVar" 

  Assert.AreEqual(timeExp, timeAct)
  Assert.AreEqual(todExp, todAct)
  Assert.AreEqual(dateTimeExp, dateTimeAct)
  Assert.AreEqual(dateExp, dateAct)

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

  write  ".arrboolVar"  boolExp 
  write  ".arrbyteVar"  byteExp 
  write  ".arrwordVar"  wordExp 
  write  ".arrdwordVar" dwordExp
  write  ".arrsintVar"  sintExp 
  write  ".arrintVar"   intExp 
  write  ".arrdintVar"  dintExp 
  write  ".arrrealVar"  realExp 
  write  ".arrlrealVar" lrealExp
  writeStrArrOnce setupClient ".arrstringVar" 9 80 stringExp 
  writeTSArrOnce setupClient  ".arrtimeVar" timeExp
  writeTSArrOnce setupClient  ".arrtodVar" todExp
  writeDTArrOnce setupClient  ".arrdateVar" dateExp
  writeDTArrOnce setupClient  ".arrdtVar" dtExp

  plc { readAny ".arrboolVar" }    |> assertArraysEqual boolExp
  plc { readAny ".arrbyteVar" }    |> assertArraysEqual byteExp
  plc { readAny ".arrwordVar" }    |> assertArraysEqual wordExp
  plc { readAny ".arrdwordVar"}    |> assertArraysEqual dwordExp
  plc { readAny ".arrsintVar" }    |> assertArraysEqual sintExp
  plc { readAny ".arrintVar"  }    |> assertArraysEqual intExp
  plc { readAny ".arrdintVar" }    |> assertArraysEqual dintExp
  plc { readAny ".arrrealVar" }    |> assertArraysEqual realExp
  plc { readAny ".arrlrealVar"}    |> assertArraysEqual lrealExp
  plc { readAny ".arrstringVar"}    |> assertArraysEqual stringExp
  plc { readAny ".arrtimeVar"}    |> assertArraysEqual timeExp
  plc { readAny ".arrtodVar"}    |> assertArraysEqual todExp
  plc { readAny ".arrdateVar"}    |> assertArraysEqual dateExp
  plc { readAny ".arrdtVar"}    |> assertArraysEqual dtExp
  
(*
  Do not test for LINT and LWORD in TC2 as those types are not supported
*)
(*
[<Test>]
let ``primitive type arrays write`` () = 

  //Set ADS in Start
  let setupClient = new TcAdsClient()
  
  setupClient.Connect("192.168.68.132.1.1", 801)
  let plc = createClient "192.168.68.132.1.1" 801

  let boolExp = fixture.Create<BOOL>()
  let byteExp = fixture.Create<BYTE>()
  let wordExp = fixture.Create<WORD>()
  let dwordExp = fixture.Create<DWORD>()
  
  let sintExp = fixture.Create<SINT>()
  let intExp = fixture.Create<INT>()
  let dintExp = fixture.Create<DINT>()
  let realExp = fixture.Create<REAL>()
  let lrealExp = fixture.Create<LREAL>()
    
  plc { writeAny ".boolVar" boolExp }   |> Result.isOk |> Assert.IsTrue
  plc { writeAny ".byteVar" byteExp }   |> Result.isOk |> Assert.IsTrue
  plc { writeAny ".wordVar" wordExp }   |> Result.isOk |> Assert.IsTrue
  plc { writeAny ".dwordVar" dwordExp } |> Result.isOk |> Assert.IsTrue
  plc { writeAny ".sintVar" sintExp }   |> Result.isOk |> Assert.IsTrue
  plc { writeAny ".intVar" intExp }     |> Result.isOk |> Assert.IsTrue
  plc { writeAny ".dintVar" dintExp }   |> Result.isOk |> Assert.IsTrue
  plc { writeAny ".realVar" realExp }   |> Result.isOk |> Assert.IsTrue
  plc { writeAny ".lrealVar" lrealExp } |> Result.isOk |> Assert.IsTrue

  let boolAct:BOOL =  readOnce setupClient ".boolVar"
  let byteAct:BYTE =  readOnce setupClient ".byteVar"
  let wordAct:WORD =  readOnce setupClient ".wordVar"
  let dwordAct:DWORD = readOnce setupClient  ".dwordVar"
  let sintAct:SINT =  readOnce setupClient ".sintVar"
  let intAct:INT =   readOnce setupClient ".intVar"
  let dintAct:DINT =  readOnce setupClient ".dintVar"
  let realAct:REAL =  readOnce setupClient ".realVar"
  let lrealAct:LREAL = readOnce setupClient  ".lrealVar"

  Assert.AreEqual(boolExp, boolAct)
  Assert.AreEqual(byteExp, byteAct)
  Assert.AreEqual(wordExp, wordAct)
  Assert.AreEqual(dwordExp, dwordAct)
  Assert.AreEqual(sintExp, sintAct)
  Assert.AreEqual(intExp, intAct)
  Assert.AreEqual(dintExp, dintAct)
  Assert.AreEqual(realExp, realAct)
  Assert.AreEqual(lrealExp, lrealAct) *)