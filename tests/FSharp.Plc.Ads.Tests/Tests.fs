module Tests

open FSharp.Plc.Ads.Experimental
open NUnit.Framework
open Ploeh.AutoFixture
open TwinCAT.Ads
open System

let writeOnce<'T when 'T : struct> (client:TcAdsClient) symName (value: 'T) =
  let handle = client.CreateVariableHandle symName
  client.WriteAny(handle, value)
  client.DeleteVariableHandle handle

let readOnce<'T when 'T : struct> (client:TcAdsClient) symName  =
  let handle = client.CreateVariableHandle symName
  let res = client.ReadAny(handle, typeof<'T>) :?> 'T
  client.DeleteVariableHandle handle
  res

let assertEqual (expected:'T) (act:Result<'T>) = 
  match act with
    | Choice1Of3 a -> Assert.AreEqual(expected,a)
    | Choice2Of3 err -> Assert.Fail err
    | Choice3Of3 (code,err) -> sprintf "%A: %s" code err |> Assert.Fail
  

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

  let fixture = new Fixture()

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

  plc { readAny ".boolVar" } |> assertEqual boolExp
  plc { readAny ".byteVar" } |> assertEqual byteExp
  plc { readAny ".wordVar" } |> assertEqual wordExp
  plc { readAny ".dwordVar"} |> assertEqual dwordExp
  plc { readAny ".sintVar" } |> assertEqual sintExp
  plc { readAny ".intVar"  } |> assertEqual intExp
  plc { readAny ".dintVar" } |> assertEqual dintExp
  plc { readAny ".realVar" } |> assertEqual realExp
  plc { readAny ".lrealVar"} |> assertEqual lrealExp
  
(*
  Do not test for LINT and LWORD in TC2 as those types are not supported
*)
[<Test>]
let ``primitive types write`` () = 

  let fixture = new Fixture()
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