module RecordTests

open FSharp.Plc.Ads.Experimental
open NUnit.Framework
open TwinCAT.Ads
open System
open System.Linq
open AutoFixture
open System.Threading
open Tests

[<Attributes.NonLinear>]
type SimpleTypesRecord = {
  [<property:Attributes.Part(".boolVar")>]
  bool: BOOL
  [<property:Attributes.Part(".byteVar")>]
  byte:BYTE
  [<property:Attributes.Part(".wordVar")>]
  word:WORD
  [<property:Attributes.Part(".dwordVar")>]
  dword:DWORD
  
  [<property:Attributes.Part(".sintVar")>]
  sint:SINT
  [<property:Attributes.Part(".intVar")>]
  int:INT
  [<property:Attributes.Part(".dintVar")>]
  dint:DINT
  
  [<property:Attributes.Part(".realVar")>]
  real:REAL
  [<property:Attributes.Part(".lrealVar")>]
  lreal:LREAL
  [<property:Attributes.Part(".string80Var")>]
  string80:string
  [<property:Attributes.Part(".string1Var")>]
  string1:string
  [<property:Attributes.Part(".string256Var")>]
  string256:string
  [<property:Attributes.Part(".timeVar")>]
  time:TimeSpan
  [<property:Attributes.Part(".todVar")>]
  tod:TimeSpan
  [<property:Attributes.Part(".dateVar")>]
  date:DateTime
  [<property:Attributes.Part(".dtVar")>]
  dt:DateTime
}


let fixture = new Fixture()
let spareFixture = new Fixture()



[<SetUp>]
let AutoFixtureSetup () =
  fixture.Register<TimeSpan>(fun _ -> spareFixture.Create<uint32>() |> float |> TimeSpan.FromMilliseconds)
  fixture.Register<DateTime>(fun _ -> spareFixture.Create<DateTime>().Date + (spareFixture.Create<uint32>() * 1000u |> float |> TimeSpan.FromMilliseconds))

[<Test>]
let ``read simple types record`` () =

  
  let setupClient = new TcAdsClient()
  setupClient.Connect("192.168.119.134.1.1", 801)
  let plc = createClient "192.168.119.134.1.1" 801 false
  let inline write symName  = writeOnce setupClient symName 
  let inline writeStr len symName  = writeStringOnce setupClient symName len
  
  let boolExp = fixture.Create<BOOL>()
  let byteExp = fixture.Create<BYTE>()
  let wordExp = fixture.Create<WORD>()
  let dwordExp = fixture.Create<DWORD>()
  let sintExp = fixture.Create<SINT>()
  let intExp = fixture.Create<INT>()
  let dintExp = fixture.Create<DINT>()
  let realExp = fixture.Create<REAL>()
  let lrealExp = fixture.Create<LREAL>()
  let stringExp = fixture.Create<string>() |> Seq.truncate 80 |> Seq.map string |> Seq.fold (+) ""
  let string1Exp = fixture.Create<string>() |> Seq.head |> string 
  let string256Exp = fixture.Create<string>() |> Seq.truncate 256 |> Seq.map string |> Seq.fold (+) ""
  
  let timeExp = fixture.Create<TimeSpan>()
  let todExp = fixture.Create<TimeSpan>()
  let dateTimeExp = fixture.Create<DateTime>()
  let dateExp = fixture.Create<DateTime>().Date
  
  write  ".boolVar"  boolExp 
  write  ".byteVar"  byteExp 
  write  ".wordVar"  wordExp 
  write  ".dwordVar" dwordExp
  write  ".sintVar"  sintExp 
  write  ".intVar"   intExp 
  write  ".dintVar"  dintExp 
  write  ".realVar"  realExp 
  write  ".lrealVar" lrealExp
  
  writeStr 256 ".string256Var" string256Exp 
  writeStr 80 ".string80Var" stringExp 
  writeStr 1 ".string1Var" string1Exp 
  
  writeTSOnce setupClient ".timeVar" timeExp
  writeTSOnce setupClient ".todVar" todExp
  writeDTOnce setupClient ".dateVar" dateExp
  writeDTOnce setupClient ".dtVar" dateTimeExp

  let res = plc { readAny "" }
  match res with
  | Choice1Of3 res ->
    Assert.AreEqual(boolExp ,res.bool)
    Assert.AreEqual(byteExp ,res.byte)
    Assert.AreEqual(wordExp ,res.word)
    Assert.AreEqual(dwordExp,res.dword)
    Assert.AreEqual(sintExp ,res.sint)
    Assert.AreEqual(intExp  ,res.int)
    Assert.AreEqual(dintExp ,res.dint)
    Assert.AreEqual(realExp ,res.real)
    Assert.AreEqual(lrealExp,res.lreal)
    Assert.AreEqual(string256Exp,res.string256,"string 256")
    Assert.AreEqual(stringExp,res.string80,"string 80")
    Assert.AreEqual(string1Exp,res.string1,"string 1")
    Assert.AreEqual(timeExp,res.time)
    Assert.AreEqual(todExp,res.tod)
    Assert.AreEqual(dateExp,res.date)
    Assert.AreEqual(dateTimeExp,res.dt)
  | Choice2Of3 err -> Assert.Fail err
  | Choice3Of3 (err,str) -> Assert.Fail(sprintf "%A: %s" err str)


[<Test>]
let ``write simple types record`` () =

  
  let setupClient = new TcAdsClient()
  setupClient.Connect("192.168.119.134.1.1", 801)
  let plc = createClient "192.168.119.134.1.1" 801 false
  
  let boolExp = fixture.Create<BOOL>()
  let byteExp = fixture.Create<BYTE>()
  let wordExp = fixture.Create<WORD>()
  let dwordExp = fixture.Create<DWORD>()
  let sintExp = fixture.Create<SINT>()
  let intExp = fixture.Create<INT>()
  let dintExp = fixture.Create<DINT>()
  let realExp = fixture.Create<REAL>()
  let lrealExp = fixture.Create<LREAL>()
  let stringExp = fixture.Create<string>() |> Seq.truncate 80 |> Seq.map string |> Seq.fold (+) ""
  let string1Exp = fixture.Create<string>() |> Seq.head |> string 
  let string256Exp = fixture.Create<string>() |> Seq.truncate 256 |> Seq.map string |> Seq.fold (+) ""
  
  let timeExp = fixture.Create<TimeSpan>()
  let todExp = fixture.Create<TimeSpan>()
  let dateTimeExp = fixture.Create<DateTime>()
  let dateExp = fixture.Create<DateTime>().Date
  let r = {
    bool      =boolExp 
    byte      =byteExp 
    word      =wordExp 
    dword     =dwordExp 
    sint      =sintExp 
    int       =intExp  
    dint      =dintExp 
    real      =realExp 
    lreal     =lrealExp 
    string256 =string256Exp
    string80  =stringExp
    string1   =string1Exp
    time=timeExp
    tod=todExp
    date=dateExp
    dt=dateTimeExp
  }

  plc { writeAny "" r } 
  |> function 
  | Choice1Of3 _ -> ()
  | Choice2Of3 err -> Assert.Fail(err)
  | Choice3Of3 (code,err) -> Assert.Fail(sprintf "%A: %s" code err)
  
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

  let stringAct = readStrOnce setupClient  ".string80Var" 80
  let string1Act = readStrOnce setupClient  ".string1Var" 1
  let string256Act = readStrOnce setupClient  ".string256Var" 256
  Assert.AreEqual(stringExp, stringAct,"string 80")
  Assert.AreEqual(string1Exp, string1Act,"string 1")
  Assert.AreEqual(string256Exp, string256Act,"string 256")

  let timeAct = readTSOnce setupClient ".timeVar" 
  let todAct = readTSOnce setupClient ".todVar" 
  let dateTimeAct=readDTOnce setupClient ".dtVar" 
  let dateAct = readDTOnce setupClient ".dateVar" 
  
  Assert.AreEqual(timeExp, timeAct)
  Assert.AreEqual(todExp, todAct)
  Assert.AreEqual(dateTimeExp, dateTimeAct)
  Assert.AreEqual(dateExp, dateAct)
  