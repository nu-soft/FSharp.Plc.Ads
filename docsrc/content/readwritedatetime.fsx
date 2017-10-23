(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Reading and writing of TIME/DATE variables
========================
Sample based on [Beckhoff Infosys - Sample 05](https://infosys.beckhoff.com/english.php?content=../content/1033/tc3_adssamples_net/html/twincat.ads.sample05.htm)

**Task**

A .Net application should read and write a date and a time.

**Description**

The PLC contains the TIME variable MAIN.Time1 and the DT variable MAIN.Date1.

**PLC program**
```pascal
ROGRAM MAIN
VAR 
    Time1:TIME := T#21h33m23s231ms;
    Date1:DT:=DT#1993-06-12-15:36:55.40;
END_VAR
```

**F# program**
*)
#r "FSharp.Plc.Ads.dll"
open FSharp.Plc.Ads.Experimental
open System
let amsNetId = "192.168.68.132.1.1"
let amsPort = 801
//instantiate builder for specific TwinCAT instance
let plc = createClient amsNetId amsPort

let dt = DateTime.Now
let time = dt.TimeOfDay

plc {
  writeAny "MAIN.Time1" myInputArray
}
|> function 
  | Choice1Of3 _ -> 
  | Choice2Of3 errMsg -> ()// handle error
  | Choice3Of3 (adsCode,additionalInfo) -> ()// handle ADS error
 plc {
  writeAny "MAIN.Date1" myInputArray
}
|> function 
  | Choice1Of3 _ -> 
  | Choice2Of3 errMsg -> ()// handle error
  | Choice3Of3 (adsCode,additionalInfo) -> ()// handle ADS error

plc {
  readAny "MAIN.Time1"
}
|> function 
  | Choice1Of3 (time: TimeSpan) -> ()// handle success
  | Choice2Of3 errMsg -> ()// handle error
  | Choice3Of3 (adsCode,additionalInfo) -> ()// handle ADS error

plc {
  readAny "MAIN.Date1"
}
|> function 
  | Choice1Of3 (date: DateTime) -> ()// handle success
  | Choice2Of3 errMsg -> ()// handle error
  | Choice3Of3 (adsCode,additionalInfo) -> ()// handle ADS error


