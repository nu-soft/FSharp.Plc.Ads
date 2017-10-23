(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Reading and writing of string variables
========================
Sample based on [Beckhoff Infosys - Sample 04](https://infosys.beckhoff.com/english.php?content=../content/1033/tc3_adssamples_net/html/twincat.ads.sample04.htm)

**Task**

A .Net application should read a string from the PLC and write a string to the PLC.

**Description**

The PLC contains the string MAIN.text.

**PLC program**
```pascal
PROGRAM MAIN
VAR
 text : STRING[30] := 'hello';
END_VAR
```

**F# program**
*)
#r "FSharp.Plc.Ads.dll"
//using IEC 61131-3 type aliases for convenience
#r "NuSoft.FSharp.IEC.DataTypes.dll"
open FSharp.Plc.Ads.Experimental
open System
let amsNetId = "192.168.68.132.1.1"
let amsPort = 801
//instantiate builder for specific TwinCAT instance
let plc = createClient amsNetId amsPort

let myInput = "hello PLC world!!!"

plc {
  writeAny "MAIN.text" myInput
}
|> function 
  | Choice1Of3 _ -> ()// handle success
  | Choice2Of3 errMsg -> ()// handle error
  | Choice3Of3 (adsCode,additionalInfo) -> ()// handle ADS error

plc {
  readAny "MAIN.text"
}
|> function 
  | Choice1Of3 (res: string) -> ()// handle success
  | Choice2Of3 errMsg -> ()// handle error
  | Choice3Of3 (adsCode,additionalInfo) -> ()// handle ADS error


