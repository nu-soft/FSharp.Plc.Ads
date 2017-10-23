(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Accessing an array in the PLC
========================
Sample based on [Beckhoff Infosys - Sample 01](https://infosys.beckhoff.com/english.php?content=../content/1033/tc3_adssamples_net/html/twincat.ads.sample01.html)

**Task**

The PLC contains an array that is to be read by the .Net application using a read command.

**Description**

The PLC contains an array of 100 elements of type integer (2 bytes). The array in the PLC is to be filled with the values from 3500 to 3599.

**PLC program**
```pascal
PROGRAM MAIN
VAR
  PLCVar : ARRAY [0..99] OF INT;
  Index: BYTE;
END_VAR
FOR Index := 0 TO 99 DO
  PLCVar[Index] := 3500 + INDEX;
END_FOR
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

let myInputArray = [| 3500s .. 3599s |]

plc {
  writeAny "MAIN.PLCVar" myInputArray
}
|> function 
  | Choice1Of3 _ -> ()// handle success
  | Choice2Of3 errMsg -> ()// handle error
  | Choice3Of3 (adsCode,additionalInfo) -> ()// handle ADS error

plc {
  readAny "MAIN.PLCVar"
}
|> function 
  | Choice1Of3 (readArray: INT array) -> ()// handle success
  | Choice2Of3 errMsg -> ()// handle error
  | Choice3Of3 (adsCode,additionalInfo) -> ()// handle ADS error


