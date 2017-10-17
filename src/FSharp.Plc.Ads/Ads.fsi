namespace FSharp.Plc.Ads.Experimental

[<AutoOpen>]
module Builder = 
  open TwinCAT.Ads
  open System.Collections.Generic

  type Result<'T> = Choice<'T,string, AdsErrorCode*string>

  module Result =
    val isOk: Result<'T> -> bool
    val isNok: Result<'T> -> bool
    val isAdsNok: Result<'T> -> bool
    val value: Result<'T> -> 'T
    val error: Result<'T> -> string
    val adsCode: Result<'T> -> AdsErrorCode
    val adsError: Result<'T> -> string

  [<Sealed>]
  type AdsWrapper = 
    member Yield: 'a -> unit

    [<CustomOperation("readAny")>] 
    member ReadAny: obj * string -> Result<'T>

    [<CustomOperation("readMany")>] 
    member ReadMany: obj * obj -> Result<'T>

    [<CustomOperation("writeAny")>] 
    member WriteAny: obj * string * 'T -> Result<unit>

    [<CustomOperation("writeMany")>] 
    member WriteMany: obj * (string * obj) seq -> Result<unit>

  val createClient: string -> int -> AdsWrapper
  //{
  //  //Client: TcAdsClient
  //  //Symbols: IDictionary<string,int*int64*int64*int>
  //  //SymbolLoader: TcAdsSymbolInfoLoader
  //}
