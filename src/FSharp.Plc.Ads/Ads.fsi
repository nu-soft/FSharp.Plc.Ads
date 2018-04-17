namespace FSharp.Plc.Ads.Experimental

[<AutoOpen>]
module Builder = 
  open TwinCAT.Ads
  open System
  open System.Collections.Generic
  
  /// type alias for `Choice<'T, string, AdsError * string>`
  ///
  /// *`Choice1Of3` is OK [Result]*
  ///
  /// *`Choice2Of3` is NOK [Result]*
  ///
  /// *`Choice3Of3` is ADS NOK [Result]*
  ///
  /// `'T` is type of successful return
  type Result<'T> = Choice<'T,string, AdsErrorCode * string> 

  module Result =
    /// **returns** true if result is `OK`
    ///
    /// `'T` is type of successful return
    val isOk: Result<'T> -> bool

    /// **returns** true if result is NOK
    ///
    /// `'T` is type of successful return
    val isNok: Result<'T> -> bool

    /// **returns** true if result is ADS NOK
    ///
    /// `'T` is type of successful return
    val isAdsNok: Result<'T> -> bool

    /// **returns** value of OK
    ///
    /// `'T` is type of successful return
    ///
    /// **throws** `System.InvalidOperationException` when value is not OK
    val value: Result<'T> -> 'T

    /// **returns** error description of NOK
    ///
    /// `'T` is type of successful return
    ///
    /// **throws** `System.InvalidOperationException` when value is not NOK
    val error: Result<'T> -> string
    
    /// **returns** ADS error code of ADS NOK
    ///
    /// `'T` is type of successful return
    ///
    /// **throws** `System.InvalidOperationException` when value is not ADS NOK
    ///
    /// **returns** [TwinCAT.Ads.AdsErrorCode](https://infosys.beckhoff.com/english.php?content=../content/1033/tc3_adsnetref/4017429771.html&id= "Beckhoff InfoSys")
    val adsCode: Result<'T> -> AdsErrorCode
    
    /// **returns** additional information for ADS NOK
    ///
    /// `'T` is type of successful return
    ///
    /// **throws** `System.InvalidOperationException` when value is not ADS NOK
    val adsError: Result<'T> -> string

  [<Sealed>]
  /// Wraps in [TwinCAT.Ads.TcAdsClient](https://infosys.beckhoff.com/english.php?content=../content/1033/tc3_adsnetref/4017828107.html&id= "Beckhoff InfoSys") in F# computation expression
  type AdsWrapper = 
    /// A method used to support the F# query syntax.
    member Yield: 'a -> unit

    /// A method used to read single value of any type
    ///
    /// `'T` is type of successful return
    ///
    /// **returns** [Result]
    [<CustomOperation("readAny")>] 
    member ReadAny: obj * string -> Result<'T>

    /// A method used to read many non-linear values
    ///
    /// **T is expected to be a F# tuple**
    ///
    /// `'T` is type of successful return
    ///
    /// **returns** [Result]
    [<CustomOperation("readMany")>] 
    member ReadMany: obj * obj -> Result<'T>
    
    /// A method used to write single value of any type
    ///
    /// `'T` is write value type
    /// **returns** [Result]
    [<CustomOperation("writeAny")>] 
    member WriteAny: obj * string * 'T -> Result<unit>
    
    /// A method used to write many non-linear values
    ///
    /// `'T` is write value type
    ///
    /// **returns** [Result]
    [<CustomOperation("writeMany")>] 
    member WriteMany: obj * (string * obj) seq -> Result<unit>
    
    /// A method used to register observer on variable
    ///
    /// `'T` is observed value type
    ///
    /// **returns** [Result]
    [<CustomOperation("observe")>] 
    member Observe: obj * string * int * int -> Result<IObservable<'T>>

    [<CustomOperation("observeStatus")>] 
    member ObserveStatus: obj -> Result<IEvent<AdsStateChangedEventHandler, AdsStateChangedEventArgs>>
    
    [<CustomOperation("readState")>] 
    member ReadState: obj -> Result<StateInfo>
    
    [<CustomOperation("removeHandles")>] 
    member RemoveHandles: obj -> unit

  /// Creates instance of AdsWrapper
  ///
  /// **amsNetId**: AmsNetId of PLC
  ///
  /// **port**: target port of PLC
  ///
  /// **returns** [AdsWrapper]
  val createClient: string -> int -> bool -> AdsWrapper
