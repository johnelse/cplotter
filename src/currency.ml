type currency = [ `BTC | `ETH | `XMR | `USD | `GBP ]

let to_string = function
  | `BTC -> "BTC"
  | `ETH -> "ETH"
  | `XMR -> "XMR"
  | `USD -> "USD"
  | `GBP -> "GBP"

let of_string = function
  | "BTC" -> Some `BTC
  | "ETH" -> Some `ETH
  | "XMR" -> Some `XMR
  | "USD" -> Some `USD
  | "GBP" -> Some `GBP
  | _ -> None
