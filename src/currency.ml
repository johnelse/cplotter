type currency = [ `BTC | `ETH | `USD | `GBP ]

let to_string = function
  | `BTC -> "BTC"
  | `ETH -> "ETH"
  | `USD -> "USD"
  | `GBP -> "GBP"

let of_string = function
  | "BTC" -> Some `BTC
  | "ETH" -> Some `ETH
  | "USD" -> Some `USD
  | "GBP" -> Some `GBP
  | _ -> None
