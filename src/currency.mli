type currency = [ `BTC | `ETH | `USD | `GBP ]

val to_string : currency -> string

val of_string : string -> currency option
