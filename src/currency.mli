type currency = [ `BTC | `ETH | `XMR | `USD | `GBP ]

val to_string : currency -> string

val of_string : string -> currency option
