type frequency = [ `histominute | `histohour | `histoday ]

val to_string : frequency -> string

val of_string : string -> frequency option

