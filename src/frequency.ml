type frequency = [ `histominute | `histohour | `histoday ]

let to_string = function
  | `histominute -> "histominute"
  | `histohour   -> "histohour"
  | `histoday    -> "histoday"

let of_string = function
  | "histominute" -> Some `histominute
  | "histohour"   -> Some `histohour
  | "histoday"    -> Some `histoday
  | _ -> None
