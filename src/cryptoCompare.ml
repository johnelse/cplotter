let make_url ~first ~second ~frequency ~limit =
  Printf.sprintf "https://min-api.cryptocompare.com/data/%s?fsym=%s&tsym=%s&limit=%d&aggregate=3&e=CCCAGG"
    (Frequency.to_string frequency)
    (Currency.to_string first)
    (Currency.to_string second)
    (max 1 limit)
