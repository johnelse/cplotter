let make_url ~first ~second ~frequency ~limit =
  Printf.sprintf "https://min-api.cryptocompare.com/data/%s?fsym=%s&tsym=%s&limit=%d&aggregate=3&e=CCCAGG"
    (Frequency.to_string frequency)
    (Currency.to_string first)
    (Currency.to_string second)
    (max 1 limit)

type data_point = {
  time:        int;
  close:       float;
  high:        float;
  low:         float;
  _open:       float [@key "open"];
  volume_from: float [@key "volumefrom"];
  volume_to:   float [@key "volumeto"];
} [@@deriving rpc]

type conversion_type = {
  _type:             string [@key "type"];
  conversion_symbol: string [@key "conversionSymbol"];
} [@@deriving rpc]

type response_type =
  | Success
  | Error [@@deriving rpc]

type response = {
  response_type:   response_type   [@key "Response"];
  _type:           int             [@key "Type"];
  aggregated:      bool            [@key "Aggregated"];
  data:            data_point list [@key "Data"];
  time_to:         int             [@key "TimeTo"];
  time_from:       int             [@key "TimeFrom"];
  first_value_int_array:
                   bool            [@key "FirstValueInArray"];
  conversion_type:
                   conversion_type [@key "ConversionType"];
} [@@deriving rpc]

let response_of_string str =
  Jsonrpc.of_string str
  |> response_of_rpc
