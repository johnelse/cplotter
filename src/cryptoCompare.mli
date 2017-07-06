val make_url :
  first:Currency.currency ->
  second:Currency.currency ->
  frequency:Frequency.frequency ->
  limit:int ->
  string

type data_point = {
  time:        int;
  close:       float;
  high:        float;
  low:         float;
  _open:       float;
  volume_from: float;
  volume_to:   float;
}

type conversion_type = {
  _type:             string;
  conversion_symbol: string;
}

type response_type =
  | Success
  | Error

type response = {
  response_type:   response_type;
  _type:           int;
  aggregated:      bool;
  data:            data_point list;
  time_to:         int;
  time_from:       int;
  first_value_int_array:
                   bool;
  conversion_type:
                   conversion_type;
}

val response_of_string : string -> response
