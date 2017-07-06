module Html = Dom_html

module Input = struct
  let get_select_value id of_string =
    match Html.getElementById_coerce id Html.CoerceTo.select with
    | Some select -> of_string (Js.to_string select##.value)
    | None -> None

  let get_currency_first () =
    get_select_value "currencyfirst" Currency.of_string

  let get_currency_second () =
    get_select_value "currencysecond" Currency.of_string

  let get_frequency () =
    get_select_value "frequency" Frequency.of_string

  let get_limit () =
    match Html.getElementById_coerce "limit" Html.CoerceTo.input with
    | Some input -> begin
      try Some (input##.value |> Js.to_string |> int_of_string)
      with Failure _ -> None
    end
    | None -> None
end

module Data = struct
  let get url =
    Promise.make
      (fun resolve reject ->
        let request = XmlHttpRequest.create () in
        request##_open (Js.string "GET") (Js.string url) Js._true;

        request##.onreadystatechange :=
          Js.wrap_callback (fun () ->
            if request##.readyState = XmlHttpRequest.DONE
            then begin
              if request##.status = 200
              then resolve (request##.responseText)
              else reject (new%js Js.error_constr request##.statusText)
            end);

        request##send Js.null)
end

module Drawing = struct
  let get_canvas () =
    Html.getElementById_coerce "graph" Html.CoerceTo.canvas
end

let button_onclick () =
  let open Input in
  match
    get_currency_first (), get_currency_second (), get_frequency (), get_limit ()
  with
  | Some currency_from, Some currency_to, Some frequency, Some limit -> begin
    let url =
      CryptoCompare.make_url
        ~first:currency_from
        ~second:currency_to
        ~frequency
        ~limit
    in
    Html.window##alert (Js.string url)
  end
  | _ -> ()

let window_onload () =
  match Html.getElementById_coerce "load" Html.CoerceTo.button with
  | Some button -> begin
    button##.onclick := Html.handler
      (fun _ ->
        let () = button_onclick () in
        Js._false)
  end
  | None -> ()

let () =
  Html.window##.onload := Html.handler
    (fun _ ->
      let () = window_onload () in
      Js._false)
