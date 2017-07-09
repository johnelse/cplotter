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

module Output = struct
  let set_message_js js_message =
    match Html.getElementById_coerce "message" Html.CoerceTo.div with
    | Some div -> begin
      div##.innerHTML := js_message
    end
    | None -> ()

  let set_message message = set_message_js (Js.string message)
end

module Data = struct
  let get url =
    let open Promise.Infix in
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

    >>= (fun responseText ->
      try
        Promise.resolve
          (CryptoCompare.response_of_string (Js.to_string responseText))
      with e ->
        Promise.reject
          (new%js Js.error_constr (Printexc.to_string e |> Js.string)))
end

module Drawing = struct
  type context = {
    ctx:    Html.canvasRenderingContext2D Js.t;
    width:  int;
    height: int;
  }

  let get_canvas () =
    Html.getElementById_coerce "graph" Html.CoerceTo.canvas

  let with_context paint =
    match get_canvas () with
    | Some canvas -> begin
      let context = {
        ctx    = canvas##getContext Html._2d_;
        width  = canvas##.width;
        height = canvas##.height;
      } in
      paint context
    end
    | None -> ()

  let fill_background {ctx; width; height} =
    ctx##.fillStyle := Js.string "#CCCCCC";
    ctx##fillRect 0. 0. (float_of_int width) (float_of_int height)

  let render_data {ctx; width; height} response =
    fill_background {ctx; width; height}
end

let button_onclick () =
  let open Input in
  match
    get_currency_first (), get_currency_second (), get_frequency (), get_limit ()
  with
  | Some currency_from, Some currency_to, Some frequency, Some limit -> begin
    Output.set_message "loading...";
    let open Promise.Infix in
    let url =
      CryptoCompare.make_url
        ~first:currency_from
        ~second:currency_to
        ~frequency
        ~limit
    in
    Data.get url
    >|| (
      (fun response ->
        Output.set_message "done";
        let open CryptoCompare in
        Drawing.(with_context (fun context -> render_data context response))),
      (fun error ->
        Output.set_message_js (error##toString))
    )
  end
  | _ -> ()

let window_onload () =
  Drawing.(with_context fill_background);
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
