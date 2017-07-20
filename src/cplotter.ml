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
  let set_div_innerHTML id innerHTML =
    match Html.getElementById_coerce id Html.CoerceTo.div with
    | Some div -> begin
      div##.innerHTML := innerHTML
    end
    | None -> ()

  let set_message message = set_div_innerHTML "message" message

  let set_average_high value =
    set_div_innerHTML "averagehigh"
      (Printf.sprintf "Average high: %f" value |> Js.string)

  let set_average_low value =
    set_div_innerHTML "averagelow"
      (Printf.sprintf "Average low: %f" value |> Js.string)

  let set_overall_high value =
    set_div_innerHTML "overallhigh"
      (Printf.sprintf "Overall high: %f" value |> Js.string)

  let set_overall_low value =
    set_div_innerHTML "overalllow"
      (Printf.sprintf "Overall low: %f" value |> Js.string)
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

  type summary = {
    count:        int;
    average_high: float;
    average_low:  float;
    overall_high: float;
    overall_low:  float;
    time_from:    int;
    time_to:      int;
  }

  let summarise response =
    let open CryptoCompare in
    let count, sum_high, sum_low, overall_high, overall_low =
      List.fold_left
        (fun (count, sum_high, sum_low, overall_high, overall_low) data_point ->
          count + 1,
          sum_high +. data_point.high,
          sum_low +. data_point.low,
          max overall_high data_point.high,
          min overall_low data_point.low
        )
        (0, 0., 0., 0., max_float)
        response.data
    in
    let time_from, time_to =
      if count = 0
      then response.time_from, response.time_to
      else
        (List.nth response.data 0).time,
        (List.nth response.data (count - 1)).time
    in
    {
      count;
      average_high = sum_high /. (float_of_int count);
      average_low  = sum_low  /. (float_of_int count);
      overall_high;
      overall_low;
      time_from;
      time_to;
    }
end

module Drawing = struct
  type context = {
    ctx:    Html.canvasRenderingContext2D Js.t;
    width:  float;
    height: float;
  }

  let get_canvas () =
    Html.getElementById_coerce "graph" Html.CoerceTo.canvas

  let with_context paint =
    match get_canvas () with
    | Some canvas -> begin
      let context = {
        ctx    = canvas##getContext Html._2d_;
        width  = float_of_int canvas##.width;
        height = float_of_int canvas##.height;
      } in
      paint context
    end
    | None -> ()

  let black      = Js.string "#000000"
  let light_grey = Js.string "#CCCCCC"
  let dark_grey  = Js.string "#666666"
  let red        = Js.string "#FF0000"
  let green      = Js.string "#00FF00"

  let fill_background {ctx; width; height} =
    ctx##.fillStyle := light_grey;
    ctx##fillRect 0. 0. width height

  let choose_colour data_point =
    let open CryptoCompare in
    if data_point.close > data_point._open then green
    else if data_point.close < data_point._open then red
    else light_grey

  let x_of_time summary plot_width axis_gap time =
    let open Data in
    plot_width
    *. (float_of_int (time            - summary.time_from))
    /. (float_of_int (summary.time_to - summary.time_from))
    +. axis_gap

  let y_of_cost summary plot_height axis_gap cost =
    let pixels_per_unit_cost =
      plot_height /.
      (summary.Data.overall_high -. summary.Data.overall_low)
    in
    axis_gap +. plot_height -.
    (pixels_per_unit_cost *. (cost -. summary.Data.overall_low))

  let draw_candles {ctx; width; height}
      axis_gap plot_width plot_height summary data_points =
    let open CryptoCompare in
    let open Data in
    List.iter
      (fun data_point ->
        let colour = choose_colour data_point in
        ctx##.strokeStyle := colour;
        ctx##.fillStyle := colour;
        let x = x_of_time summary plot_width axis_gap data_point.time in
        let y_close = y_of_cost summary plot_height axis_gap data_point.close in
        let y_high  = y_of_cost summary plot_height axis_gap data_point.high in
        let y_low   = y_of_cost summary plot_height axis_gap data_point.low in
        let y_open  = y_of_cost summary plot_height axis_gap data_point._open in
        ctx##beginPath;
        ctx##moveTo x y_low;
        ctx##lineTo x y_high;
        ctx##stroke;
        let top = min y_open y_close in
        let candle_height = abs_float (y_open -. y_close) in
        let candle_width = 4.0 in
        ctx##fillRect (x -. (candle_width /. 2.)) top candle_width candle_height)
      data_points

  let draw_axes {ctx; width; height} axis_gap plot_width plot_height summary =
    (* Draw main lines of axes. *)
    ctx##.strokeStyle := black;
    ctx##beginPath;
    ctx##moveTo axis_gap                 axis_gap;
    ctx##lineTo axis_gap                 (plot_height +. axis_gap);
    ctx##lineTo (plot_width +. axis_gap) (plot_height +. axis_gap);
    ctx##stroke;
    (* Draw time scale. *)
    let open Data in
    if summary.count > 1 then begin
      let time_range = summary.time_to - summary.time_from in
      let time_step = time_range / (summary.count - 1) in
      let steps_per_tick = max 1 (time_range / time_step / 4) in
      let ticks = time_range / (steps_per_tick * time_step) + 1 in
      for tick = 0 to ticks - 1 do
        ctx##beginPath;
        let time = summary.time_from + tick * time_step * steps_per_tick in
        let x = x_of_time summary plot_width axis_gap time in
        ctx##moveTo x (plot_height +. axis_gap);
        ctx##lineTo x (plot_height +. axis_gap +. 5.0);
        ctx##stroke
      done
    end

  let render_data {ctx; width; height} summary data_points =
    let axis_gap    = 50.0 in
    let plot_width  = width -. 2.0 *. axis_gap in
    let plot_height = height -. 2.0 *. axis_gap in
    fill_background {ctx; width; height};
    draw_candles {ctx; width; height}
      axis_gap plot_width plot_height summary data_points;
    draw_axes {ctx; width; height} axis_gap plot_width plot_height summary
end

let button_onclick () =
  let open Input in
  match
    get_currency_first (), get_currency_second (), get_frequency (), get_limit ()
  with
  | Some currency_from, Some currency_to, Some frequency, Some limit -> begin
    Output.set_message (Js.string "loading...");
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
        Output.set_message (Js.string "done");
        let open CryptoCompare in
        let summary = Data.summarise response in
        Drawing.(with_context
          (fun context -> render_data context summary response.data));
        Output.set_average_high summary.Data.average_high;
        Output.set_average_low  summary.Data.average_low;
        Output.set_overall_high summary.Data.overall_high;
        Output.set_overall_low  summary.Data.overall_low;
      ),
      (fun error ->
        Output.set_message (error##toString))
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
