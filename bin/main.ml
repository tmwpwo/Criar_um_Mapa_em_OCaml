
let html_to_string html = Fmt.str "%a" (Tyxml.Html.pp ()) html



let generate ?(who: string option = None) request= 
  let open Tyxml.Html in
  html  
  (head
  (title (txt "generate"))[])
    (body 
    [ h1 [ txt "calculator"];
      form ~a:[a_action "/calc"; a_method `Post] [
        Dream.csrf_tag request |> Unsafe.data;
        label ~a:[a_label_for "value_1"] [txt "First Value: "];
        input ~a:[a_input_type `Text; a_id "value_1"; a_name "value_1"] ();
        br();br();
        label ~a:[a_label_for "value_2"] [txt "Second Value: "];
        input ~a:[a_input_type `Text; a_id "value_2"; a_name "value_2"] ();
        br();br();
        label ~a:[a_label_for "result"] [txt "Result: "; txt (Option.value ~default:"" who)];
        br();br();
        input ~a:[a_input_type `Submit; a_value "   +  "; a_formaction "/add"] ();
        txt " ";
        input ~a:[a_input_type `Submit; a_value "   -  "; a_formaction "/sub"] ();
        txt " ";
        input ~a:[a_input_type `Submit; a_value "   *  "; a_formaction "/mult"] ();
        txt " ";
        input ~a:[a_input_type `Submit; a_value "   /  "; a_formaction "/div"] ();
      ] 
    ])

 


let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.memory_sessions
  @@ Dream.router [

    Dream.get "/"
      (fun _ ->
        Dream.html "Good morning, world!");

    Dream.get "/echo/:word"
      (fun request ->
        Dream.html (Dream.param request "word"));
    
    Dream.get "/calc"
      (fun request ->  
        match Dream.session_field request "user" with
      | None ->
        let%lwt () = Dream.invalidate_session request in
        let%lwt () = Dream.set_session_field request "user" "admin" in
        Dream.html @@ html_to_string (generate request)
      | Some _ ->
        Dream.html @@ html_to_string (generate request));

  
    Dream.post "/div" (fun request ->
      let find_data form_data key =
        match List.assoc key form_data with
        | data -> data
        | exception Not_found -> "Not provided"
      in
      let%lwt form_result = Dream.form request in
      match form_result with
      | `Ok form_data ->
        let val1 = find_data form_data "value_1" in
        let val2 = find_data form_data "value_2" in

        let str_to_int str =
          try Some (int_of_string str) with
          | Failure _ -> None
        in

        let int_to_str int =
          match int with
          | Some i -> Some (string_of_int i)
          | None -> None
        in

        let val1 =  str_to_int val1 in
        let val2 =  str_to_int val2 in
        
        let add_options opt1 opt2 =
          match (opt1, opt2) with
          | Some x, Some y -> Some (x / y)
          | _ -> None
        in
        
          Dream.html @@ html_to_string (generate request ~who:(int_to_str (add_options val1 val2)))
      | _ -> failwith "Form data parsing error"
        );

    Dream.post "/add" (fun request ->
      let find_data form_data key =
        match List.assoc key form_data with
        | data -> data
        | exception Not_found -> "Not provided"
      in
      let%lwt form_result = Dream.form request in
      match form_result with
      | `Ok form_data ->
        let val1 = find_data form_data "value_1" in
        let val2 = find_data form_data "value_2" in

        let str_to_int str =
          try Some (int_of_string str) with
          | Failure _ -> None
        in

        let int_to_str int =
          match int with
          | Some i -> Some (string_of_int i)
          | None -> None
        in

        let val1 =  str_to_int val1 in
        let val2 =  str_to_int val2 in
        
        let add_options opt1 opt2 =
          match (opt1, opt2) with
          | Some x, Some y -> Some (x + y)
          | _ -> None
        in
        
          Dream.html @@ html_to_string (generate request ~who:(int_to_str (add_options val1 val2)))
      | _ -> failwith "Form data parsing error"
        );

    Dream.post "/sub" (fun request ->
      let find_data form_data key =
        match List.assoc key form_data with
        | data -> data
        | exception Not_found -> "Not provided"
      in
      let%lwt form_result = Dream.form request in
      match form_result with
      | `Ok form_data ->
        let val1 = find_data form_data "value_1" in
        let val2 = find_data form_data "value_2" in

        let str_to_int str =
          try Some (int_of_string str) with
          | Failure _ -> None
        in

        let int_to_str int =
          match int with
          | Some i -> Some (string_of_int i)
          | None -> None
        in

        let val1 =  str_to_int val1 in
        let val2 =  str_to_int val2 in
        
        let add_options opt1 opt2 =
          match (opt1, opt2) with
          | Some x, Some y -> Some (x - y)
          | _ -> None
        in
        
          Dream.html @@ html_to_string (generate request ~who:(int_to_str (add_options val1 val2)))
      | _ -> failwith "Form data parsing error"
        );

    Dream.post "/mult" (fun request ->
      let find_data form_data key =
        match List.assoc key form_data with
        | data -> data
        | exception Not_found -> "Not provided"
      in
      let%lwt form_result = Dream.form request in
      match form_result with
      | `Ok form_data ->
        let val1 = find_data form_data "value_1" in
        let val2 = find_data form_data "value_2" in

        let str_to_int str =
          try Some (int_of_string str) with
          | Failure _ -> None
        in

        let int_to_str int =
          match int with
          | Some i -> Some (string_of_int i)
          | None -> None
        in

        let val1 =  str_to_int val1 in
        let val2 =  str_to_int val2 in
        
        let add_options opt1 opt2 =
          match (opt1, opt2) with
          | Some x, Some y -> Some (x * y)
          | _ -> None
        in
        
          Dream.html @@ html_to_string (generate request ~who:(int_to_str (add_options val1 val2)))
      | _ -> failwith "Form data parsing error"
        )

  ]