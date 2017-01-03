open Lwt;;


let rec serv_service ic oc () =
    try
        let t = Lwt_io.read_line Lwt_io.stdin in
        let start = Unix.gettimeofday() in
        Lwt.bind t
        (fun send_msg -> Lwt_io.write_line oc send_msg);

        Lwt_io.read_line_opt ic >>=
        (fun msg ->
            match Lwt.state t with
            | Lwt.Fail exn -> Lwt.fail exn
            | _  -> match msg with
                | Some "message received by client" ->
                    let time_elapsed = string_of_float (Unix.gettimeofday() -. start) in
                    Lwt_io.write_line Lwt_io.stdout ("message received by client. time elapsed (s) : "^time_elapsed) >>=
                    (fun () -> Lwt_io.flush Lwt_io.stdout >>= serv_service ic oc)
                | Some msg ->
                    let reply = "message received by server" in
                    Lwt_io.write_line oc reply >>=
                    (fun () -> Lwt_io.write_line Lwt_io.stdout msg >>=
                        (fun () -> Lwt_io.flush Lwt_io.stdout >>=
                            serv_service ic oc))
                | None -> Lwt_io.write_line Lwt_io.stdout "Connection closed" >>=
                       (fun () -> Lwt_io.flush Lwt_io.stdout >>= return))
    with
        Exit -> return_unit
      | exn -> Lwt_io.close ic >>= (fun () -> Lwt.fail exn) ;;



let create_server sock = 
   let rec serve () =
       Lwt_unix.accept sock >>= (fun (s, caller) ->
           let inchan = Lwt_chan.in_channel_of_descr s
           and outchan = Lwt_chan.out_channel_of_descr s in
           Lwt.on_failure (serv_service inchan outchan ()) (fun e -> Lwt_log.ign_error (Printexc.to_string e));
           Lwt_log.info "New connection" >>= return;) >>= serve
   in serve;;


let establish_server sockaddr =
   let open Lwt_unix in
   let domain = PF_INET in
   let sock = socket domain SOCK_STREAM 0 in
   bind sock sockaddr ;
   listen sock 3;
   create_server sock;;

let get_my_addr () =
   (Unix.gethostbyname(Unix.gethostname())).Unix.h_addr_list.(0) ;;


let main_server () =
   if Array.length Sys.argv < 2 then Printf.eprintf "usage : serv_up port\n"
   else
        try
          let port =  int_of_string Sys.argv.(1) in
          let my_address = get_my_addr()
          in
              Lwt_main.run (establish_server (Unix.ADDR_INET(my_address, port)) ());
        with
          Failure("int_of_string") ->
            Printf.eprintf "serv_up : bad port number\n" ;;
    

main_server();;

