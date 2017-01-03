open Lwt;;


let write_to_ui str = Lwt_io.write_line Lwt_io.stdout str >>= (fun () -> Lwt_io.flush Lwt_io.stdout);;



let rec client_service ic oc () =

    try 
        let t = Lwt_io.read_line Lwt_io.stdin in
        let start = Unix.gettimeofday() in
        let _ = Lwt.bind t
        (fun send_msg -> Lwt_io.write_line oc send_msg >>= (fun () -> Lwt_io.flush oc) >>= client_service ic oc) in
 
        Lwt_io.read_line_opt ic >>=
    	(fun msg ->
            match Lwt.state t with
            | Lwt.Fail exn -> Lwt.fail exn
            | _  -> match msg with
                | Some "message received by server" -> 
                    let time_elapsed = string_of_float (Unix.gettimeofday() -. start) in 
                    write_to_ui ("message received by server. time elapsed (s) :"^time_elapsed) >>= client_service ic oc
                | Some msg ->
                    let reply = "message received by client" in
                    Lwt_io.write_line oc reply >>= 
                    (fun () -> Lwt_io.flush oc >>=
                        (fun () -> write_to_ui msg >>= 
                            client_service ic oc))
                | None ->  write_to_ui "Connection closed" >>= raise (Failure "server stopped"))
    with
        Exit -> return_unit
      | exn -> Lwt_io.close ic >>= (fun () -> Lwt.fail exn) ;;



let establish_client sockaddr =
    Lwt_io.open_connection sockaddr >>=
    (fun (ic, oc) -> write_to_ui "Connection established. Please enter message below" >>= 
        (fun () -> client_service ic oc () >>= 
            (fun() -> Lwt_io.close ic)));;



let main_client () =
    if Array.length Sys.argv < 3 then
        Printf.printf "usage : client server port \n"
    else let server = Sys.argv.(1) in
        let server_addr = 
            try Unix.inet_addr_of_string server
            with Failure("inet_addr_of_string") ->
                try (Unix.gethostbyname server).Unix.h_addr_list.(0)
                with Not_found ->
                    Printf.eprintf "%s : Unknown server\n" server; exit 2
        in try 
            let port = int_of_string (Sys.argv.(2)) in 
            let sockaddr = Unix.ADDR_INET(server_addr, port) in
            Lwt_main.run (establish_client sockaddr);
           with Failure("int_of_string") -> Printf.eprintf "bad port number"; exit 2;;

main_client();;
