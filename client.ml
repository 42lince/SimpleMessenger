let main_client client_fun =
    if Array.length Sys.argv < 3 then
        Printf.printf "usage : client server port \n"
    else let server = Sys.argv.(1) in
        let server_addr = 
            try Unix.inet_addr_of_string server
            with Failure("inet_addr_of_string") ->
                try (Unix.gethostbyname server).Unix.h_addr_list.(0)
                with Not_found ->
                    Printf.eprintf "%s : Unknown server\n" server;
                    exit 2
        in try 
            let port = int_of_string (Sys.argv.(2)) in 
            let sockaddr = Unix.ADDR_INET(server_addr, port) in
            let ic, oc = Unix.open_connection sockaddr
            in client_fun ic oc ;
                Unix.shutdown_connection ic
           with Failure("int_of_string") -> Printf.eprintf "bad port number";
                                            exit 2;;

let client_fun ic oc =
    try 
        while true do
            print_string "Client : ";
            flush stdout;
            let send_msg = input_line stdin in
            output_string oc (send_msg ^ "\n");
            flush oc;
            let r = input_line ic in
                Printf.printf "Server : %s\n\n" r ; flush stdout;
        done
    with
        Exit -> exit 0
      | exn -> Unix.shutdown_connection ic ; raise exn ;;

let run_client () = main_client client_fun;;

run_client() ;; 
