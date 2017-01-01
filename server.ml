
let establish_server server_fun sockaddr =
   let domain = Unix.PF_INET in
   let sock = Unix.socket domain Unix.SOCK_STREAM 0 
   in Unix.bind sock sockaddr ;
      Unix.listen sock 3;
      while true do
        let (s, caller) = Unix.accept sock 
        in match Unix.fork() with
               0 -> if Unix.fork() <> 0 then exit 0 ; 
                    let inchan = Unix.in_channel_of_descr s 
                    and outchan = Unix.out_channel_of_descr s 
                    in server_fun inchan outchan ;
                       close_in inchan ;
                       close_out outchan ;
                       exit 0
             | id -> Unix.close s; ignore(Unix.waitpid [] id)
      done ;;

let get_my_addr () =
   (Unix.gethostbyname(Unix.gethostname())).Unix.h_addr_list.(0) ;;

let main_server  serv_fun =
   if Array.length Sys.argv < 2 then Printf.eprintf "usage : serv_up port\n"
   else 
        try
          let port =  int_of_string Sys.argv.(1) in 
          let my_address = get_my_addr() 
          in
              establish_server serv_fun  (Unix.ADDR_INET(my_address, port));
        with
          Failure("int_of_string") -> 
            Printf.eprintf "serv_up : bad port number\n" ;;

let receive_service ic oc =
   try while true do    
         let receive_msg = input_line ic in
            Printf.printf "Client : %s \n" receive_msg;
            flush stdout; 
            output_string oc ("message received \n") ;
            flush oc;
            print_string "Server :";
            flush stdout;
       done
   with _ -> Printf.printf "End of text\n" ; flush stdout ; exit 0 ;;

let go_server_service () =
   Printf.printf "Server started\n";
   flush stdout;
   Unix.handle_unix_error main_server receive_service ;; 

go_server_service () ;;
