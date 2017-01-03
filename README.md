# SimpleMessenger

A simple one-to-one client-server chat implemented in Ocaml

How to compile (linux/ubuntu):

ocamlfind ocamlopt -package lwt,lwt.unix -linkpkg -o server ./server.ml

ocamlfind ocamlopt -package lwt,lwt.unix -linkpkg -o client ./client.ml

How to run (linux/ubuntu):

to start server

./server [port number]

to start client

./client [server hostname] [port number] 
