OCB_INC   = -I src -I tests 
OCB_FLAGS = -use-ocamlfind -pkgs ocaml-protoc -pkgs raft -pkgs lwt.unix
OCB       = ocamlbuild $(OCB_FLAGS) $(OCB_INC)

ifeq "$(shell ocamlc -config | grep os_type)" "os_type: Win32"
	@EXE=.exe
else
	@EXE=
endif

.PHONY: test gen lib.native lib.byte lib.install lib.uninstall clean 

test: 
	$(OCB) server.native
	$(OCB) client.native
	$(OCB) app.native
	$(OCB) start_all_servers.native
	$(OCB) start_all_clients.native

gen:
	ocaml-protoc -I ../raft.git/src/ -ml_out src src/raft_udp.proto

lib.native:
	$(OCB) raft_udp.cmxa
	$(OCB) raft_udp.cmxs

lib.byte:
	$(OCB) raft_udp.cma

clean:
	$(OCB) -clean
