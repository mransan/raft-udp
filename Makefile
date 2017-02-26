OCB_INC  += -I src/app 
OCB_INC  += -I src/clt
OCB_INC  += -I src/com 
OCB_INC  += -I src/srv 
OCB_INC  += -I src/utl
OCB_INC  += -I src/cry
OCB_INC  += -I tests/
OCB_INC  += -I tests/counter
OCB_INC  += -I tests/hash
OCB_INC  += -I tests/asset

OCB_FLAGS = -use-ocamlfind 
OCB_FLAGS+= -pkgs result,ocplib-endian,ocaml-protoc
OCB_FLAGS+= -pkgs mtime.os,ptime.clock.os,raft,raft-pb,lwt.unix
OCB       = ocamlbuild $(OCB_FLAGS) $(OCB_INC)

ifeq "$(shell ocamlc -config | grep os_type)" "os_type: Win32"
	@EXE=.exe
else
	@EXE=
endif

.PHONY: test gen lib.native lib.byte lib.install lib.uninstall clean 

test: 
	$(OCB) -pkg raft-rocks server.native
	$(OCB) counter_srv.native
	$(OCB) counter_clt.native
	$(OCB) hash_srv.native
	$(OCB) hash_clt.native
	$(OCB) start_all_servers.native
	$(OCB) start_all_clients.native

gen:
	ocaml-protoc -I ../raft-pb.git/src/ -ml_out src/com src/com/raft_com.proto
	ocaml-protoc -I ../raft-pb.git/src/ -ml_out tests/counter tests/counter/counter.proto
	ocaml-protoc -I ../raft-pb.git/src/ -ml_out tests/hash tests/hash/hash.proto

lib.native:
	$(OCB) raft_udp.cmxa
	$(OCB) raft_udp.cmxs

lib.byte:
	$(OCB) raft_udp.cma

clean:
	$(OCB) -clean
