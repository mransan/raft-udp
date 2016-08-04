OCB_INC  += -I src/app 
OCB_INC  += -I src/clt
OCB_INC  += -I src/com 
OCB_INC  += -I src/srv 
OCB_INC  += -I src/utl
OCB_INC  += -I src/cry
OCB_INC  += -I tests/
OCB_INC  += -I tests/counter
OCB_INC  += -I tests/asset
OCB_INC  += -I tests/unit_tests/

OCB_FLAGS = -use-ocamlfind -pkgs ocaml-protoc,raft,lwt.unix,ppx_deriving.show
OCB       = ocamlbuild $(OCB_FLAGS) $(OCB_INC)

ifeq "$(shell ocamlc -config | grep os_type)" "os_type: Win32"
	@EXE=.exe
else
	@EXE=
endif

.PHONY: test gen lib.native lib.byte lib.install lib.uninstall clean 
.PHONY: framework counter asset unit

all: framework counter asset

framework: 
	$(OCB) server.native
	$(OCB) start_all_servers.native
	$(OCB) start_all_clients.native

counter:
	$(OCB) counter_srv.native
	$(OCB) counter_clt.native

asset:
	$(OCB) -pkgs cryptokit,base58 asset_test_01.native
	$(OCB) -pkgs cryptokit,base58 asset_test_02.native
	$(OCB) -pkgs cryptokit,base58 asset_srv.native
	$(OCB) -pkgs cryptokit,base58 asset_clt.native

unit:
	$(OCB) test_utl_encoding.native
	./test_utl_encoding.native
	$(OCB) test_srv_logrecord.native
	./test_srv_logrecord.native

gen:
	ocaml-protoc -I ../raft.git/src/ -ml_out src/com src/com/raft_com.proto
	ocaml-protoc -I ../raft.git/src/ -I src/com -ml_out src/clt src/clt/raft_clt.proto
	ocaml-protoc -I ../raft.git/src/ -I src/com -ml_out src/app src/app/raft_app.proto
	ocaml-protoc -I ../raft.git/src/ -ml_out tests/counter  tests/counter/counter.proto
	ocaml-protoc -I ../raft.git/src/ -ml_out tests/asset  tests/asset/asset.proto

lib.native:
	$(OCB) raft_udp.cmxa
	$(OCB) raft_udp.cmxs

lib.byte:
	$(OCB) raft_udp.cma

clean:
	$(OCB) -clean
