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
	$(OCB) test.native
	time ./test.native -m server --id 0  

gen:
	ocaml-protoc -I ../raft/src/ -ml_out src src/raft_udp.proto

lib.native:
	$(OCB) raft_udp.cmxa
	$(OCB) raft_udp.cmxs

lib.byte:
	$(OCB) raft_udp.cma

# LIB_FILES=raft_pb raft_helper raft_logic
# 
# LIB_BUILD     =_build/src/
# LIB_INSTALL   = META 
# LIB_INSTALL  +=$(patsubst %,$(LIB_BUILD)/%.cmi,$(LIB_FILES))
# LIB_INSTALL  +=$(patsubst %,$(LIB_BUILD)/%.annot,$(LIB_FILES))
# LIB_INSTALL  +=$(patsubst %,$(LIB_BUILD)/%.cmo,$(LIB_FILES))
# LIB_INSTALL  +=$(LIB_BUILD)/raft.cma 
# 
# LIB_INSTALL  +=-optional  
# LIB_INSTALL  +=$(patsubst %,$(LIB_BUILD)/%.cmx,$(LIB_FILES))
# LIB_INSTALL  +=$(patsubst %,$(LIB_BUILD)/%.cmt,$(LIB_FILES))
# LIB_INSTALL  +=$(LIB_BUILD)/raft.cmxa 
# LIB_INSTALL  +=$(LIB_BUILD)/raft.cmxs
# LIB_INSTALL  +=$(LIB_BUILD)/raft.a
# 
# lib.install:
# 	ocamlfind install raft $(LIB_INSTALL)
# 
# lib.uninstall:
# 	ocamlfind remove raft

clean:
	$(OCB) -clean
