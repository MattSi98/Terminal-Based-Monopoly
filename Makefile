MODULES=player main property state command consts boardPrinter properties
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
MAIN=main.byte
TEST=test.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS=oUnit,ANSITerminal,str

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS) 

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

zip:
	zip 3110Project.zip *.ml* _tags Makefile INSTALL.txt .merlin

test:
	$(OCAMLBUILD) -tag debug $(TEST) && ./$(TEST)

docs: docs-public docs-private
	
docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private report search_src.zip bisect*.out