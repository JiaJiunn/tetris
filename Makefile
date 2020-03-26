# Define variables
MODULES=gamesetup movetetro scoreline drawfuncs inactivefuncs pausefuncs settingsfuncs tutorialfuncs leaderboardcsv main
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=tests.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
MAIN=main.byte

# Recipes 
default: build #first build (dependies) then run utop (command)
	utop

build: 
	$(OCAMLBUILD) $(OBJECTS) 

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

clean:
	ocamlbuild -clean

docs: docs-public docs-private
	
docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
