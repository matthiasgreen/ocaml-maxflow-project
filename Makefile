.PHONY: all build format edit demo clean

EXEC=btest
DEMO_BP=bipartites/bipartite_readme.txt

all: build

build:
	@echo "\n   🚨  COMPILING  🚨 \n"
	dune build src/$(EXEC).exe
	ls src/*.exe > /dev/null && ln -fs src/*.exe .

format:
	ocp-indent --inplace src/*

edit:
	code . -n

demo: build
	@echo "\n   ⚡  EXECUTING  ⚡\n"
	./$(EXEC).exe $(DEMO_BP) outfile
	@echo "\n   🥁  RESULT (content of outfile)  🥁\n"
	@cat outfile

clean:
	find -L . -name "*~" -delete
	rm -f *.exe
	dune clean
