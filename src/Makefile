# TO BE MODIFIED

EXE=_build/default/main.exe

all: $(EXE)

$(EXE): *.ml*
	dune build @all
	ln -sf $(EXE) ccomp

%.png: %.c
	$(EXE) --debug --parse-only $^
	dot -v -Tpng -O $(basename $@)_ast.dot

cfg: $(EXE)
	./ccomp --convert-cfg cfg_test.dot test.c
	dot -v -Tpng -O cfg_test.dot

test: $(EXE)
	-make clean >/dev/null
	-make all >/dev/null
	./../tst/test.bash

.PHONY: clean
clean:
	-dune clean
	rm -f ../tst/*/*.dot ../tst/*/*.dot.png ../tst/*/*.log *.dot *.dot.png *~ *.log ccomp
