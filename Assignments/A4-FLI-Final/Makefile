# Makefile for Krivine and SECD Machine implementations

# Compiler and flags
OCAMLC = ocamlc

# Targets
all: krv_test secd_test

# Krivine Machine test
krv_test: machine.cmo krv_test.cmo
	$(OCAMLC) $(OCAMLFLAGS) -o krv_test machine.cmo  krv_test.cmo

# SECD Machine test
secd_test: machine.cmo secd_test.cmo
	$(OCAMLC) $(OCAMLFLAGS) -o secd_test machine.cmo secd_test.cmo

# Compile machine.ml (shared definitions)
machine.cmo: machine.ml
	$(OCAMLC) $(OCAMLFLAGS) -c machine.ml


# Compile krv_test.ml
krv_test.cmo: krv_test.ml machine.cmo 
	$(OCAMLC) $(OCAMLFLAGS) -c krv_test.ml

# Compile secd_test.ml
secd_test.cmo: secd_test.ml machine.cmo
	$(OCAMLC) $(OCAMLFLAGS) -c secd_test.ml

# Run tests
run_krv: krv_test
	./krv_test

run_secd: secd_test
	./secd_test

run: run_krv run_secd

# Clean generated files
clean:
	rm -f *.cmo *.cmi krv_test secd_test

.PHONY: all clean run run_krv run_secd
