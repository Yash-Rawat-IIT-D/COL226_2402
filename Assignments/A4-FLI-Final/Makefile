OCAMLC = ocamlc

# Target
all: test

# Bytecode compilation
test: machine.cmo main_test.cmo
	$(OCAMLC) $(OCAMLFLAGS) -o test machine.cmo main_test.cmo

# Compile machine.ml
machine.cmo: machine.ml
	$(OCAMLC) $(OCAMLFLAGS) -c machine.ml

# Compile main_test.ml
main_test.cmo: main_test.ml machine.cmo
	$(OCAMLC) $(OCAMLFLAGS) -c main_test.ml

# Run tests
run: test
	./test

# Clean generated files
clean:
	rm -f *.cmo *.cmi test

.PHONY: all clean run