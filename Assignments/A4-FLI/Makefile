# Source files and output
SOURCES = src/secd_machine.ml src/secd_main.ml
RESULT = build/secd_main

# OCaml compilation flags
OCAMLFLAGS = -I src
OCAMLC = ocamlc

# Default target
secd_m: $(RESULT)

# Compile the program
$(RESULT): $(SOURCES)
	$(OCAMLC) $(OCAMLFLAGS) -o $@ $^

# Clean generated files
clean:
	rm -f src/*.cmo src/*.cmi  src/*.o
	rm -f $(RESULT)

# Run the SECD machine
run: $(RESULT)
	./$(RESULT)

# Run with specific test cases
test: $(RESULT)
	./$(RESULT) test

.PHONY: all clean run test