# Makefile for OCaml Lexer and Parser Project

# Compiler tools
OCAMLC = ocamlc
OCAMLLEX = ocamllex
OCAMLYACC = ocamlyacc

# Directories
SRC_DIR = src
BUILD_DIR = build
OUTPUT_DIR = output

# Flags
OCAMLFLAGS = -I $(SRC_DIR)

# Default target
all: directories $(BUILD_DIR)/lexer_test

# Create necessary directories
directories:
	mkdir -p $(BUILD_DIR) $(OUTPUT_DIR)

# Generate parser from .mly file
$(SRC_DIR)/my_parser.ml $(SRC_DIR)/my_parser.mli: $(SRC_DIR)/my_parser.mly
	$(OCAMLYACC) $<

# Generate lexer from .mll file
$(SRC_DIR)/my_lexer.ml: $(SRC_DIR)/my_lexer.mll
	$(OCAMLLEX) -o $@ $<

# Compile my_ast.ml (independent of everyone)
$(SRC_DIR)/my_ast.cmo $(SRC_DIR)/my_ast.cmi: $(SRC_DIR)/my_ast.ml
	$(OCAMLC) $(OCAMLFLAGS) -c $< -o $(SRC_DIR)/my_ast.cmo

# Compile parser interface (depends on my_ast)
$(SRC_DIR)/my_parser.cmi: $(SRC_DIR)/my_parser.mli $(SRC_DIR)/my_ast.cmi
	$(OCAMLC) $(OCAMLFLAGS) -c $< -o $(SRC_DIR)/my_parser.cmi

# Compile parser implementation (depends on my_parser.cmi)
$(SRC_DIR)/my_parser.cmo: $(SRC_DIR)/my_parser.ml $(SRC_DIR)/my_parser.cmi
	$(OCAMLC) $(OCAMLFLAGS) -c $< -o $(SRC_DIR)/my_parser.cmo

# Compile lexer (depends on my_parser and my_ast)
$(SRC_DIR)/my_lexer.cmo: $(SRC_DIR)/my_lexer.ml $(SRC_DIR)/my_parser.cmi $(SRC_DIR)/my_ast.cmi
	$(OCAMLC) $(OCAMLFLAGS) -c $< -o $(SRC_DIR)/my_lexer.cmo

# Compile main_lexer (depends on my_lexer)
$(SRC_DIR)/main_lexer.cmo: $(SRC_DIR)/main_lexer.ml $(SRC_DIR)/my_lexer.cmo
	$(OCAMLC) $(OCAMLFLAGS) -c $< -o $(SRC_DIR)/main_lexer.cmo

# Build lexer test executable (final target in build directory)
$(BUILD_DIR)/lexer_test: $(SRC_DIR)/my_ast.cmo $(SRC_DIR)/my_parser.cmo $(SRC_DIR)/my_lexer.cmo $(SRC_DIR)/main_lexer.cmo
	$(OCAMLC) $(OCAMLFLAGS) -o $@ $^

# For future: Compile main_parser (for parser testing)
$(SRC_DIR)/main_parser.cmo: $(SRC_DIR)/main_parser.ml $(SRC_DIR)/my_parser.cmo $(SRC_DIR)/my_lexer.cmo
	$(OCAMLC) $(OCAMLFLAGS) -c $< -o $(SRC_DIR)/main_parser.cmo

# For future: Build parser test executable
$(BUILD_DIR)/parser_test: $(SRC_DIR)/my_ast.cmo $(SRC_DIR)/my_parser.cmo $(SRC_DIR)/my_lexer.cmo $(SRC_DIR)/main_parser.cmo
	$(OCAMLC) $(OCAMLFLAGS) -o $@ $^

# Run tests
test: $(BUILD_DIR)/lexer_test
	@for t in test/t*; do \
		echo "Testing $$t"; \
		$(BUILD_DIR)/lexer_test < $$t > $(OUTPUT_DIR)/`basename $$t`.out; \
	done

# Clean target to remove generated files
clean:
	rm -f $(SRC_DIR)/*.cm* $(SRC_DIR)/my_parser.ml $(SRC_DIR)/my_parser.mli $(SRC_DIR)/my_lexer.ml
	rm -f $(BUILD_DIR)/* $(OUTPUT_DIR)/*

# Phony targets
.PHONY: all clean test directories
