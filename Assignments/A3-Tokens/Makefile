# Directories
SRC = src
BUILD = build

# File Paths
LEXER = $(SRC)/my_lexer
TOKEN = $(SRC)/token
AST = $(SRC)/my_ast
PARSER = $(SRC)/my_parser
ENV = $(SRC)/env
TYPE_CHECK = $(SRC)/type_check
MAIN_LEX = $(SRC)/main_lex
MAIN_PARSER = $(SRC)/main_parser

# Output Files
TEST_LEXER = $(BUILD)/test_lexer
TEST_PARSER = $(BUILD)/test_parser

# Build All
all: $(TEST_LEXER) $(TEST_PARSER)

# Create build directory if it doesn't exist
$(BUILD):
	mkdir -p $(BUILD)

# Compile the token interface into src directory
src/token.cmi: $(TOKEN).mli
	ocamlc -c -I $(SRC) $<

# Generate lexer code from .mll file into src directory
src/my_lexer.ml: $(LEXER).mll src/token.cmi
	ocamllex -o $@ $<

# Compile the lexer into src directory
src/my_lexer.cmo: src/my_lexer.ml src/token.cmi
	ocamlc -c -I $(SRC) $<

# Build test_lexer into build directory
$(TEST_LEXER): src/my_lexer.cmo $(MAIN_LEX).ml
	@echo "Compiling test_lexer..."
	ocamlc -o $@ -I $(SRC) $^
	@echo "✅ Test lexer compiled!"

# Compile AST into src directory (depends on token)
src/my_ast.cmo: $(AST).ml src/token.cmi
	ocamlc -c -I $(SRC) $<

# Compile environment into src directory (depends on AST)
src/env.cmo: $(ENV).ml src/my_ast.cmo src/token.cmi
	ocamlc -c -I $(SRC) $<

# Compile type checker into src directory (depends on AST + env)
src/type_check.cmo: $(TYPE_CHECK).ml src/my_ast.cmo src/env.cmo src/token.cmi
	ocamlc -c -I $(SRC) $<

# Generate parser code from .mly file into src directory
src/my_parser.ml src/my_parser.mli: $(PARSER).mly
	ocamlyacc $<

# Compile parser interface into src directory
src/my_parser.cmi: src/my_parser.mli src/my_ast.cmo
	ocamlc -c -I $(SRC) $<

# Compile parser into src directory (depends on AST + token)
src/my_parser.cmo: src/my_parser.ml src/my_ast.cmo src/token.cmi src/my_parser.cmi
	ocamlc -c -I $(SRC) $<

# Build test_parser into build directory (linking order matters!)
$(TEST_PARSER): src/my_lexer.cmo src/my_parser.cmo src/my_ast.cmo src/env.cmo src/type_check.cmo $(MAIN_PARSER).ml
	@echo "Compiling test_parser..."
	ocamlc -o $@ -I $(SRC) $^
	@echo "✅ Test parser compiled!"

# Clean everything in src and build directories
clean:
	rm -rf $(BUILD)/*
	rm -f src/*.cmi src/*.cmo src/*.o
	rm -f src/my_lexer.ml src/my_parser.ml src/my_parser.mli
	@echo "✅ Cleaned up src and build directories!"

.PHONY: all clean
