LEXER = src/my_lexer
PARSER = src/my_parser
TOKEN = src/token
AST = src/my_ast
TYPE_CHECK = src/type_check

EXEC = my_parser_test

all: $(EXEC)

# Compile the token interface
$(TOKEN).cmi: $(TOKEN).mli
	ocamlc -c -I src $<

# Generate parser code from .mly file
$(PARSER).ml $(PARSER).mli: $(PARSER).mly
	ocamlyacc $<

# Generate lexer code from .mll file
$(LEXER).ml: $(LEXER).mll $(TOKEN).cmi
	ocamllex $<

# Compile the AST file
$(AST).cmo: $(AST).ml $(TOKEN).cmi
	ocamlc -c -I src $<

# Compile the lexer (make sure it knows about the token module)
$(LEXER).cmo: $(LEXER).ml $(TOKEN).cmi
	ocamlc -c -I src $<

# Compile the parser
$(PARSER).cmo: $(PARSER).ml $(TOKEN).cmi $(AST).cmo
	ocamlc -c -I src $<

# Compile the type checker
$(TYPE_CHECK).cmo: $(TYPE_CHECK).ml $(AST).cmi $(TOKEN).cmi
	ocamlc -c -I src $<

# Link everything together into a single binary (for testing parser)
$(EXEC): $(LEXER).cmo $(PARSER).cmo $(AST).cmo $(TYPE_CHECK).cmo
	ocamlc -o $(EXEC) $^

# Test compilation only
test-grammar:
	@echo "Checking if grammar compiles..."
	@ocamlyacc $(PARSER).mly && echo "✅ Parser compiled successfully!" || echo "❌ Parser compilation failed!"

clean:
	rm -f src/*.cmo src/*.cmi src/*.o src/my_lexer.ml src/my_parser.ml src/my_parser.mli $(EXEC)

.PHONY: all clean test-grammar
