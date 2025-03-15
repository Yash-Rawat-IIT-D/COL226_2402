# Paths and filenames
SRC_DIR = src
LEXER = $(SRC_DIR)/my_lexer
MAIN = $(SRC_DIR)/main
TOKEN = $(SRC_DIR)/token
EXEC = my_lexer_test

# Build the lexer and main components
all: $(EXEC)

# Build token interface FIRST (Add -I src to include directory)
$(TOKEN).cmi: $(TOKEN).mli
	ocamlc -I $(SRC_DIR) -c $<

# Generate lexer from .mll file AFTER token interface is compiled
$(LEXER).ml: $(LEXER).mll $(TOKEN).cmi
	ocamllex $<

# Compile lexer AFTER token interface is compiled
$(LEXER).cmo: $(LEXER).ml $(TOKEN).cmi
	ocamlc -I $(SRC_DIR) -c $(LEXER).ml

# Compile main program
$(MAIN).cmo: $(MAIN).ml $(TOKEN).cmi $(LEXER).cmo
	ocamlc -I $(SRC_DIR) -c $(MAIN).ml

# Link and create the executable
$(EXEC): $(LEXER).cmo $(MAIN).cmo
	ocamlc -o $(EXEC) -I $(SRC_DIR) $^

# Run test cases
test: $(EXEC)
	@mkdir -p output
	@for file in tests/*; do \
		if [ -f "$$file" ]; then \
			filename=$$(basename "$$file"); \
			./$(EXEC) "$$file" > "output/$$filename.out"; \
		fi \
	done

# Clean compiled files and outputs
clean:
	rm -f $(SRC_DIR)/*.cmo $(SRC_DIR)/*.cmi $(SRC_DIR)/*.o $(LEXER).ml $(EXEC) output/*.out

# Clean only test outputs
clean_test:
	rm -f output/*.out

# Declare phony targets
.PHONY: all clean test clean_test
