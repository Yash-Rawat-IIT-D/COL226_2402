LEXER=my_lexer
MAIN=main
TOKEN=token
EXEC=my_lexer_test

all: $(EXEC)

$(EXEC): $(LEXER).cmo $(MAIN).cmo
	ocamlc -o $(EXEC) $^

$(TOKEN).cmi: $(TOKEN).mli
	ocamlc -c $<

$(LEXER).ml: $(LEXER).mll
	ocamllex $<

$(LEXER).cmo: $(LEXER).ml $(TOKEN).cmi
	ocamlc -c $(LEXER).ml

$(MAIN).cmo: $(MAIN).ml $(TOKEN).cmi $(LEXER).cmo
	ocamlc -c $(MAIN).ml

test: $(EXEC)
		@mkdir -p output
		@for file in tests/*; do \
			if [ -f "$$file" ]; then \
				filename=$$(basename "$$file"); \
				./$(EXEC) "$$file" > "output/$$filename.out"; \
			fi \
		done

clean:
	rm -f *.cmo *.cmi *.o $(LEXER).ml $(EXEC) output/*.out

clean_test:
	rm -f output/*.out

.PHONY: all clean test