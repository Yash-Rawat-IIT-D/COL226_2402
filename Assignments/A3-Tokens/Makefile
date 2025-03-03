LEXER=lexer
MAIN=main
TOKEN=token
EXEC=lexer_test

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

clean:
	rm -f *.cmo *.cmi *.o $(LEXER).ml $(EXEC)
