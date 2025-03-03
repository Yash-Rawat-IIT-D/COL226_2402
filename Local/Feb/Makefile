CC = ocamlc

SRC = Subs_Unif.ml
SRC_MLI = Subs_Unif.mli
SRC_CMO = Subs_Unif.cmo
SRC_CMI = Subs_Unif.cmi

TARGET = main.exe

all : $(TARGET)

$(TARGET) : $(SRC_CMO)
	
	ocamlc -o $@ $<

$(SRC_CMO) : $(SRC_CMI) $(SRC)
	ocamlc -o $@ -c $(SRC)

$(SRC_CMI) : $(SRC_MLI)
	ocamlc -o Subs_Unif.cmi -c $(SRC_MLI)		

$(SRC_MLI) : $(SRC)
	ocamlc -i $(SRC) > $(SRC_MLI)

clean :
	rm -f *.cmo *.cmi *.mli *.exe