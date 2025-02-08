CC = ocamlc

SRC = a2.ml
SRC_MLI = a2.mli
SRC_CMO = a2.cmo
SRC_CMI = a2.cmi

TARGET = main.exe

all : $(TARGET)

$(TARGET) : $(SRC_CMO)
	
	ocamlc -o $@ $<

$(SRC_CMO) : $(SRC_CMI) $(SRC)
	ocamlc -o $@ -c $(SRC)

$(SRC_CMI) : $(SRC_MLI)
	ocamlc -o a2.cmi -c $(SRC_MLI)		

$(SRC_MLI) : $(SRC)
	ocamlc -i $(SRC) > $(SRC_MLI)

clean :
	rm -f *.cmo *.cmi *.mli *.exe