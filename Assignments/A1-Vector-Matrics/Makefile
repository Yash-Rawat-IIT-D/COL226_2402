CC = ocamlc

SRC = vector.ml
SRC_MLI = vector.mli
SRC_CMO = vector.cmo
SRC_CMI = vector.cmi

TARGET = main.exe

all : $(TARGET)

$(TARGET) : $(SRC_CMO)
	
	ocamlc -o $@ $<

$(SRC_CMO) : $(SRC_CMI) $(SRC)
	ocamlc -o $@ -c $(SRC)

$(SRC_CMI) : $(SRC_MLI)
	ocamlc -o vector.cmi -c $(SRC_MLI)		

$(SRC_MLI) : $(SRC)
	ocamlc -i $(SRC) > $(SRC_MLI)

clean :
	rm -f *.cmo *.cmi *.mli *.exe