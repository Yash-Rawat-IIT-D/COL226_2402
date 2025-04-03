CC = ocamlc

SRC = main.ml
SRC_MLI = main.mli
SRC_CMO = main.cmo
SRC_CMI = main.cmi

TARGET = main.exe

all : $(TARGET)

$(TARGET) : $(SRC_CMO)
	
	ocamlc -o $@ $<

$(SRC_CMO) : $(SRC_CMI) $(SRC)
	ocamlc -o $@ -c $(SRC)

$(SRC_CMI) : $(SRC_MLI)
	ocamlc -o main.cmi -c $(SRC_MLI)		

$(SRC_MLI) : $(SRC)
	ocamlc -i $(SRC) > $(SRC_MLI)

clean :
	rm -f *.cmo *.cmi *.mli *.exe