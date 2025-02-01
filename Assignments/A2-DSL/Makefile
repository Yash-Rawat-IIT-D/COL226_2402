CC = ocamlc

SRC = my_code.ml
SRC_MLI = my_code.mli
SRC_CMO = my_code.cmo
SRC_CMI = my_code.cmi

TARGET = main.exe

all : $(TARGET)

$(TARGET) : $(SRC_CMO)
	
	ocamlc -o $@ $<

$(SRC_CMO) : $(SRC_CMI) $(SRC)
	ocamlc -o $@ -c $(SRC)

$(SRC_CMI) : $(SRC_MLI)
	ocamlc -o my_code.cmi -c $(SRC_MLI)		

$(SRC_MLI) : $(SRC)
	ocamlc -i $(SRC) > $(SRC_MLI)

clean :
	rm -f *.cmo *.cmi *.mli *.exe