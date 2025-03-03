CC = ocamlc
SRC = main.ml
SRC_MLI = main.mli
SRC_EXTRA = main.cmi main.cmo
TARGET = toy

all : $(TARGET)

$(TARGET) : $(SRC) $(SRC_MLI)
	@ $(CC) $(SRC_MLI)
	@ $(CC) -o $(TARGET) $(SRC)

$(SRC_MLI) : $(SRC)
	@ $(CC) -i $(SRC) > $(SRC_MLI)

clean :
	@ rm -f $(TARGET) $(SRC_MLI) $(SRC_EXTRA)