CC = ocamlc
SRC = my_toy.ml
SRC_MLI = my_toy.mli
SRC_EXTRA = my_toy.cmi my_toy.cmo
TARGET = toy

all : $(TARGET)

$(TARGET) : $(SRC) $(SRC_MLI)
	@ $(CC) $(SRC_MLI)
	@ $(CC) -o $(TARGET) $(SRC)

$(SRC_MLI) : $(SRC)
	@ $(CC) -i $(SRC) > $(SRC_MLI)

clean :
	@ rm -f $(TARGET) $(SRC_MLI) $(SRC_EXTRA)