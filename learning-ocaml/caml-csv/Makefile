BIN=main
SRC=main.ml
INPUT=sample.csv

.PHONY: run
run: build
	@./$(BIN) $(INPUT)

build: $(SRC)
	@ocamlc -o $(BIN) $(SRC)
