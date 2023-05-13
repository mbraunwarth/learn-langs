CC=gcc
FLAGS=-Wall -Wpedantic
OUT=pong
SRC=main.c

.PHONY: run
run: build
	@./$(OUT)

build: $(SRC)
	@$(CC) $(FLAGS) -o $(OUT) $(SRC)
