OUT=sphere
CC=gcc
FLAGS=-Wall -Wpedantic

PHONY: run
run: build
	@./$(OUT)

build: main.c
	@$(CC) $(FLAGS) -o $(OUT) main.c

clean:
	@rm $(OUT)
