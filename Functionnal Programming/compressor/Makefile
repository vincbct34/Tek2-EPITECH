##
## EPITECH PROJECT, 2025
## Bootstrap Wolfram
## File description:
## Makefile
##

NAME =	imageCompressor

build: 	stack
		cp $(shell stack path --local-install-root)/bin/$(NAME) .

@all: 	build

clean:
		stack clean

fclean: clean
		stack clean --full
		rm -f $(NAME)

re: 	fclean @all

tests_run:
		stack test

stack:
		stack build
