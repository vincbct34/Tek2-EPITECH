##
## EPITECH PROJECT, 2025
## Makefile
## File description:
## Paradigms Seminar - First day of C pool
##

NAME        =       z

SRC         =       $(wildcard src/*.c)

OBJ         =       $(SRC:.c=.o)

CFLAGS      =       -std=gnu17 -Wall -Wextra
CPPFLAGS    =       -I./include

all:                $(NAME)

$(NAME):            $(OBJ)
	$(CC) -o $(NAME) $(OBJ)

clean:
	$(RM) $(OBJ)

fclean: 			clean
	$(RM) $(NAME)

re: 				fclean all
