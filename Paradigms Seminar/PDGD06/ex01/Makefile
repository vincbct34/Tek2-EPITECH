##
## EPITECH PROJECT, 2025
## Paradigms Seminar
## File description:
## Exercise 1
##

NAME	=	MyCat

SRC		=	MyCat.cpp

OBJ		=	$(SRC:.cpp=.o)

CFLAGS	=	-std=c++20 -Wall -Wextra -Werror

all:	$(NAME)

$(NAME):	$(OBJ)
	$(CXX) -o $(NAME) $(OBJ)

clean:
	rm -f $(OBJ)

fclean:	clean
	rm -f $(NAME)

re:	fclean all
