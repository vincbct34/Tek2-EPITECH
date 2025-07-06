##
## EPITECH PROJECT, 2025
## Interstonar [WSL: Ubuntu-24.04]
## File description:
## Makefile
##

NAME		=	interstonar

SRC			=	$(wildcard src/*.cpp)	\
				main.cpp

OBJ			=	$(SRC:.cpp=.o)

CXXFLAGS	=	-I ./include -I ./include/libs/ -W -Wall -Wextra -Werror -std=c++20

all:		$(NAME)

$(NAME):	$(OBJ)
		$(CXX) -o $(NAME) $(OBJ) $(CXXFLAGS)

clean:
		rm -f $(OBJ)

fclean:		clean
		rm -f $(NAME)

re:			fclean all
