##
## EPITECH PROJECT, 2025
## Arcade
## File description:
## Makefile
##

NAME	=	arcade

SRC 	=	$(wildcard ./*.cpp ../utils/*.cpp ../../main.cpp)

OBJ		=	$(SRC:.cpp=.o)

CXXFLAGS=	-W -Wall -Wextra -Werror -std=c++20 -I ../../include/core -I ../../include/graphicals -I ../../include/games -I ../../include/utils

LDFLAGS	=	-ldl -fno-gnu-unique

all:		$(NAME)
		mv $(NAME) ../../
		@mkdir -p ../../lib

$(NAME):	$(OBJ)
		$(CXX) -o $(NAME) $(OBJ) $(LDFLAGS)

clean:
		$(RM) $(OBJ)

fclean:		clean
		$(RM) ../../$(NAME)

re:			fclean all
