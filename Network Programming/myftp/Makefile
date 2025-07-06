##
## EPITECH PROJECT, 2025
## myFTP
## File description:
## Makefile
##

NAME		=	myftp

SRC			=	$(wildcard src/*.cpp src/utilities/*.cpp src/commands/*.cpp main.cpp)

OBJ			=	$(SRC:.cpp=.o)

CPPFLAGS	=	-std=c++20 -W -Wall -Wextra -Werror -I./include -I./include/utilities -I./include/commands

all:		$(NAME)

$(NAME):	$(OBJ)
		$(CXX) -o $(NAME) $(OBJ) $(CPPFLAGS)

clean:
		rm -f $(OBJ)

fclean:		clean
		rm -f $(NAME)

re:			fclean all
