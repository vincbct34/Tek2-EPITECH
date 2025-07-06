##
## EPITECH PROJECT, 2025
## Paradigms Seminar
## File description:
## Rush 3
##

NAME		=	MyGKrellm

SRC			=	$(wildcard src/data/*.cpp src/sfml/*.cpp src/ncurses/*.cpp main.cpp)

OBJ			=	$(SRC:%.cpp=%.o)

SFML		=	-lsfml-graphics -lsfml-window -lsfml-system

NCURSES		=	-lncurses

CXXFLAGS	=	-W -Wall -Wextra -Werror -std=c++20

all:		$(NAME)

$(NAME):	$(OBJ)
	$(CXX) -o $(NAME) $(OBJ) $(CXXFLAGS) $(SFML) $(NCURSES)

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

clean:
	$(RM) $(OBJ)

fclean:	clean
	$(RM) $(NAME)

re:	fclean all
