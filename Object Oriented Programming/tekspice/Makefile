##
## EPITECH PROJECT, 2025
## NanoTekSpice
## File description:
## Makefile
##

NAME 		=	nanotekspice

SRC			=	$(shell find ./src -name "*.cpp")

OBJ 		=	$(SRC:.cpp=.o)

DEBUGFLAGS 	=	-g3

CXXFLAGS 	=	-std=c++20 -W -Wall -Wextra -Werror

CXXFLAGS 	+=	-I src/							\
				-I src/Components				\
				-I src/Components/Elementary	\
				-I src/Components/Advanced		\
				-I src/Components/Special		\
				-I src/Components/Gates			\
				-I src/Exceptions				\
				-I src/Parsing					\
				-I src/Core						\

all: 		$(NAME)

$(NAME): 	$(OBJ)
	g++ -o $(NAME) $(OBJ)


debug: 		$(OBJ)
	g++ -o $(NAME) $(OBJ) $(DEBUGFLAGS)

clean:
	rm -f $(OBJ)

fclean: 	clean
	rm -f $(NAME)

re: 		fclean all
