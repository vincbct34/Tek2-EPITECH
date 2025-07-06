##
## EPITECH PROJECT, 2025
## Paradigms Seminar
## File description:
## Tests
##

NAME		=	unit_tests

SRC_TESTS	=	tests/*.cpp		\
				Peasant.cpp		\
				Knight.cpp		\
				Enchanter.cpp	\
				Priest.cpp		\
				Paladin.cpp

CXXFLAGS	=	-std=c++20 -W -Wall -Wextra

LDFLAGS		=	-lcriterion --coverage

all:		$(NAME)

$(NAME):
			$(CXX) -o $(NAME) $(SRC_TESTS) $(LDFLAGS)

clean:
			$(RM) *.gc*

fclean:		clean
			$(RM) $(NAME)

re:			fclean all

tests_run:	all
			./$(NAME)

coverage:	tests_run
			gcovr --exclude tests/
