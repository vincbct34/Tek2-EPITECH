##
## EPITECH PROJECT, 2025
## Interstonar [WSL: Ubuntu-24.04]
## File description:
## Makefile
##

NAME		=	stone_analysis
TEST_NAME	=	unit_tests

SRC			=	$(wildcard src/*.cpp)
MAIN_SRC	=	main.cpp
SRC_NO_MAIN	=	$(filter-out $(MAIN_SRC), $(SRC))
TEST_SRC	=	$(wildcard tests/*.cpp)

CXXFLAGS	=	-I ./include -W -Wall -Wextra -Werror -std=c++20 -pthread -lsndfile
LDFLAGS		=	-lcriterion -fprofile-arcs -ftest-coverage

OBJ			=	$(SRC:.cpp=.o)
MAIN_OBJ	=	$(MAIN_SRC:.cpp=.o)

all:		$(NAME)

$(NAME):	$(OBJ) $(MAIN_OBJ)
		$(CXX) -o $(NAME) $(OBJ) $(MAIN_OBJ) $(CXXFLAGS)

clean:
		rm -f $(OBJ) $(MAIN_OBJ) tests/*.o *.gcno *.gcda *.gcov

fclean: clean
		rm -f $(NAME) $(TEST_NAME)

re:			fclean all

tests_run:
		$(CXX) -o $(TEST_NAME) $(SRC_NO_MAIN) $(TEST_SRC) $(CXXFLAGS) $(LDFLAGS)
		./$(TEST_NAME)

coverage: 	clean tests_run
		gcovr --branches --exclude tests/
