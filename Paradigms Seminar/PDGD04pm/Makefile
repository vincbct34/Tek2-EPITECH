##
## EPITECH PROJECT, 2025
## Makefile
## File description:
## Paradigms Seminar - First day of C pool
##

TEST_NAME		=	unit_tests

SRC				=	$(wildcard *.c)
SRC_TEST		=	$(wildcard tests/*.c)

OBJ				=	$(SRC:.c=.o)
OBJ_TEST		=	$(SRC_TEST:.c=.o)

CFLAGS      	=   -W -Wall -Wextra -Werror --coverage
CPPFLAGS		=	-I./
LDFLAGS_TEST	=	-lcriterion --coverage

tests_run:              $(TEST_NAME)

$(TEST_NAME):		$(OBJ) $(OBJ_TEST)
			$(CC) -o $(TEST_NAME) $(OBJ) $(OBJ_TEST) $(LDFLAGS_TEST)
			./$(TEST_NAME)

clean:
			$(RM) $(OBJ_TEST)
			$(RM) $(OBJ)
			$(RM) *.gcno
			$(RM) *.gcda
			$(RM) tests/*.gcno
			$(RM) tests/*.gcda

fclean:			clean
			$(RM) $(TEST_NAME)

re:			fclean all

coverage:		tests_run
			@gcovr --exclude tests/
