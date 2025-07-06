##
## EPITECH PROJECT, 2025
## Paradigms Seminar
## File description:
## Tests
##

TEST_NAME	=	unit_tests

SRC_TEST	=   $(shell find . -path './bonus' -prune -o -wholename '*.c' -not -name 'main.c' -not -name 'list.c' -print)

OBJ_TEST	=   $(SRC_TEST:.c=.o)

CFLAGS		=	-std=gnu17 -Wall -Wextra
CPPFLAGS_MACOS= -I./ -I/opt/homebrew/opt/criterion/include
LDFLAGS_TEST=	-lcriterion --coverage
LDFLAGS_MACOS= 	-L/opt/homebrew/opt/criterion/lib

all:			tests_run

tests_run:
	gcc -o $(TEST_NAME) $(SRC_TEST) $(LDFLAGS_TEST)
	./$(TEST_NAME)

tests_run_macos:
	gcc -o $(TEST_NAME) $(SRC_TEST) $(LDFLAGS_TEST) $(LDFLAGS_MACOS)
	./$(TEST_NAME)

clean:
	rm -f $(OBJ_TEST)
	rm -f *.gcno
	rm -f *.gcda
	rm -f tests/*.gcno
	rm -f tests/*.gcda

fclean:			clean
	rm -f $(TEST_NAME)

re:				fclean all

coverage:		tests_run
	@gcovr --exclude tests/

coverage_macos:	tests_run_macos
	@gcovr --exclude tests/