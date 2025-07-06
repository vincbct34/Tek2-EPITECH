##
## EPITECH PROJECT, 2025
## Makefile
## File description:
## Paradigms Seminar - Second day of C pool
##

NAME   		=	libstring.a
TEST_NAME	=	unit_tests

SRC    		=   ex00.c 	\
				ex01.c 	\
				ex02.c 	\
				ex03.c 	\
				ex04.c 	\
				ex05.c 	\
				ex06.c 	\
				ex07.c 	\
				ex08.c 	\
				ex09.c 	\
				ex10.c
SRC_TEST	=   $(shell find . -wholename '*.c' -not -name 'main.c')

OBJ    		=   $(SRC:.c=.o)
OBJ_TEST	=   $(SRC_TEST:.c=.o)

CFLAGS		=	-std=gnu17 -Wall -Wextra
LDFLAGS_TEST=	-lcriterion --coverage

all:			$(NAME)

tests_run:
	gcc -o $(TEST_NAME) $(SRC_TEST) $(LDFLAGS_TEST)
	./$(TEST_NAME)

$(NAME):		$(OBJ)
	ar rc $(NAME) $(OBJ)

clean:
	rm -f $(OBJ)
	rm -f $(OBJ_TEST)
	rm -f *.gcno
	rm -f *.gcda
	rm -f tests/*.gcno
	rm -f tests/*.gcda

fclean:			clean
	rm -f $(NAME)
	rm -f $(TEST_NAME)

re:				fclean all

coverage:		tests_run
	@gcovr --exclude tests/
