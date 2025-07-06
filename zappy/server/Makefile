##
## EPITECH PROJECT, 2025
## Zappy
## File description:
## Makefile
##

NAME		=	zappy_server
TEST_NAME	=	unit_tests

MAIN_SRC	=	main.c
SRC			=	$(wildcard src/**/*.c) \
				$(wildcard src/**/**/*.c) \
				src/server_main.c
TESTS_SRC	=	$(wildcard ../tests/server/*.c)

CFLAGS		=	-I ./include \
				-I ./include/args-parsing \
				-I ./include/client \
				-I ./include/commands \
				-I ./include/game \
				-I ./include/network \
				-I ./include/utils \
				-std=gnu17 -W -Wall -Wextra -Werror

TEST_CFLAGS	=	$(CFLAGS)
LDFLAGS		=	-lcriterion	\
				-I$(shell brew --prefix criterion)/include	\
				-L$(shell brew --prefix criterion)/lib

OBJ			=	$(SRC:.c=.o)
MAIN_OBJ	=	$(MAIN_SRC:.c=.o)

all:			$(NAME)

$(NAME):		$(OBJ) $(MAIN_OBJ)
	$(CC) -o $(NAME) $(OBJ) $(MAIN_OBJ) $(CFLAGS)

clean:
	$(RM) $(OBJ) $(MAIN_OBJ)
	$(RM) ../*.gcda *.gcda
	$(RM) ../*.gcno *.gcno
	$(RM) coverage.html coverage.*.html coverage.css

clean_tests:
	@echo "\033[34m[INFO]\033[0m Cleaning test files..."
	@$(RM) $(TEST_NAME)
	@$(RM) ../*.gcda *.gcda
	@$(RM) ../*.gcno *.gcno
	@$(RM) coverage.html coverage.*.html
	@echo "\033[32m[SUCCESS]\033[0m Test cleanup complete"

fclean:	clean
	$(RM) $(NAME)
	$(RM) $(TEST_NAME)

re:	fclean all

tests_run:
	@echo "\033[34m[INFO]\033[0m Building and running unit tests..."
	@$(CC) -o $(TEST_NAME) $(TESTS_SRC) $(SRC) $(TEST_CFLAGS) $(LDFLAGS)
	@echo "\033[34m[INFO]\033[0m Executing tests..."
	@./$(TEST_NAME) --verbose && echo \
	"\033[32m[SUCCESS]\033[0m All tests passed!" || (echo \
	"\033[31m[ERROR]\033[0m Some tests failed!" && exit 1)

coverage: 		clean
	@echo "\033[34m[INFO]\033[0m Running tests with coverage analysis..."
	@command -v gcovr >/dev/null 2>&1 || (echo \
	"\033[33m[WARNING]\033[0m gcovr not found. \
	Install with: pip install gcovr" && exit 1)
	@$(CC) -o $(TEST_NAME) $(TESTS_SRC) $(SRC) $(TEST_CFLAGS) $(LDFLAGS) \
	--coverage
	@./$(TEST_NAME) >/dev/null 2>&1
	@gcovr --exclude ../tests/ --html --html-details -o coverage.html
	@echo "\033[32m[SUCCESS]\033[0m Coverage report generated: coverage.html"
	@mv *.gcda ../ 2>/dev/null || true
	@mv *.gcno ../ 2>/dev/null || true

.PHONY:			all clean fclean re tests_run coverage
