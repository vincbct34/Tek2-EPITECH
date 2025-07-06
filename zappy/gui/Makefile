##
## EPITECH PROJECT, 2025
## Zappy
## File description:
## Makefile
##

NAME		=	zappy_gui

TEST_NAME	=	unit_tests

MAIN_SRC	=	main.cpp
SRC			=	$(wildcard src/**/*.cpp)

CXXFLAGS	=	-I ./src -std=c++20 -W -Wall -Wextra -Werror

UNAME_S 	:= $(shell uname -s)

LDLIBS		=	-lraylib -lGL -lm -lpthread -ldl -lrt -lX11

LD_TEST_FLAGS		=	-lcriterion -fprofile-arcs -ftest-coverage
SRC_NO_MAIN	=	$(filter-out $(MAIN_SRC), $(SRC))
TEST_SRC	=	$(wildcard ../tests/*.cpp)

ifeq ($(UNAME_S),Darwin)
    CXXFLAGS += -I /opt/homebrew/opt/raylib/include -I /opt/homebrew/include
    LDFLAGS  += -L /opt/homebrew/opt/raylib/lib -L /opt/homebrew/lib
    LDLIBS   += -framework OpenGL -framework Cocoa -framework IOKit
	LDLIBS   += -framework CoreVideo
endif

OBJ			=	$(SRC:.cpp=.o)
MAIN_OBJ	=	$(MAIN_SRC:.cpp=.o)

all:			$(NAME)

$(NAME):		$(OBJ) $(MAIN_OBJ)
	$(CXX) -o $(NAME) $(OBJ) $(MAIN_OBJ) $(CXXFLAGS) $(LDFLAGS) $(LDLIBS)

clean:
	@$(RM) $(OBJ) $(MAIN_OBJ) *.gcno *.gcda *.gcov

fclean:			clean
	@$(RM) $(NAME)
	@$(RM) $(TEST_NAME)

re:				fclean all

tests_run:
	$(CXX) -o $(TEST_NAME) $(SRC_NO_MAIN) $(TEST_SRC) $(CXXFLAGS) \
	$(LD_TEST_FLAGS) $(LDLIBS)
	./$(TEST_NAME)

coverage: 	clean tests_run
	gcovr --branches --exclude tests/

.PHONY:			all clean fclean re tests_run $(NAME)
