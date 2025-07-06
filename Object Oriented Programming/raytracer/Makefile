##
## EPITECH PROJECT, 2025
## B-OOP-400-MPL-4-1-raytracer-vincent.bichat
## File description:
## Makefile
##

NAME 		= raytracer

SRC 		= $(shell find ./src -name '*.cpp')

OBJ 		= $(SRC:.cpp=.o)

CXXFLAGS 	= -I ./src/ -I ./src/core/ -I ./src/exceptions/ -I ./src/loaders/ -I ./src/plugins/ -I ./src/plugins/math/ -I ./src/plugins/camera/ -I ./src/plugins/lights/ -I ./src/plugins/render/ -I ./src/plugins/primitives/ -std=c++20 -W -Wall -Wextra -Werror

LDLIBS   	= -lconfig++ -lsfml-graphics -lsfml-window -lsfml-system

UNAME_S 	:= $(shell uname -s)

ifeq ($(UNAME_S),Darwin)
	CXXFLAGS += -I /opt/homebrew/opt/sfml@2/include -I /opt/homebrew/include
	LDFLAGS  += -L /opt/homebrew/opt/sfml@2/lib -L /opt/homebrew/lib
endif

all: 		$(NAME) plugins

$(NAME): 	$(OBJ)
		$(CXX) -o $(NAME) $(OBJ) $(CXXFLAGS) $(LDFLAGS) $(LDLIBS)

clean:
		$(RM) -f $(OBJ)

fclean: 	clean
		$(RM) -f $(NAME)
		make -C ./src/plugins fclean

re: 		fclean all

plugins:
		make -C ./src/plugins

doc:
		doxygen Doxyfile
		$(MAKE) -C doc/latex
		mv doc/latex/refman.pdf doc/
		mv doc/refman.pdf doc/doc.pdf
		rm -rf doc/latex doc/html

.PHONY: all clean fclean re plugins doc
