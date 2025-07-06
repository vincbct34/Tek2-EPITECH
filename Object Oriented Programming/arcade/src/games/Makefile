##
## EPITECH PROJECT, 2025
## Arcade
## File description:
## Makefile
##

LIBS			=	arcade_menu.so arcade_snake.so arcade_centipede.so

CXXFLAGS		=	-W -Wall -Wextra -Werror -std=c++20 -I ../../include/core -I ../../include/graphicals -I ../../include/games -I ../../include/utils

LDFLAGS			=	-fPIC -shared

all:				$(LIBS)
				mv $(LIBS) ../../lib/

arcade_menu.so: 	$(wildcard ./menu/*.cpp) $(wildcard ../utils/*.cpp)
				$(CXX) $(CXXFLAGS) $(LDFLAGS) -o $@ $^

arcade_snake.so: 	$(wildcard ./snake/*.cpp) $(wildcard ../utils/*.cpp)
				$(CXX) $(CXXFLAGS) $(LDFLAGS) -o $@ $^

arcade_centipede.so: 	$(wildcard ./centipede/*.cpp) $(wildcard ../utils/*.cpp)
				$(CXX) $(CXXFLAGS) $(LDFLAGS) -o $@ $^

clean:
				for lib in $(LIBS); do \
					$(RM) ../../lib/$$lib; \
				done

fclean:				clean

re:					fclean all
