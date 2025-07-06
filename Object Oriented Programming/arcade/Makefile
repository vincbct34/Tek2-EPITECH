##
## EPITECH PROJECT, 2025
## B-OOP-400-MPL-4-1-arcade-vincent.bichat
## File description:
## Makefile
##

RULES			=	core games

DEFAULT_RULES	=	graphicals

RULES_MACOS 	=	graphicals_macos

all: 				$(RULES) $(DEFAULT_RULES)

macos:				$(RULES) $(RULES_MACOS)

core:
					$(MAKE) -C src/core

games:
					$(MAKE) -C src/games

graphicals:
					$(MAKE) -C src/graphicals

graphicals_macos:
					$(MAKE) -C src/graphicals macos

clean:
					$(MAKE) -C src/core clean
					$(MAKE) -C src/games clean
					$(MAKE) -C src/graphicals clean

fclean:
					$(MAKE) -C src/core fclean
					$(MAKE) -C src/games fclean
					$(MAKE) -C src/graphicals fclean

re:					fclean all

doc:
					doxygen Doxyfile
					$(MAKE) -C doc/latex
					mv doc/latex/refman.pdf doc/
					mv doc/refman.pdf doc/doc.pdf
					rm -rf doc/latex doc/html

.PHONY: core games graphicals graphicals_macos clean fclean re doc
