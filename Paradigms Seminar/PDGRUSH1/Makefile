##
## EPITECH PROJECT, 2025
## Makefile
## File description:
## Paradigms Seminar - Rush of Haskell pool
##

NAME	=	pushswap_checker

SRC 	=	$(wildcard *.hs)

all:	$(NAME)

$(NAME):	$(SRC)
	ghc -o $(NAME) $(SRC)

clean:
	rm -f *.o
	rm -f *.hi

fclean:	clean
	rm -f $(NAME)

re:	fclean all
