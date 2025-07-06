##
## EPITECH PROJECT, 2024
## Makefile
## File description:
## Paradigms Seminar - Second day of Haskell pool
##

GHC 	= 	ghc

NAME	= 	doop

SRC		= 	DoOp.hs

all:		$(NAME)

$(NAME):	$(SRC)
	$(GHC) -o $(NAME) $(SRC)

clean:
	rm -f *.hi
	rm -f *.o

fclean:		clean
	rm -f $(NAME)

re:			fclean all
