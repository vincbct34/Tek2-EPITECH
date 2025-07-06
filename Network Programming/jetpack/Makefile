##
## EPITECH PROJECT, 2025
## Jetpack
## File description:
## Makefile
##

NAME_CLIENT	=	jetpack_client
NAME_SERVER	=	jetpack_server

SRC_CLIENT	=	$(wildcard src/client/*.cpp)
SRC_SERVER	=	$(wildcard src/server/*.cpp)
SRC_COMMON 	=	$(wildcard src/common/*.cpp)

OBJ_CLIENT	=	$(SRC_CLIENT:.cpp=.o)
OBJ_SERVER	=	$(SRC_SERVER:.cpp=.o)
OBJ_COMMON	=	$(SRC_COMMON:.cpp=.o)

CXXFLAGS	=	-W -Wall -Wextra -Werror -I./include -std=c++20
LDFLAGS		=	

GRAPHICS	=	-lsfml-graphics -lsfml-window -lsfml-system

all:	$(NAME_CLIENT) $(NAME_SERVER)

$(NAME_CLIENT):	$(OBJ_CLIENT) $(OBJ_COMMON)
	$(CXX) -o $(NAME_CLIENT) $(OBJ_CLIENT) $(OBJ_COMMON) $(CXXFLAGS) $(LDFLAGS) $(GRAPHICS)

$(NAME_SERVER):	$(OBJ_SERVER) $(OBJ_COMMON)
	$(CXX) -o $(NAME_SERVER) $(OBJ_SERVER) $(OBJ_COMMON) $(CXXFLAGS)

clean:
	$(RM) $(OBJ_CLIENT) $(OBJ_SERVER) $(OBJ_COMMON)

fclean:	clean
	$(RM) $(NAME_CLIENT) $(NAME_SERVER)

re:	fclean all

macos:
	$(MAKE) CXX=clang++ \
		LDFLAGS="-L/opt/homebrew/opt/sfml@2/lib" \
		CXXFLAGS="$(CXXFLAGS) -I/opt/homebrew/opt/sfml@2/include" \
		all