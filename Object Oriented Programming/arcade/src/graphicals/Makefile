##
## EPITECH PROJECT, 2025
## Arcade
## File description:
## Makefile
##

LIBS			=	arcade_ncurses.so arcade_sfml.so arcade_sdl2.so

LIBS_MAC		=	arcade_ncurses.so sfml_macos.so sdl_macos.so

CXXFLAGS		=	-W -Wall -Wextra -Werror -std=c++20 -I ../../include/core -I ../../include/graphicals -I ../../include/games -I ../../include/utils

LDFLAGS			=	-fPIC -shared

all:				$(LIBS)
				mv $(LIBS) ../../lib/

macos:				$(LIBS_MAC)
				mv $(LIBS_MAC) ../../lib/

arcade_ncurses.so: 	$(wildcard ./ncurses/*.cpp) $(wildcard ../utils/*.cpp)
				$(CXX) $(CXXFLAGS) $(LDFLAGS) -o $@ $^ -lncurses

arcade_sfml.so: 	$(wildcard ./sfml/*.cpp) $(wildcard ../utils/*.cpp)
				$(CXX) $(CXXFLAGS) $(LDFLAGS) -o $@ $^ -lsfml-graphics -lsfml-window -lsfml-system

arcade_sdl2.so: 	$(wildcard ./sdl/*.cpp) $(wildcard ../utils/*.cpp)
				$(CXX) $(CXXFLAGS) $(LDFLAGS) -o $@ $^ -lSDL2 -lSDL2_image -lSDL2_ttf

clean:
				for lib in $(LIBS) $(LIBS_MAC); do \
					$(RM) ../../lib/$$lib; \
				done

fclean:				clean

re:					fclean all


## For SFML on MacOS

SFML_PATH 			= 	$(shell brew --prefix sfml@2)

SDL_PATHS 			= 	$(shell brew --prefix sdl2) $(shell brew --prefix sdl2_image) 		\
						$(shell brew --prefix sdl2_ttf) $(shell brew --prefix sdl2_mixer)

INCLUDE_PATHS		=	-I ../../include/core -I ../../include/graphicals -I ../../include/games -I ../../include/utils

CXXFLAGS_MAC_SFML 	= 	-fPIC -shared -std=c++20 -I$(SFML_PATH)/include $(INCLUDE_PATHS)

LDFLAGS_MAC_SFML 	= 	-L$(SFML_PATH)/lib -lsfml-graphics -lsfml-window -lsfml-system

CXXFLAGS_MAC_SDL 	= 	-fPIC -shared -std=c++20 $(INCLUDE_PATHS) 				\
				   		$(foreach P,$(SDL_PATHS),-I$P/include -I$P/include/SDL2)

LDFLAGS_MAC_SDL 	= 	(foreach P,$(SDL_PATHS),-L$P/lib) 			\
				  		-lSDL2 -lSDL2_image -lSDL2_ttf -lSDL2_mixer

sfml_macos.so:			$(wildcard ./sfml/*.cpp) $(wildcard ../utils/*.cpp)
					$(CXX) $(CXXFLAGS_MAC_SFML) -o $@ $^ $(LDFLAGS_MAC_SFML)

sdl_macos.so:			$(wildcard ./sdl/*.cpp) $(wildcard ../utils/*.cpp)
					$(CXX) $(CXXFLAGS_MAC_SDL) -o $@ $^ $(LDFLAGS_MAC_SDL)