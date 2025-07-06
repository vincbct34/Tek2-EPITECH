##
## EPITECH PROJECT, 2025
## B-OOP-400-MPL-4-1-raytracer-vincent.bichat
## File description:
## Makefile
##

DEPENDECIES	=			camera.so lights.so render.so

PLUGINS		=			raytracer_sphere.so camera.so lights.so render.so raytracer_cylinder.so raytracer_cone.so raytracer_plane.so raytracer_cube.so

CXXFLAGS 	= 			-I ./../../src/ -I ./../../src/core/ -I ./../../src/exceptions/ -I ./../../src/loaders/ -I ./../../src/plugins/ -I ./../../src/plugins/math/ -I ./../../src/plugins/camera/ -I ./../../src/plugins/lights/ -I ./../../src/plugins/render/ -I ./../../src/plugins/primitives/ -std=c++20 -W -Wall -Wextra -Werror

LDFLAGS		=			-fPIC -shared

all:					$(DEPENDECIES) $(PLUGINS)
					mkdir -p ../../plugins/
					mv $(PLUGINS) ../../plugins/

camera.so:				$(wildcard ./camera/*.cpp) $(wildcard ./math/*.cpp) $(wildcard ./primitives/Ray.cpp)
					$(CXX) $(CXXFLAGS) $(LDFLAGS) -o $@ $^

lights.so:				$(wildcard ./lights/*.cpp) $(wildcard ./math/*.cpp)
					$(CXX) $(CXXFLAGS) $(LDFLAGS) -o $@ $^

render.so:				$(wildcard ./render/*.cpp) $(wildcard ./math/*.cpp) ./primitives/Ray.cpp ./primitives/Sphere.cpp ./primitives/Cone.cpp
					$(CXX) $(CXXFLAGS) $(LDFLAGS) -o $@ $^

raytracer_sphere.so:	./primitives/Sphere.cpp $(wildcard ./math/*.cpp)
					$(CXX) $(CXXFLAGS) $(LDFLAGS) -o $@ $^

raytracer_plane.so:		./primitives/Plane.cpp $(wildcard ./math/*.cpp)
					$(CXX) $(CXXFLAGS) $(LDFLAGS) -o $@ $^

raytracer_cylinder.so:	./primitives/Cylinder.cpp $(wildcard ./math/*.cpp)
					$(CXX) $(CXXFLAGS) $(LDFLAGS) -o $@ $^

raytracer_cone.so:		./primitives/Cone.cpp $(wildcard ./math/*.cpp)
					$(CXX) $(CXXFLAGS) $(LDFLAGS) -o $@ $^

raytracer_cube.so:		./primitives/Cube.cpp $(wildcard ./math/*.cpp)
					$(CXX) $(CXXFLAGS) $(LDFLAGS) -o $@ $^

fclean:
					for i in $(DEPENDECIES); do \
						$(RM) $$i; \
					done
					for i in $(PLUGINS); do \
						$(RM) ../../plugins/$$i; \
					done