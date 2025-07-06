##
## EPITECH PROJECT, 2025
## Zappy
## File description:
## Makefile
##

NAME_SERVER	=	zappy_server
DIR_SERVER	=	server

NAME_AI		=	zappy_ai
DIR_AI		=	ai

NAME_GUI	=	zappy_gui
DIR_GUI		=	gui

all:			$(NAME_SERVER) $(NAME_GUI) $(NAME_AI)

$(NAME_SERVER):
	$(MAKE) -C $(DIR_SERVER)
	mv $(DIR_SERVER)/$(NAME_SERVER) .

$(NAME_AI):
	$(MAKE) -C $(DIR_AI)
	mv $(DIR_AI)/$(NAME_AI) .

$(NAME_GUI):
	$(MAKE) -C $(DIR_GUI)
	mv $(DIR_GUI)/$(NAME_GUI) .

clean:
	$(MAKE) clean -C $(DIR_SERVER)
	$(MAKE) clean -C $(DIR_GUI)
	$(MAKE) clean -C $(DIR_AI)

fclean:			clean
	$(MAKE) fclean -C $(DIR_SERVER)
	@$(RM) $(NAME_SERVER)
	$(MAKE) fclean -C $(DIR_GUI)
	@$(RM) $(NAME_GUI)
	$(MAKE) fclean -C $(DIR_AI)
	@$(RM) $(NAME_AI)

re:				fclean all

tests_run:
	$(MAKE) tests_run -C $(DIR_SERVER)
	$(MAKE) tests_run -C $(DIR_GUI)
	$(MAKE) tests_run -C $(DIR_AI)

coverage:
	$(MAKE) coverage -C $(DIR_SERVER)
	$(MAKE) coverage -C $(DIR_GUI)
	$(MAKE) coverage -C $(DIR_AI)

doc-server:
	doxygen ServerDoxyfile
	$(MAKE) -C doc/server/latex
	mv doc/server/latex/refman.pdf doc/server/
	mv doc/server/refman.pdf doc/server/doc.pdf
	rm -rf doc/server/latex doc/server/html

doc-ai:
	doxygen AIDoxyfile
	$(MAKE) -C doc/ai/latex
	mv doc/ai/latex/refman.pdf doc/ai/
	mv doc/ai/refman.pdf doc/ai/doc.pdf
	rm -rf doc/ai/latex doc/ai/html

doc-gui:
	doxygen GUIDoxyfile
	$(MAKE) -C doc/gui/latex
	mv doc/gui/latex/refman.pdf doc/gui/
	mv doc/gui/refman.pdf doc/gui/doc.pdf
	rm -rf doc/gui/latex doc/gui/html

doc: doc-server doc-ai doc-gui

.PHONY: all clean fclean re tests_run coverage doc-server doc-ai doc-gui doc \
		$(NAME_SERVER) $(NAME_GUI) $(NAME_AI)
