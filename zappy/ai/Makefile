##
## EPITECH PROJECT, 2025
## Zappy
## File description:
## Makefile
##

NAME		=	zappy_ai

PYTHON_SRC	=	main.py agent.py client.py utils.py

PYTHON		=	python3

all:			$(NAME)

$(NAME):		$(PYTHON_SRC)
	@printf '#!/bin/bash\n' > $(NAME)
	@printf 'SCRIPT_DIR="$$( cd "$$( dirname "$${BASH_SOURCE[0]}" )"' \
		>> $(NAME)
	@printf ' &> /dev/null && pwd )"\n' >> $(NAME)
	@printf 'cd "$$SCRIPT_DIR"\n' >> $(NAME)
	@printf 'exec $(PYTHON) main.py "$$@"\n' >> $(NAME)
	@chmod +x $(NAME)

clean:
	@$(RM) -rf __pycache__
	@find . -name "*.pyc" -delete
	@find . -name "*.pyo" -delete
	@find . -name ".pytest_cache" -type d -exec rm -rf {} + \
		2>/dev/null || true

fclean:			clean
	@$(RM) $(NAME)

re:				fclean all

install-test-deps:
	@pip3 install -r ../tests/requirements-test.txt 2>/dev/null || \
		echo "Warning: Could not install test dependencies"

tests_run: install-test-deps
	@echo "Running AI tests..."
	@cd ../tests && PYTHONPATH=.. $(PYTHON) -m pytest test_*.py -v \
		--tb=short 2>/dev/null || \
		echo "Tests failed or pytest not available"

test-coverage: install-test-deps
	@echo "Running AI tests with coverage..."
	@cd ../tests && PYTHONPATH=.. $(PYTHON) -m pytest test_*.py --cov=.. \
		--cov-report=term-missing -v || \
		echo "Coverage tests failed or dependencies not available"

.PHONY:	all clean fclean re tests_run test-coverage install-test-deps $(NAME)
