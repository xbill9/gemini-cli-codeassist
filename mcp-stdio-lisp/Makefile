# Makefile for mcp-stdio-lisp

APP_NAME = mcp-server
LISP = sbcl
LISP_FLAGS = --noinform --disable-ldb --disable-debugger

.PHONY: all deps run build clean

all: build

# Target to install dependencies
deps:
	@./setup.lisp

# Target to build the binary
build:
	@echo "Building $(APP_NAME)..."
	@$(LISP) $(LISP_FLAGS) --load build.lisp

# Target to run the application (dev mode)
run:
	@$(LISP) $(LISP_FLAGS) \
		--load $(HOME)/quicklisp/setup.lisp \
		--load mcp-stdio-lisp.asd \
		--eval '(let ((*standard-output* *error-output*)) (ql:quickload :mcp-stdio-lisp))' \
		--eval '(mcp-server:main)' \
		--quit

# Target to run tests
test:
	@$(LISP) $(LISP_FLAGS) \
		--load $(HOME)/quicklisp/setup.lisp \
		--load mcp-stdio-lisp.asd \
		--eval '(let ((*standard-output* *error-output*)) (ql:quickload :mcp-stdio-lisp/tests))' \
		--eval '(asdf:test-system :mcp-stdio-lisp)' \
		--quit

# Target to run diagnostics and debug
debug:
	@echo "Running diagnostics..."
	@$(LISP) $(LISP_FLAGS) --load debug-collections.lisp
	@$(LISP) $(LISP_FLAGS) --load debug-lambda.lisp
	@$(LISP) $(LISP_FLAGS) --load debug-server-val.lisp
	@echo "Running $(APP_NAME) in debug mode..."
	@$(LISP) \
		--load $(HOME)/quicklisp/setup.lisp \
		--load mcp-stdio-lisp.asd \
		--eval '(let ((*standard-output* *error-output*)) (ql:quickload :mcp-stdio-lisp))' \
		--eval '(mcp-server:main)'

clean:
	@echo "Cleaning up..."
	@rm -f $(APP_NAME)
	@rm -f *.fasl