# Makefile for firestore-https-lisp

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
	@$(LISP) $(LISP_FLAGS) \
		--load $(HOME)/quicklisp/setup.lisp \
		--load firestore-https-lisp.asd \
		--eval '(ql:quickload :firestore-https-lisp)' \
		--eval '(sb-ext:save-lisp-and-die "$(APP_NAME)" :executable t :toplevel (quote mcp-server:main))'
	@if [ ! -s $(APP_NAME) ]; then echo "Error: $(APP_NAME) is empty or not created"; exit 1; fi
	@chmod +x $(APP_NAME)

# Target to run the application (dev mode)
run:
	@$(LISP) $(LISP_FLAGS) \
		--load $(HOME)/quicklisp/setup.lisp \
		--load firestore-https-lisp.asd \
		--eval '(let ((*standard-output* *error-output*)) (ql:quickload :firestore-https-lisp))' \
		--eval '(mcp-server:main)' \
		--quit

# Target to run tests
test:
	@$(LISP) $(LISP_FLAGS) \
		--load $(HOME)/quicklisp/setup.lisp \
		--load firestore-https-lisp.asd \
		--eval '(let ((*standard-output* *error-output*)) (ql:quickload :firestore-https-lisp/tests))' \
		--eval '(asdf:test-system :firestore-https-lisp)' \
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
		--load firestore-https-lisp.asd \
		--eval '(let ((*standard-output* *error-output*)) (ql:quickload :firestore-https-lisp))' \
		--eval '(mcp-server:main)'

clean:
	@echo "Cleaning up..."
	@rm -f $(APP_NAME)
	@rm -f *.fasl

deploy:
	echo "Submitting build to Google Cloud Build..."
	gcloud builds submit . --config cloudbuild.yaml


