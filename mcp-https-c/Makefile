CC = cc
BASE_CFLAGS = -std=c17 -Wall -Wextra -Imcpc \
              -DMCPC_C23PTCH_KW1 -DMCPC_C23PTCH_CKD1 -DMCPC_C23PTCH_UCHAR1 -DMCPC_C23GIVUP_FIXENUM
CFLAGS = $(BASE_CFLAGS) -O2
LDFLAGS = mcpc/src/libmcpc.a -lpthread

# Strict flags for linting/static analysis
LINT_FLAGS = $(BASE_CFLAGS) -Wpedantic -Wshadow -Wpointer-arith \
             -Wcast-qual -Wstrict-prototypes -Wmissing-prototypes \
             -fsyntax-only

all: server

debug: CFLAGS = $(BASE_CFLAGS) -g -O0
debug: MCPC_FLAGS = DBG=1
debug: clean server

release: CFLAGS = $(BASE_CFLAGS) -O3
release: LDFLAGS += -s
release: clean server

server: main.o mcpc/src/libmcpc.a
	$(CC) -o server main.o $(LDFLAGS)

run: server
	./server

main.o: main.c
	$(CC) $(CFLAGS) -c main.c

mcpc:
	git clone https://github.com/micl2e2/mcpc

mcpc/src/libmcpc.a: | mcpc
	$(MAKE) -C mcpc $(MCPC_FLAGS)

lint:
	$(CC) $(LINT_FLAGS) main.c

test: server
	python3 test_server.py

check: lint test

format:
	clang-format -i main.c

clean:
	rm -f server main.o
	[ -d mcpc ] && $(MAKE) -C mcpc clean || true

# Submit the build to Google Cloud Build
deploy:
	echo "Submitting build to Google Cloud Build..."
	gcloud builds submit . --config cloudbuild.yaml


.PHONY: all clean lint test check format debug release run
