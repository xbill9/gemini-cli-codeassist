COBC = cobc
CC = cc
BASE_CFLAGS = -std=c17 -Wall -Wextra -Imcpc/src -Imcpc -Dis_unix \
              -DMCPC_C23PTCH_KW1 -DMCPC_C23PTCH_CKD1 -DMCPC_C23PTCH_UCHAR1 -DMCPC_C23GIVUP_FIXENUM
CFLAGS = $(BASE_CFLAGS) -O2
LDFLAGS = mcpc/src/libmcpc.a -lm

# Strict flags for linting/static analysis
LINT_FLAGS = $(BASE_CFLAGS) -Wpedantic -Wshadow -Wpointer-arith \
             -Wcast-qual -Wstrict-prototypes -Wmissing-prototypes \
             -fsyntax-only

all: firestore-server

debug: CFLAGS = $(BASE_CFLAGS) -g -O0
debug: COBCFLAGS = -g -debug
debug: MCPC_FLAGS = DBG=1
debug: clean firestore-server

release: CFLAGS = $(BASE_CFLAGS) -O3
release: COBCFLAGS = -O3
release: LDFLAGS += -s
release: clean firestore-server

firestore-server: server.cob c_helpers.o cob_helpers.o firestore_client.o mcpc/src/libmcpc.a
	$(COBC) -x -free $(COBCFLAGS) -o firestore-server server.cob c_helpers.o cob_helpers.o firestore_client.o $(LDFLAGS)

c_helpers.o: c_helpers.c
	$(CC) $(CFLAGS) -c c_helpers.c

cob_helpers.o: cob_helpers.c
	$(CC) $(CFLAGS) -c cob_helpers.c

firestore_client.o: firestore_client.c
	$(CC) $(CFLAGS) -c firestore_client.c

mcpc:
	git clone https://github.com/micl2e2/mcpc

mcpc/src/libmcpc.a: | mcpc
	$(MAKE) -C mcpc/src $(MCPC_FLAGS)

lint:
	$(CC) $(LINT_FLAGS) c_helpers.c
	$(CC) $(LINT_FLAGS) cob_helpers.c
	$(CC) $(LINT_FLAGS) firestore_client.c

test: firestore-server
	python3 test_server_cobol.py

check: lint test

format:
	clang-format -i c_helpers.c

clean:
	rm -f firestore-server server.o c_helpers.o cob_helpers.o firestore_client.o server.c server.c.h server.c.l[12].h server.i
	[ -d mcpc ] && $(MAKE) -C mcpc/src clean || true

.PHONY: all clean lint check format debug release help

help:
	@echo "Available targets:"
	@echo "  all             Build the COBOL MCP server"
	@echo "  firestore-server    Build the COBOL MCP server"
	@echo "  debug           Build with debug symbols and no optimizations"
	@echo "  release         Build with optimizations and stripped binaries"
	@echo "  test            Run integration tests for the COBOL server"
	@echo "  lint            Lint C helper code"
	@echo "  check           Run linting and all tests"
	@echo "  format          Format C code using clang-format"
	@echo "  clean           Remove build artifacts"