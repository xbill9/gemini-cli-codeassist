CC = cc
FC = gfortran
BASE_CFLAGS = -std=c17 -Wall -Wextra -Imcpc \
              -DMCPC_C23PTCH_KW1 -DMCPC_C23PTCH_CKD1 -DMCPC_C23PTCH_UCHAR1 -DMCPC_C23GIVUP_FIXENUM
CFLAGS = $(BASE_CFLAGS) -O2
FFLAGS = -O2
LDFLAGS = mcpc/src/libmcpc.a -lpthread

# Strict flags for linting/static analysis
LINT_FLAGS = $(BASE_CFLAGS) -Wpedantic -Wshadow -Wpointer-arith \
             -Wcast-qual -Wstrict-prototypes -Wmissing-prototypes \
             -fsyntax-only

all: server-fortran

debug: CFLAGS = $(BASE_CFLAGS) -g -O0
debug: FFLAGS = -g -O0
debug: MCPC_FLAGS = DBG=1
debug: clean server-fortran

release: CFLAGS = $(BASE_CFLAGS) -O3
release: FFLAGS = -O3
release: LDFLAGS += -s
release: clean server-fortran

server-fortran: server.o c_helpers.o mcpc/src/libmcpc.a
	$(FC) -o server-fortran server.o c_helpers.o $(LDFLAGS)

server.o: server.f90
	$(FC) $(FFLAGS) -c server.f90

c_helpers.o: c_helpers.c
	$(CC) $(CFLAGS) -c c_helpers.c

mcpc:
	git clone https://github.com/micl2e2/mcpc

mcpc/src/libmcpc.a: | mcpc
	$(MAKE) -C mcpc $(MCPC_FLAGS)

lint:
	$(CC) $(LINT_FLAGS) c_helpers.c

test: server-fortran
	python3 test_server_fortran.py

check: lint test

format:
	clang-format -i c_helpers.c

clean:
	rm -f server-fortran server.o c_helpers.o
	[ -d mcpc ] && $(MAKE) -C mcpc clean || true

deploy:
	echo "Submitting build to Google Cloud Build..."
	gcloud builds submit . --config cloudbuild.yaml



.PHONY: all clean lint check format debug release
