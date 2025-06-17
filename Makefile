CC=clang
CFLAGS=-std=gnu11 -fwrapv -Wall
CFLAGS_DEBUG=-g -fno-strict-aliasing -fsanitize=undefined -fsanitize=address
CFLAGS_RELEASE=-O2

BUILDDIR=build/

COMPILER_SRCS=$(wildcard src/compiler/*.c)
COMPILER_HEADERS=$(wildcard src/compiler/*.h)

RUNTIME_SRCS=$(wildcard src/runtime/*.c)
RUNTIME_HEADERS=$(wildcard src/runtime/*h)

TEST_SRCS=$(wildcard test/*.c)

all: debug

debug: compiler_debug runtime_debug

release: compiler_release runtime_release

compiler_debug: $(BUILDDIR) $(COMPILER_SRC) $(COMPILER_HEADERS)
	$(CC) $(CFLAGS) $(CFLAGS_DEBUG) $(COMPILER_SRCS) -o build/bin/yalispc

compiler_release: $(BUILDDIR) $(COMPILER_SRC) $(COMPILER_HEADERS)	
	$(CC) $(CFLAGS) $(CFLAGS_RELEASE) $(COMPILER_SRCS) -o build/bin/yalispc

runtime_debug: $(BUILDDIR) $(RUNTIME_SRCS) $(RUNTIME_HEADERS)
	$(CC) $(CFLAGS) $(CFLAGS_DEBUG) $(RUNTIME_SRCS) -c -o build/lib/runtime.o

runtime_release: $(BUILDDIR) $(RUNTIME_SRCS) $(RUNTIME_HEADERS)
	$(CC) $(CFLAGS) $(CFLAGS_RELEASE) $(RUNTIME_SRCS) -c -o build/lib/runtime.o

$(BUILDDIR):
	mkdir -p $(BUILDDIR)/bin
	mkdir -p $(BUILDDIR)/lib

clean:
	rm -rf $(BUILDDIR)
