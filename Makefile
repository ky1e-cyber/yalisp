CFLAGS=-std=gnu11 -fwrapv -Wall
CFLAGS_DEBUG=-g -fno-strict-aliasing -fsanitize=undefined -fsanitize=address
CFLAGS_RELEASE=-O2 -finline-functions-called-once

COMPILER_SRCS=$(wildcard src/compiler/*.c)
COMPILER_HEADERS=$(wildcard src/compiler/*.h)

TEST_SRCS=$(wildcard test/*.c)

all: compiler_debug

compiler_debug: $(COMPILER_SRC) $(COMPILER_HEADERS)
	$(CC) $(CFLAGS) $(CFLAGS_DEBUG) $(COMPILER_SRCS) -o yalispc

clean:
	rm -f yalispc
