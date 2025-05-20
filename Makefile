CFLAGS=-std=gnu11 -fwrapv -fno-strict-aliasing -Wall -Werror -fsanitize=undefined -fsanitize=address
CFLAGS_DEBUG=-g
CFLAGS_RELEASE=-O2 -finline-functions-called-once 

SRCS=$(wildcard src/*.c)
HEADERS=$(wildcard src/*.h)
OBJS=$(SRCS:.c=.o)

TEST_SRCS=$(wildcard test/*.c)
TESTS=$(TEST_SRCS:.c=.exe)

all: debug

debug: $(SRCS) $(HEADERS)
	$(CC) $(CFLAGS) $(CFLAGS_DEBUG) $(SRCS) -o yalispc
