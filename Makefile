CC=gcc
CFLAGS=-Wall -Werror -g -Wpointer-arith -Winit-self -Wformat-nonliteral
CXX=g++
CXXFLAGS=-Wall -Werror -g -O0

.PHONY: clean check-syntax

check-syntax:
	gcc -o /dev/null -S ${CHK_SOURCES}

clean:
	rm -rf *.o
	rm -rf *.dSYM
