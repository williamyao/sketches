CFLAGS=-Wall -g -pedantic -ansi

check-syntax:
	gcc -o /dev/null -S ${CHK_SOURCES}
