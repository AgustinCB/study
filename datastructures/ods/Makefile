GPP=g++
GPP_FLAGS=-Wall -Wextra -Werror -pedantic -std=c++17

clean:
	rm -f *.o

array.o:
	${GPP} ${GPP_FLAGS} -c array.cpp

vector.o: array.o
	${GPP} ${GPP_FLAGS} -c vector.cpp

circular_vector.o: array.o
	${GPP} ${GPP_FLAGS} -c circular_vector.cpp

rootish_vector.o: array.o vector.o
	${GPP} ${GPP_FLAGS} -c rootish_vector.cpp

array_stack.o: rootish_vector.o vector.o
	${GPP} ${GPP_FLAGS} -c array_stack.cpp

array_queue.o: circular_vector.o
	${GPP} ${GPP_FLAGS} -c array_queue.cpp

array_deque.o: circular_vector.o
	${GPP} ${GPP_FLAGS} -c array_deque.cpp

dual_array_deque.o: array_deque.o
	${GPP} ${GPP_FLAGS} -c dual_array_deque.cpp

test.o:
	${GPP} ${GPP_FLAGS} -c test.cpp

test_array: test.o array.o
	${GPP} ${GPP_FLAGS} -c test_array.cpp
	${GPP} ${GPP_FLAGS} -o test_array test.o test_array.o array.o
	./test_array
	valgrind ./test_array
	rm test_array

test_seq: test.o circular_vector.o vector.o
	${GPP} ${GPP_FLAGS} -c test_seq.cpp
	${GPP} ${GPP_FLAGS} -o test_seq test.o test_seq.o circular_vector.o vector.o array.o
	./test_seq
	valgrind ./test_seq
	rm test_seq

test_array_stack: test.o array_stack.o
	${GPP} ${GPP_FLAGS} -c test_stack.cpp
	${GPP} ${GPP_FLAGS} -o test_array_stack test.o test_stack.o array_stack.o rootish_vector.o vector.o array.o
	./test_array_stack
	valgrind ./test_array_stack
	rm test_array_stack

test_array_queue: test.o array_queue.o
	${GPP} ${GPP_FLAGS} -c test_queue.cpp
	${GPP} ${GPP_FLAGS} -o test_array_queue test.o test_queue.o array_queue.o circular_vector.o array.o
	./test_array_queue
	valgrind ./test_array_queue
	rm test_array_queue

test_deque: test.o array_deque.o dual_array_deque.o
	${GPP} ${GPP_FLAGS} -c test_deque.cpp
	${GPP} ${GPP_FLAGS} -o test_deque test.o test_deque.o array_deque.o dual_array_deque.o circular_vector.o array.o
	./test_deque
	valgrind ./test_deque
	rm test_deque

test_all: clean test_array test_seq test_array_stack test_array_queue test_deque
