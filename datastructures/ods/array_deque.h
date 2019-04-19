#ifndef H_ARRAY_DEQUE
#define H_ARRAY_DEQUE
#include <optional>
#include "deque.h"
#include "circular_vector.h"

template <typename T>
class ArrayDeque : public Deque<ArrayDeque<T>, T> {
private:
	CircularVector<T> internal;
public:
	unsigned int size();
	const T& get(unsigned int i);
	T& get_mut(unsigned int i);
	std::optional<T> set(unsigned int i, T x);
	void addFirst(T t);
	void addLast(T t);
	std::optional<T> removeFirst();
	std::optional<T> removeLast();
};
#endif
