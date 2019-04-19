#ifndef H_ARRAY_STACK
#define H_ARRAY_STACK
#include <optional>
#include "stack.h"
#include "vector.h"

template <typename T, class V = Vector<T>>
class ArrayStack : public Stack<ArrayStack<T, V>, T> {
private:
	V internal;
	unsigned int n = 0;
public:
	unsigned int size();
	const T& get(unsigned int i);
	T& get_mut(unsigned int i);
	std::optional<T> set(unsigned int i, T x);
	void push(T);
	std::optional<T> pop();
};
#endif
