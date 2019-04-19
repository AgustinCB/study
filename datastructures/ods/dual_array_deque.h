#ifndef H_DUAL_ARRAY_DEQUE
#define H_DUAL_ARRAY_DEQUE
#include <optional>
#include "array_deque.h"
#include "deque.h"

template <typename T>
class DualArrayDeque : public Deque<DualArrayDeque<T>, T> {
private:
	ArrayDeque<T> back;
	ArrayDeque<T> front;
	void balance();
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
