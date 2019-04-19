#ifndef H_ARRAY_QUEUE
#define H_ARRAY_QUEUE
#include <optional>
#include "circular_vector.h"
#include "queue.h"

template <typename T>
class ArrayQueue : public Queue<ArrayQueue<T>, T> {
private:
	CircularVector<T> internal;
public:
	unsigned int size();
	const T& get(unsigned int i);
	T& get_mut(unsigned int i);
	std::optional<T> set(unsigned int i, T x);
	void enqueue(T);
	std::optional<T> dequeue();
};
#endif
