#ifndef H_QUEUE
#define H_QUEUE
#include <optional>

template <typename T, class C>
class Queue {
public:
	inline void enqueue(T t) {
		static_cast<C*>(this)->enqueue(t);
	}

	inline std::optional<T> dequeue() {
		return static_cast<C*>(this).dequeue();
	}
};
#endif
