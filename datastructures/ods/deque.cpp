#ifndef H_DEQUE
#define H_DEQUE
#include <optional>

template <typename T, class C>
class Deque {
public:
	inline void addFirst(T t) {
		static_cast<C*>(this)->addFirst(t);
	}

	inline std::optional<T> removeFirst() {
		return static_cast<C*>(this).removeFirst();
	}

	inline void addLast(T t) {
		static_cast<C*>(this)->addLast(t);
	}

	inline std::optional<T> removeLast() {
		return static_cast<C*>(this).removeLast();
	}
};
#endif
