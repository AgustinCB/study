#ifndef H_STACK
#define H_STACK
#include <optional>

template <class C, typename T>
class Stack {
	inline void push(T t) {
		static_cast<C*>(this)->push(t);
	}
	inline std::optional<T> pop() {
		return static_cast<C*>(this)->pop();
	}
};
#endif
