#include <algorithm>
#include "array_stack.h"
#include "rootish_vector.h"

template <typename T, class V>
unsigned int ArrayStack<T, V>::size() {
	return n;
}
template <typename T, class V>
inline const T& ArrayStack<T, V>::get(unsigned int i) {
	return internal.get(i);
}
template <typename T, class V>
inline T& ArrayStack<T, V>::get_mut(unsigned int i) {
	return internal.get_mut(i);
}
template <typename T, class V>
inline std::optional<T> ArrayStack<T, V>::set(unsigned int i, T x) {
	if (i < n) {
		return internal.set(i, x);
	}
	return {};
}
template <typename T, class V>
inline void ArrayStack<T, V>::push(T t) {
	internal.add(n, t);
	n++;
}
template <typename T, class V>
inline std::optional<T> ArrayStack<T, V>::pop() {
	if (n == 0) return {};
	n--;
	return internal.remove(n);
}

template class ArrayStack<unsigned int>;
template class ArrayStack<unsigned int, RootishVector<unsigned int>>;
template class ArrayStack<unsigned int, RootishVector<unsigned int, false>>;
