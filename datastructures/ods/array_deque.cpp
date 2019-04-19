#include <algorithm>
#include "array_deque.h"

template <typename T>
unsigned int ArrayDeque<T>::size() {
	return internal.size();
}
template <typename T>
inline const T& ArrayDeque<T>::get(unsigned int i) {
	return internal.get(i);
}
template <typename T>
inline T& ArrayDeque<T>::get_mut(unsigned int i) {
	return internal.get_mut(i);
}
template <typename T>
inline std::optional<T> ArrayDeque<T>::set(unsigned int i, T x) {
	if (i < size()) {
		return internal.set(i, x);
	}
	return {};
}
template <typename T>
inline void ArrayDeque<T>::addLast(T t) {
	internal.add(size(), t);
}
template <typename T>
inline void ArrayDeque<T>::addFirst(T t) {
	internal.add(0, t);
}
template <typename T>
inline std::optional<T> ArrayDeque<T>::removeLast() {
	if (size() == 0) return {};
	return internal.remove(size()-1);
}
template <typename T>
inline std::optional<T> ArrayDeque<T>::removeFirst() {
	if (size() == 0) return {};
	return internal.remove(0);
}

template class ArrayDeque<unsigned int>;
