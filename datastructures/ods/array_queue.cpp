#include <algorithm>
#include "array.h"
#include "array_queue.h"

template <typename T>
unsigned int ArrayQueue<T>::size() {
	return internal.size();
}
template <typename T>
inline const T& ArrayQueue<T>::get(unsigned int i) {
	return internal.get(i);
}
template <typename T>
inline T& ArrayQueue<T>::get_mut(unsigned int i) {
	return internal.get_mut(i);
}
template <typename T>
inline std::optional<T> ArrayQueue<T>::set(unsigned int i, T x) {
	if (i < size()) {
		return internal.set(i, x);
	}
	return {};
}
template <typename T>
inline void ArrayQueue<T>::enqueue(T t) {
	internal.add(size(), t);
}
template <typename T>
inline std::optional<T> ArrayQueue<T>::dequeue() {
	if (size() == 0) return {};
	return internal.remove(0);
}

template class ArrayQueue<unsigned int>;
