#include "array.h"
#include <assert.h>

template <typename T>
array<T>::array(unsigned int l) {
	length = l;
	internal = std::make_unique<T[]>(l);
}

template <typename T>
inline T& array<T>::operator[](unsigned int i) {
	return get_mut(i);
}

template <typename T>
inline bool array<T>::operator==(const array<T> &that) {
	return length == that.length && internal == that.internal;
}

template <typename T>
inline const T& array<T>::get(unsigned int i) const {
	assert(i < length);
	return internal[i];
}

template <typename T>
inline T& array<T>::get_mut(unsigned int i) {
	assert(i < length);
	return internal[i];
}

template <typename T>
inline unsigned int array<T>::len() const {
	return length;
}

template class array<unsigned int>;
template class array<array<unsigned int>>;
