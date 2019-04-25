#include <type_traits>
#include "vector.h"

template <typename T>
inline unsigned int Vector<T>::size() const {
	return _size;
}

template <typename T>
inline unsigned int Vector<T>::capacity() const {
	return internal.len();
}

template <typename T>
inline const T& Vector<T>::get(unsigned int i) const {
	return internal.get(i);
}

template <typename T>
inline T& Vector<T>::get_mut(unsigned int i) {
	return internal.get_mut(i);
}

template <typename T>
inline T Vector<T>::set(unsigned int i, T x) {
	T prev = std::move(internal.get_mut(i));
	internal[i] = std::move(x);
	return prev;
}

template <typename T>
void Vector<T>::addAll(unsigned int i, Seq<Vector<T>, T>&& x) {
    unsigned int target_size = _size;
    while (target_size * 2 < _size + x.size()) target_size *= 2;
    if (target_size + 1 > capacity()) {
        internal = mem_manager.resize(std::move(internal), target_size);
    }
	mem_manager.move_right(internal, i, _size, x.size());
    for (unsigned int j = 0; j < x.size(); j++) {
        internal[i+j] = std::move(x.get_mut(j));
    }
    _size += x.size();
}

template <typename T>
void Vector<T>::add(unsigned int i, T x) {
	if (_size + 1 > capacity()) resize();
	mem_manager.move_right(internal, i, _size);
	set(i, std::move(x));
	_size++;
}

template <typename T>
T Vector<T>::remove(unsigned int i) {
	T prev_value = std::move(get_mut(i));
	mem_manager.move_left(internal, i, _size);
	_size--;
	if (capacity() >= 3*_size) resize();
	return prev_value;
}

template <typename T>
void Vector<T>::resize() {
	internal = mem_manager.resize(std::move(internal), _size);
}

template class Vector<unsigned int>;
template class Vector<array<unsigned int>>;
