#include <assert.h>
#include "circular_vector.h"

inline int mod (int a, int b) {
	int ret = a % b;
	if(ret < 0)
		ret+=b;
	return ret;
}

template <typename T>
inline unsigned int CircularVector<T>::size() const {
	return _size;
}

template <typename T>
inline unsigned int CircularVector<T>::capacity() const {
	return internal.len();
}

template <typename T>
inline const T& CircularVector<T>::get(unsigned int i) const {
	assert(i < _size);
	return internal.get(mod((start+i), capacity()));
}

template <typename T>
inline T& CircularVector<T>::get_mut(unsigned int i) {
	assert(i < _size);
	return internal.get_mut(mod((start+i), capacity()));
}

template <typename T>
inline T CircularVector<T>::set(unsigned int i, T x) {
	assert(i < _size);
	T prev = get(i);
	auto index = mod((start+i), capacity());
	internal[index] = x;
	return prev;
}

template <typename T>
void CircularVector<T>::add(unsigned int i, T x) {
	if (_size + 1 > capacity()) resize();
	_size++;
	if (i < _size/2) {
		start = mod(((int) start) - 1, capacity());
		for (int j = 0; j < (int) i; j++) {
			set(j, get(j-1));
		}
	} else {
		for (int j = _size-1; j > (int) i; j--) {
			set(j, get(j-1));
		}
	}
	set(i, x);
}

template <typename T>
void CircularVector<T>::addAll(unsigned int i, Seq<CircularVector<T>, T>&& x) {
    unsigned int target_size = _size;
    while (target_size * 2 < _size + x.size()) target_size *= 2;
    if (target_size + 1 > capacity()) {
        internal = mem_manager.resize(internal, start, target_size, capacity());
    }
    for (unsigned int j = 0; j < x.size(); j++) {
        add(i+j, std::move(x.get_mut(j)));
    }
}

template <typename T>
T CircularVector<T>::remove(unsigned int i) {
	auto prev_value = get(i);
	if (i < _size/2) {
		for (unsigned int j = 0; j < i; j++) {
			set(j+1, get(j));
		}
		start = (start+1) % capacity();
	} else {
		for (unsigned int j = i; j < _size - 1; j++) {
			set(j, get(j+1));
		}
	}
	_size--;
	if (capacity() >= 3*_size) resize();
	return prev_value;
}

template <typename T>
void CircularVector<T>::resize() {
	internal = mem_manager.resize(internal, start, _size, capacity());
	start = 0;
}

template class CircularVector<unsigned int>;
