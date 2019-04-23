#include <assert.h>
#include <cmath>
#include "rootish_vector.h"

inline unsigned int blocks_capacity(unsigned int b) {
	return b * (b + 1) / 2;
}

template <typename T, bool C>
inline unsigned int RootishVector<T, C>::size() const {
	return _size;
}

template <typename T, bool C>
inline unsigned int RootishVector<T, C>::capacity() const {
	unsigned int capacity = 0;
	for (unsigned int j = 0; j < blocks.size(); j++) {
		capacity += blocks.get(j).len();
	}
	return capacity;
}

template <typename T, bool C>
const T& RootishVector<T, C>::get(unsigned int i) const {
	assert(i < _size);
	unsigned int block = block_for_index(i);
	unsigned int block_index = index_within_block(i, block);
	return blocks.get(block).get(block_index);
}

template <typename T, bool C>
T& RootishVector<T, C>::get_mut(unsigned int i) {
	assert(i < _size);
	unsigned int block = block_for_index(i);
	unsigned int block_index = index_within_block(i, block);
	return blocks.get_mut(block).get_mut(block_index);
}

template <typename T, bool C>
T RootishVector<T, C>::set(unsigned int i, T x) {
	assert(i < _size);
	T prev = get(i);
	get_mut(i) = x;
	return prev;
}

template <typename T, bool C>
void RootishVector<T, C>::add(unsigned int i, T x) {
	if (blocks_capacity(blocks.size()) < _size + 1) grow();
	_size ++;
	for (int j = _size-1; j > (int) i; j --) {
		set(j, get(j-1));
	}
	set(i, x);
}

template <typename T, bool C>
T RootishVector<T, C>::remove(unsigned int i) {
	T x = get(i);
	for (unsigned int j = i; i < _size-1; i++) {
		set(j, get(j+1));
	}
	_size--;
	if (blocks_capacity(blocks.size() - 2) >= _size) shrink();
	return x;
}

template <typename T, bool C>
void RootishVector<T, C>::grow() {
	array<T> new_block(blocks.size() + 1);
	blocks.add(blocks.size(), std::move(new_block));
    sqrt_manager.initsqrtab(9 + 8 * blocks_capacity(blocks.size()));
}

template <typename T, bool C>
void RootishVector<T, C>::shrink() {
	while (blocks.size() > 0 && blocks_capacity(blocks.size() - 2) >= _size) {
		blocks.remove(blocks.size() - 1);
	}
}

template <typename T, bool C>
inline unsigned int RootishVector<T, C>::index_within_block(unsigned int index, unsigned int block) const {
	return index - blocks_capacity(block);
}

template <typename T, bool C>
inline unsigned int RootishVector<T, C>::block_for_index(unsigned int index) const {
	return (unsigned int) ceil(
		(-3.0 + sqrt_manager.do_sqrt(9 + 8 * index)) / 2.0
	);
}

template class RootishVector<unsigned int>;
template class RootishVector<unsigned int, false>;
