#ifndef H_ROOTISH_VECTOR
#define H_ROOTISH_VECTOR
#include "array.h"
#include "seq.h"
#include "vector.h"

template <typename T>
class RootishVector : public Seq<RootishVector<T>, T> {
private:
	Vector<array<T>> blocks;
	unsigned int _size = 0;
	void grow();
	void shrink();
	unsigned int index_within_block(unsigned int, unsigned int) const;
	unsigned int block_for_index(unsigned int) const;
public:
	unsigned int size() const;
	unsigned int capacity() const;
	const T& get(unsigned int i) const;
	T& get_mut(unsigned int i);
	T set(unsigned int i, T x);
	void add(unsigned int i, T x);
	T remove(unsigned int i);
};
#endif
