#ifndef H_VECTOR
#define H_VECTOR
#include <memory>
#include "array.h"
#include "seq.h"

template <typename T>
class Vector : public Seq<Vector<T>, T> {
private:
	template <typename T1, bool>
	struct memory_manager {
		array<T1> resize(array<T1> internal, unsigned int size) {
			array<T1> new_internal(std::max(2*size, 1U));
			for (unsigned int i = 0; i < size; i++) {
				new_internal[i] = std::move(internal[i]);
			}
			return new_internal;
		}
		void move_right(array<T1>& internal, unsigned int from, unsigned int to) {
			for (unsigned int j = to; j > from; j--) {
				internal[j] = std::move(internal[j-1]);
			}
		}
		void move_left(array<T1>& internal, unsigned int from, unsigned int to) {
			for (unsigned int j = from; j < to-1; j++) {
				internal[j] = std::move(internal[j+1]);
			}
		}
	};
	template <typename T1>
	struct memory_manager<T1, true> {
		array<T1> resize(array<T1> internal, unsigned int size) {
			array<T> new_internal(std::max(2*size, 1U));
			std::copy(internal.begin(), internal.from(size), new_internal.begin());
			return new_internal;
		}
		void move_right(array<T1>& internal, unsigned int from, unsigned int to) {
			std::copy_backward(internal.from(from), internal.from(to), internal.from(to+1));
		}
		void move_left(array<T1>& internal, unsigned int from, unsigned int to) {
			std::copy(internal.from(from), internal.from(to-1), internal.from(from+1));
		}
	};
	array<T> internal;
	memory_manager<T, std::is_copy_assignable<T>::value> mem_manager;
	unsigned int _size = 0;
	void resize();
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
