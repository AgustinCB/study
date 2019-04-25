#ifndef H_CIRCULAR_VECTOR
#define H_CIRCULAR_VECTOR
#include "array.h"
#include "seq.h"

template <typename T>
class CircularVector : public Seq<CircularVector<T>, T> {
private:
	template <typename T1, bool>
	struct memory_manager {
		array<T1> resize(array<T1>& internal, unsigned int start, unsigned int size, unsigned int capacity) {
			array<T> new_internal(std::max(2*size, 1U));
			for (unsigned int i = 0; i < size; i++) {
				new_internal[i] = std::move(internal[(i+start) % capacity]);
			}
			return new_internal;
		}
	};
	template <typename T1>
	struct memory_manager<T1, true> {
		array<T1> resize(array<T1>& internal, unsigned int start, unsigned int size, unsigned int capacity) {
			array<T> new_internal(std::max(2*size, 1U));
			auto first_copy_to = std::min(start+size, capacity);
			auto second_copy_to = std::min(size, capacity)-(first_copy_to-start);
			std::copy(internal.from(start), internal.from(first_copy_to), new_internal.begin());
			std::copy(internal.begin(), internal.from(second_copy_to), new_internal.from(size-start));
			return new_internal;
		}
	};
	array<T> internal;
	memory_manager<T, std::is_copy_assignable<T>::value> mem_manager;
	unsigned int _size = 0;
	unsigned int start = 0;
	void resize();
public:
	unsigned int size() const;
	unsigned int capacity() const;
	const T& get(unsigned int) const;
	T& get_mut(unsigned int);
	T set(unsigned int, T);
	void add(unsigned int, T);
    void addAll(unsigned int i, Seq<CircularVector<T>, T>&& x);
	T remove(unsigned int);
};
#endif
