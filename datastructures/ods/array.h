#ifndef H_ARRAY
#define H_ARRAY
#include <memory>

template <typename T>
class array {
private:
	std::unique_ptr<T[]> internal;
	unsigned int length;
public:
	array() {
		internal = std::make_unique<T[]>(0);
		length = 0;
	}
	array(unsigned int);
	T& operator[](unsigned int);
	bool operator==(const array<T> &that);
	const T& get(unsigned int) const;
	T& get_mut(unsigned int);
	unsigned int len() const;
	T* begin() { return internal.get(); }
	T* end() { return internal.get() + length; }
	T* from(unsigned int i) { return internal.get() + i; }
};
#endif
