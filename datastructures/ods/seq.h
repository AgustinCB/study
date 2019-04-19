#ifndef H_SEQ
#define H_SEQ

template <class C, typename T>
class Seq {
	unsigned int size() const {
		return static_cast<C*>(this)->size();
	}
	const T& get(unsigned int i) const {
		return static_cast<C*>(this)->get(i);
	}
	T set(unsigned int i, T x) {
		static_cast<C*>(this)->set(i, x);
	}
	void add(unsigned int i, T x) {
		static_cast<C*>(this)->add(i, x);
	}
	T remove(unsigned int i) {
		return static_cast<C*>(this)->remove(i);
	}
};
#endif
