#ifndef H_SEQ
#define H_SEQ

template <class C, typename T>
class Seq {
public:
	unsigned int size() const {
		return static_cast<const C*>(this)->size();
	}
	const T& get(unsigned int i) const {
		return static_cast<const C*>(this)->get(i);
	}
	T& get_mut(unsigned int i) {
		return static_cast<C*>(this)->get_mut(i);
	}
	T set(unsigned int i, T x) {
		static_cast<C*>(this)->set(i, x);
	}
	void add(unsigned int i, T x) {
		static_cast<C*>(this)->add(i, x);
	}
    template <typename C1>
    void addAll(unsigned int i, Seq<C1, T>&& x) {
        static_cast<C*>(this)->addAll(i, x);
    }
	T remove(unsigned int i) {
		return static_cast<C*>(this)->remove(i);
	}
};
#endif
