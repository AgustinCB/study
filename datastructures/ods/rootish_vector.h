#ifndef H_ROOTISH_VECTOR
#define H_ROOTISH_VECTOR
#include <cassert>
#include "array.h"
#include "cmath"
#include "seq.h"
#include "vector.h"

template <bool>
struct SqrtManager {
    inline float do_sqrt(uint32_t v) const {
        return sqrt(v);
    }
    inline void initsqrtab(unsigned int) const {}
};

template <>
struct SqrtManager<false> {
    unsigned int current_n = 0;
    unsigned int current_r = 0;
    array<unsigned int> sqrtab;

    inline float do_sqrt(uint32_t v) const {
        assert(v <= current_n);
        unsigned int rp = log(v);
        unsigned int upgrade = current_r - rp;
        unsigned int xp = v << upgrade;
        int s = sqrtab.get((xp>>(current_r/2+1))) >> (upgrade/2);
        while ((s+1)*(s+1) <= v) s++;
        return (float)sqrt(v);
    }
    inline void initsqrtab(unsigned int n) {
        current_r = log(n);
        current_n = n;
        auto half_r = current_r/2;
        sqrtab = array<unsigned int>(1<<(half_r+1));
        unsigned int s = 1<<(current_r/4);
        for (unsigned int j = 0; j < sqrtab.len(); j++) {
            if ((s+1)*(s+1) <= j << half_r) s++;
            sqrtab[j] = s;
        }
    }
    /*
     * See https://stackoverflow.com/a/31718095
     */
    inline unsigned char log(uint32_t v) const {
        static unsigned char pos[32] = {
            0, 9, 1, 10, 13, 21, 2, 29, 11, 14, 16, 18, 22, 25, 3, 30,
            8, 12, 20, 28, 15, 17, 24, 7, 19, 27, 23, 6, 26, 5, 4, 31
        };
        v |= v >> 1;
        v |= v >> 2;
        v |= v >> 4;
        v |= v >> 8;
        v |= v >> 16;
        return pos[(v * 0x07C4ACDDU) >> 27];
    }
};

template <typename T, bool S = true>
class RootishVector : public Seq<RootishVector<T>, T> {
private:
	Vector<array<T>> blocks;
    SqrtManager<S> sqrt_manager;
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
