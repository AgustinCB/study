#include <algorithm>
#include "dual_array_deque.h"

template <typename T>
unsigned int DualArrayDeque<T>::size() {
	return front.size() + back.size();
}
template <typename T>
inline const T& DualArrayDeque<T>::get(unsigned int i) {
	if (i < front.size()) {
		return front.get(front.size() - 1 - i);
	} else {
		return back.get(i - front.size());
	}
}
template <typename T>
inline T& DualArrayDeque<T>::get_mut(unsigned int i) {
	if (i < front.size()) {
		return front.get_mut(front.size() - 1 - i);
	} else {
		return back.get_mut(i - front.size());
	}
}
template <typename T>
inline std::optional<T> DualArrayDeque<T>::set(unsigned int i, T x) {
	if (i >= size()) return {};
	if (i < front.size()) {
		return front.set(front.size() - 1 - i, x);
	} else {
		return back.set(i - front.size(), x);
	}
}
template <typename T>
inline void DualArrayDeque<T>::addLast(T t) {
	back.addLast(t);
	balance();
}
template <typename T>
inline void DualArrayDeque<T>::addFirst(T t) {
	front.addLast(t);
	balance();
}
template <typename T>
inline std::optional<T> DualArrayDeque<T>::removeLast() {
	auto ret = (back.size() > 0) ? back.removeLast() : front.removeFirst();
	balance();
	return ret;
}
template <typename T>
inline std::optional<T> DualArrayDeque<T>::removeFirst() {
	auto ret = (front.size() > 0) ? front.removeLast() : back.removeFirst();
	balance();
	return ret;
}

template <typename T>
inline void DualArrayDeque<T>::balance() {
	if (front.size()*3 > back.size() || back.size()*3 > front.size()) {
		ArrayDeque<T> new_front;
		ArrayDeque<T> new_back;
		for (int i = size()/2+1; i > 0; i--) {
			new_front.addLast(get(i-1));
		}
		for (unsigned int i = size()/2+1; i < size(); i++) {
			new_back.addLast(get(i));
		}
		front = std::move(new_front);
		back = std::move(new_back);
	}
}

template class DualArrayDeque<unsigned int>;
