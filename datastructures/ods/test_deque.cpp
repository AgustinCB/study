#include <optional>
#include "array_deque.h"
#include "catch.h"
#include "dual_array_deque.h"

TEST_CASE("Test array deque") {
	ArrayDeque<unsigned int> deque;

	SECTION("Test addLast function") {
		for (unsigned int j = 0; j < 8; j++) {
			deque.addLast(j);
		}
		for (unsigned int j = 0; j < 8; j++) {
			REQUIRE(deque.get(j) == j);
		}
	}
	SECTION("Test removeLast function") {
		for (unsigned int j = 0; j < 8; j++) {
			deque.addLast(j);
		}
		for (unsigned int j = 0; j < 8; j++) {
			REQUIRE(deque.removeLast().value() == 8-1-j);
		}
		REQUIRE(deque.removeLast() == std::nullopt);
	}

	SECTION("Test addFirst function") {
		for (unsigned int j = 0; j < 8; j++) {
			deque.addFirst(j);
		}
		for (unsigned int j = 0; j < 8; j++) {
			REQUIRE(deque.get(j) == 8-1-j);
		}
	}
	SECTION("Test removeFirst function") {
		for (unsigned int j = 0; j < 8; j++) {
			deque.addFirst(j);
		}
		for (unsigned int j = 0; j < 8; j++) {
			REQUIRE(deque.removeFirst().value() == 8-1-j);
		}
		REQUIRE(deque.removeFirst() == std::nullopt);
	}
}

TEST_CASE("Test dual array deque") {
	DualArrayDeque<unsigned int> deque;

	SECTION("Test addLast function") {
		for (unsigned int j = 0; j < 8; j++) {
			deque.addLast(j);
		}
		for (unsigned int j = 0; j < 8; j++) {
			REQUIRE(deque.get(j) == j);
		}
	}
	SECTION("Test removeLast function") {
		for (unsigned int j = 0; j < 8; j++) {
			deque.addLast(j);
		}
		for (unsigned int j = 0; j < 8; j++) {
			REQUIRE(deque.removeLast().value() == 8-1-j);
		}
		REQUIRE(deque.removeLast() == std::nullopt);
	}

	SECTION("Test addFirst function") {
		for (unsigned int j = 0; j < 8; j++) {
			deque.addFirst(j);
		}
		for (unsigned int j = 0; j < 8; j++) {
			REQUIRE(deque.get(j) == 8-1-j);
		}
	}
	SECTION("Test removeFirst function") {
		for (unsigned int j = 0; j < 8; j++) {
			deque.addFirst(j);
		}
		for (unsigned int j = 0; j < 8; j++) {
			REQUIRE(deque.removeFirst().value() == 8-1-j);
		}
		REQUIRE(deque.removeFirst() == std::nullopt);
	}
}
