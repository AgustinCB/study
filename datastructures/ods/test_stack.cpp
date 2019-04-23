#include <optional>
#include "array_stack.h"
#include "catch.h"
#include "rootish_vector.h"

TEST_CASE("Test array stack") {
	ArrayStack<unsigned int> stack;

	SECTION("Test push function") {
		for (unsigned int j = 0; j < 8; j++) {
			stack.push(j);
		}
		for (unsigned int j = 0; j < 8; j++) {
			REQUIRE(stack.get(j) == j);
		}
	}
	SECTION("Test pop function") {
		for (unsigned int j = 0; j < 8; j++) {
			stack.push(j);
		}
		for (unsigned int j = 0; j < 8; j++) {
			REQUIRE(stack.pop().value() == 8-1-j);
		}
		REQUIRE(stack.pop() == std::nullopt);
	}
}

TEST_CASE("Test array stack with rootish vector") {
	ArrayStack<unsigned int, RootishVector<unsigned int>> stack;

	SECTION("Test push function") {
		for (unsigned int j = 0; j < 8; j++) {
			stack.push(j);
		}
		for (unsigned int j = 0; j < 8; j++) {
			REQUIRE(stack.get(j) == j);
		}
	}
	SECTION("Test pop function") {
		for (unsigned int j = 0; j < 8; j++) {
			stack.push(j);
		}
		for (unsigned int j = 0; j < 8; j++) {
			REQUIRE(stack.pop().value() == 8-1-j);
		}
		REQUIRE(stack.pop() == std::nullopt);
	}
}

TEST_CASE("Test array stack with rootish vector using alternative sqrt") {
	ArrayStack<unsigned int, RootishVector<unsigned int, false>> stack;

	SECTION("Test push function") {
		for (unsigned int j = 0; j < 8; j++) {
			stack.push(j);
		}
		for (unsigned int j = 0; j < 8; j++) {
			REQUIRE(stack.get(j) == j);
		}
	}
	SECTION("Test pop function") {
		for (unsigned int j = 0; j < 8; j++) {
			stack.push(j);
		}
		for (unsigned int j = 0; j < 8; j++) {
			REQUIRE(stack.pop().value() == 8-1-j);
		}
		REQUIRE(stack.pop() == std::nullopt);
	}
}
