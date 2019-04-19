#include <optional>
#include "array_queue.h"
#include "catch.h"

TEST_CASE("Test array queue") {
	ArrayQueue<unsigned int> queue;

	SECTION("Test push function") {
		for (unsigned int j = 0; j < 8; j++) {
			queue.enqueue(j);
		}
		for (unsigned int j = 0; j < 8; j++) {
			REQUIRE(queue.get(j) == j);
		}
	}
	SECTION("Test pop function") {
		for (unsigned int j = 0; j < 8; j++) {
			queue.enqueue(j);
		}
		for (unsigned int j = 0; j < 8; j++) {
			REQUIRE(queue.dequeue().value() == j);
		}
		REQUIRE(queue.dequeue() == std::nullopt);
	}
}
