#include "array.h"
#include "catch.h"

TEST_CASE("Test array") {
	array<unsigned int> a(5); 

	SECTION("Test push function") {
		for (unsigned int j = 0; j < 4; j++) {
			a.get_mut(j) = j;
		}
		for (unsigned int j = 0; j < 4; j++) {
			REQUIRE(a.get(j) == j);
		}
	}
}
