#include "catch.h"
#include "circular_vector.h"
#include "vector.h"

TEST_CASE("Test vector") {
	Vector<unsigned int> vector;

	SECTION("Test add function") {
		for (unsigned int j = 0; j < 8; j++) {
			vector.add(j, j);
		}
		for (unsigned int j = 0; j < 8; j++) {
			REQUIRE(vector.get(j) == j);
		}
        vector.add(4, 9);
        REQUIRE(vector.get(4) == 9);
		for (unsigned int j = 5; j < 9; j++) {
			REQUIRE(vector.get(j) == j-1);
		}
	}
	SECTION("Test remove function") {
		for (unsigned int j = 0; j < 8; j++) {
			vector.add(j, j);
		}
        REQUIRE(vector.remove(7) == 7);
		for (unsigned int j = 0; j < 7; j++) {
			REQUIRE(vector.remove(0) == j);
		}
	}

    SECTION("Test addAll function") {
	    Vector<unsigned int> vector1;
		for (unsigned int j = 0; j < 8; j++) {
			vector.add(j, j);
		}
        vector1.add(0, 0);
        vector1.addAll(0, std::move(vector));
		for (unsigned int j = 0; j < 8; j++) {
			REQUIRE(vector1.get(j) == j);
		}
        REQUIRE(vector1.get(8) == 0);
    }
}

TEST_CASE("Test circular vector") {
	CircularVector<unsigned int> vector;

	SECTION("Test add function") {
		for (unsigned int j = 0; j < 8; j++) {
			vector.add(j, j);
		}
		for (unsigned int j = 0; j < 8; j++) {
			REQUIRE(vector.get(j) == j);
        }
        vector.add(4, 9);
        REQUIRE(vector.get(4) == 9);
		for (unsigned int j = 5; j < 9; j++) {
			REQUIRE(vector.get(j) == j-1);
		}
	}
	SECTION("Test remove function") {
		for (unsigned int j = 0; j < 8; j++) {
			vector.add(j, j);
		}
        REQUIRE(vector.remove(7) == 7);
		for (unsigned int j = 0; j < 7; j++) {
			REQUIRE(vector.remove(0) == j);
		}
	}

    SECTION("Test addAll function") {
	    CircularVector<unsigned int> vector1;
		for (unsigned int j = 0; j < 8; j++) {
			vector.add(j, j);
		}
        vector1.add(0, 0);
        vector1.addAll(0, std::move(vector));
		for (unsigned int j = 0; j < 8; j++) {
			REQUIRE(vector1.get(j) == j);
		}
        REQUIRE(vector1.get(8) == 0);
    }
}
