#include "FBullCowGame.h"

void FBullCowGame::Reset() {
    MaxTries = 5;
    CurrentTry = 1;
}

int FBullCowGame::GetMaxTries() const {
    return MaxTries;
}

int FBullCowGame::GetCurrentTry() const {
    return CurrentTry;
}

bool FBullCowGame::isGameWon() const {
    return false;
}

bool FBullCowGame::checkGuessValidity(std::string) {
    return false;
}

FBullCowGame::FBullCowGame() {
    Reset();
}
