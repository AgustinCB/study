#include <algorithm>
#include <iostream>
#include <map>
#include "FBullCowGame.h"

using FString = std::string;
using int32 = int;

std::map<char, int32> GetPositionMap(FString& str) {
    std::map<char, int32> result;
    for (int32 i = 0; i < str.size(); i += 1) {
        result[str[i]] = i;
    }
    return result;
}

void FBullCowGame::Reset() {
    MaxTries = 5;
    CurrentTry = 1;
    HiddenWord = "planet";
    HiddenWordCharPositions = GetPositionMap(HiddenWord);
}

int32 FBullCowGame::GetMaxTries() const {
    return MaxTries;
}

int32 FBullCowGame::GetCurrentTry() const {
    return CurrentTry;
}

bool FBullCowGame::isGameWon() const {
    return false;
}

std::optional<WordError> FBullCowGame::checkGuessValidity(FString Guess) const {
    if (GetHiddenWordLength() != Guess.length()) {
        return WrongLengthError(GetHiddenWordLength(), Guess.length());
    }
    return {};
}

FBullCowGame::FBullCowGame() {
    Reset();
}

FBullCowCount FBullCowGame::SubmitGuess(FString Guess) {
    CurrentTry += 1;
    FBullCowCount BullCowCount;
    auto GuessCharPositions = GetPositionMap(Guess);

    BullCowCount.Cows = (int32) std::count_if(Guess.begin(), Guess.end(), [&](char c) {
        return HiddenWordCharPositions.find(c) != HiddenWordCharPositions.end() &&
                HiddenWordCharPositions[c] != GuessCharPositions[c];
    });
    BullCowCount.Bulls = (int32) std::count_if(Guess.begin(), Guess.end(), [&](char c) {
        return HiddenWordCharPositions.find(c) != HiddenWordCharPositions.end() &&
               HiddenWordCharPositions[c] == GuessCharPositions[c];
    });
    return BullCowCount;
}

unsigned long FBullCowGame::GetHiddenWordLength() const {
    return HiddenWord.length();
}
