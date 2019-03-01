#include <algorithm>
#include <iostream>
#include <map>
#include <string>
#include "FBullCowGame.h"

#define TMap std::map

using FString = std::string;
using int32 = int;

TMap<char, int32> GetPositionMap(FString& str) {
    TMap<char, int32> result;
    for (int32 i = 0; i < str.size(); i += 1) {
        result[str[i]] = i;
    }
    return result;
}

std::optional<char> GetFirstNotLowercaseChar(FString& Word) {
    std::string::iterator i = std::find_if(Word.begin(), Word.end(), [](char c ) {
        return !std::islower((int32) c);
    });
    if (i != Word.end()) {
        return Word[i - Word.begin()];
    }
    return {};
}

std::optional<char> GetFirstRepeatedChar(FString& Word) {
    TMap<char, bool> Seen;
    for (char const &c : Word) {
        if (Seen.find(c) != Seen.end()) {
            return c;
        }
        Seen[c] = true;
    }
    return {};
}

void FBullCowGame::Reset() {
    bIsGameWon = false;
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
    return bIsGameWon;
}

std::optional<WordError> FBullCowGame::checkGuessValidity(FString Guess) const {
    if (GetHiddenWordLength() != Guess.length()) {
        return WrongLengthError(GetHiddenWordLength(), Guess.length());
    }
    if (auto letter = GetFirstNotLowercaseChar(Guess)) {
        return NotLowercaseError(*letter);
    }
    if (auto letter = GetFirstRepeatedChar(Guess)) {
        return NotIsogramError(*letter);
    }

    return {};
}

FBullCowGame::FBullCowGame() {
    Reset();
}

FBullCowCount FBullCowGame::SubmitValidGuess(FString Guess) {
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
    bIsGameWon = BullCowCount.Bulls == HiddenWord.length();
    return BullCowCount;
}

unsigned long FBullCowGame::GetHiddenWordLength() const {
    return HiddenWord.length();
}
