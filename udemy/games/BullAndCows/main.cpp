/*
 * This is the view of the MVC of the game Bulls and Cows
 */
#include <algorithm>
#include <iostream>
#include "FBullCowGame.h"

using FString = std::string;
using int32 = int;

bool AskToPlayAgain();
FString GetValidGuess();
void PlayGame();
void PrintGameSummary();
void PrintWelcomeMessage();

FBullCowGame BCGame;

int main() {
    do {
        PrintWelcomeMessage();
        PlayGame();
        PrintGameSummary();
    } while(AskToPlayAgain());

    return 0;
}

bool AskToPlayAgain() {
    std::cout << "Do you want to play again with the same hidden word? (y/n)" << std::endl;
    FString Response;
    std::getline(std::cin, Response);
    return Response[0] == 'y' || Response[0] == 'Y';
}

FString GetValidGuess() {
    FString Guess;
    std::optional<WordError> Validity;
    do {
        std::cout << "Try " << BCGame.GetCurrentTry() << " of " << BCGame.GetMaxTries() << ": Guess the word: ";
        std::getline(std::cin, Guess);
        Validity = BCGame.checkGuessValidity(Guess);
        if (Validity.has_value()) {
            std::visit(HandleWordError(std::cout), *Validity);
        }
    } while(Validity.has_value());
    return Guess;
}

void PlayGame() {
    BCGame.Reset();
    for (int32 count = 1; count <= BCGame.GetMaxTries() && !BCGame.isGameWon(); count += 1) {
        FString Guess = GetValidGuess();
        std::cout << "Your guess was " << Guess << std::endl;
        auto Result = BCGame.SubmitValidGuess(Guess);
        std::cout << "Number of cows: " << Result.Cows << std::endl;
        std::cout << "Number of bulls: " << Result.Bulls << std::endl;
        std::cout << std::endl;
    }
}

void PrintGameSummary() {
    if (BCGame.isGameWon()) {
        std::cout << "Well done, you won!";
    } else {
        std::cout << "You failed, yo' sucker";
    }
    std::cout << std::endl;
}

void PrintWelcomeMessage() {
    std::cout << "Welcome to Bulls and Cows a fun word game." << std::endl;
    std::cout << "Can you guess the " << BCGame.GetHiddenWordLength() << " letter isogram I'm thinking of?" << std::endl;
}
