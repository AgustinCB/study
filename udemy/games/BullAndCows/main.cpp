#include <iostream>
#include "FBullCowGame.h"

bool AskToPlayAgain();
std::string GetGuess();
void PlayGame();
void PrintWelcomeMessage();

FBullCowGame BCGame;

int main() {
    do {
        PrintWelcomeMessage();
        PlayGame();
    } while(AskToPlayAgain());

    return 0;
}

bool AskToPlayAgain() {
    std::cout << "Do you want to play again? (y/n)" << std::endl;
    std::string Response;
    std::getline(std::cin, Response);
    return Response[0] == 'y' || Response[0] == 'Y';
}

std::string GetGuess() {
    std::cout << "Try " << BCGame.GetCurrentTry() << ": Guess the word: ";
    std::string Guess;
    std::getline(std::cin, Guess);
    return Guess;
}

void PlayGame() {
    BCGame.Reset();
    for (int count = 1; count <= BCGame.GetMaxTries(); count+=1) {
        std::string Guess = GetGuess();
        std::cout << "Your guess was " << Guess << std::endl;
        std::cout << std::endl;
    }
}

void PrintWelcomeMessage() {
    constexpr int WORD_LENGTH = 5;
    std::cout << "Welcome to Bulls and Cows a fun word game." << std::endl;
    std::cout << "Can you guess the " << WORD_LENGTH << " letter isogram I'm thinking of?" << std::endl;
}
