#ifndef BULLANDCOWS_FBULLCOWGAME_H
#define BULLANDCOWS_FBULLCOWGAME_H

#include <string>

class FBullCowGame {
public:
    FBullCowGame();
    void Reset();
    int GetMaxTries() const;
    int GetCurrentTry() const;
    bool isGameWon() const;
    bool checkGuessValidity(std::string);
private:
    int CurrentTry;
    int MaxTries;
};

#endif //BULLANDCOWS_FBULLCOWGAME_H
