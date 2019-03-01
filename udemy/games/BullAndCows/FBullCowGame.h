#ifndef BULLANDCOWS_FBULLCOWGAME_H
#define BULLANDCOWS_FBULLCOWGAME_H

#include <map>
#include <optional>
#include <string>
#include <variant>

using FString = std::string;
using int32 = int;

struct FBullCowCount {
    int32 Bulls = 0;
    int32 Cows = 0;
};

struct NotIsogramError {
    char RepeatedChar;
};
struct WrongLengthError {
    unsigned long Expected;
    unsigned long Got;
    WrongLengthError(unsigned long _Expected, unsigned long _Got) : Expected(_Expected), Got(_Got) {};
};
struct NotLowercaseError {
    char NotLowercaseChar;
};
using WordError = std::variant<NotIsogramError, WrongLengthError, NotLowercaseError>;

struct HandleWordError {
    std::ostream& OutputStream;
    HandleWordError(std::ostream& _OutputStream) : OutputStream(_OutputStream) {};
    void operator()(WrongLengthError& Error) {
        OutputStream << "Please enter a " << Error.Expected << " letters word";
        OutputStream << " instead of a " << Error.Got << " letters word" << std::endl;
    }
    void operator()(NotIsogramError& Error) {
        OutputStream << "The word has to be an isogram, your guess repeated letter " << Error.RepeatedChar << std::endl;
    }
    void operator()(NotLowercaseError& Error) {
        OutputStream << "The word has to be all lowercase, your guess has an uppercase " << Error.NotLowercaseChar;
        OutputStream << std::endl;
    }
};

class FBullCowGame {
public:
    FBullCowGame();
    void Reset();
    int32 GetMaxTries() const;
    int32 GetCurrentTry() const;
    unsigned long GetHiddenWordLength() const;
    bool isGameWon() const;
    std::optional<WordError> checkGuessValidity(FString) const;
    FBullCowCount SubmitGuess(FString);
private:
    int32 CurrentTry;
    int32 MaxTries;
    FString HiddenWord;
    std::map<char, int32> HiddenWordCharPositions;
};

#endif //BULLANDCOWS_FBULLCOWGAME_H
