#include <cstring>
#include <iostream>
using namespace std;

class User {
    private:
        int hand;
    public:
        void Init();
        void setHand();
        void showHand();
};

void User::Init(){
    hand=0;
};


void User::setHand(){
    cout << "1 : Paper, 2 : Stone, 3 : Scissors \n";
    cout << "Input 1, 2 or 3\n";
    cin >> hand;
};

void User::showHand(){
    if(hand == 1){
        cout << "Paper \n";
    }
    else if(hand == 2){
        cout << "Stone \n";
    }
    else if (hand == 3){
        cout << "Scissors \n";
    }
    else{
        cout << "No inputs \n";
    }
    
}

int main() {
    User user;

    user.Init();
    user.setHand();
    user.showHand();
}
    