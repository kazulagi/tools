#include <cstring>
#include <iostream>
using namespace std;

struct Employee {
    int number;
    char name[80];
    int salary;
};

struct Company {
    struct Employee tanaka;
    struct Employee sato;
    struct Employee tomobe;
    char companyName[80];
};

int main() {
    struct Employee tanaka;

    tanaka.number = 1234;
    strcpy(tanaka.name, "Tanaka Ichiro");
    tanaka.salary = 200000;

    cout << tanaka.number << "\n";
    cout << tanaka.name << "\n";
    cout << tanaka.salary << "\n";

    return 0;
}