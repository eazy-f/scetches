#include <iostream>

using namespace std;

class Greeter {
public:
  Greeter() {
    cout << "Hello, world!\n";
  }

  ~Greeter() { /* parenthesis are necessary */
    cout << "Bye, world.\n";
  }
}; /* semicolon, wow */

void hello() {
  Greeter on_stack;
  /* Greeter on_stack(); and what is this? */
}

int main () {
  hello();

  auto n = 13; /* {} doesn't work */
  cout << "size of " << n << " is " << sizeof(n) << "\n";
}
