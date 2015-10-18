#include <iostream>
#include <thread>

int main() {
  auto phrase = "Hi there";
  std::thread speach {[=](void){std::cout << phrase << "\n";}};
  speach.join();
}
