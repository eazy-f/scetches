#include <iostream>
#include <thread>

class PetersonLock {
public:
  PetersonLock() : victim{0}, flag{false, false} {};

  void lock(int thread_id) {
    auto other = 1 - thread_id;
    flag[thread_id] = true;
    victim = thread_id;

    while(flag[other] && victim == thread_id);
  }

  void unlock(int thread_id) {
    flag[thread_id] = false;
  }

private:
  bool flag[2];
  int victim;
};

void increment(int* n, int increment_num,
               int thread_id, PetersonLock* plock)
{
  for(auto i = 0; i < increment_num; i++) {
    plock->lock(thread_id);
    (*n)++;
    plock->unlock(thread_id);
  }
}

int main(int argc, char **argv) {
  PetersonLock plock;
  int n = 0;

  if (argc < 2) {
    std::cerr << "supply a number of increment operations\n";
    return 1;
  }
  int limit = atol(argv[1]);

  std::thread first {increment, &n, limit, 0, &plock};
  std::thread second {increment, &n, limit, 1, &plock};

  first.join();
  second.join();

  std::cout << n << "\n";
}
