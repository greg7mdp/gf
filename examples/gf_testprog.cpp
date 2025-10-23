#include <vector>
#include <iostream>
#include <variant>
#include <string>
#include <cstdint>
#include <algorithm>

using namespace std;

int fib(int n) {
  if (n <= 1)
    return n;
  return fib(n - 1) + fib(n - 2);
}

struct A {
   int              i = 1;
   string           s = "one";
   std::vector<int> v = {1, 2, 3};
};

struct C {
   int              i = 0;
   std::vector<A>   as = {{1, "one"}, {2, "two"}, {3, "three"}};
};

void check_variants() {
    std::variant<int, float> v, w;
    v = 42; // v contains int
    [[maybe_unused]] int i = std::get<int>(v);
    w = std::get<int>(v);

    std::variant<std::string> x("abc");
    x = "def";
}

int main(int argc, char** argv) {
   if (argc > 1)
      printf("%s\n", argv[1]);
   C c;
   check_variants();
   std::vector<int> v { 2, 3, 4 };
   std::vector<A> v2 { {2, "two", {}}, {3, "three", {}} };
   int i = 1;
   A a;
   float f[] {3.14159f, 4.0f, 5.5f };
   const char* s = "hello";
   printf("%s fib(3)=%d\n", s, fib(3));
   if (i < 3)
      i = (i + 1);

   int res = a.i + i + c.as[1].i - fib(1);

   return std::clamp(res, 0, 0);
}
