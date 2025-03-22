#include <bits/stdc++.h>
#include <iostream>

using namespace std;

int fib(uint64_t n) {
  if (n <= 1)
    return n;
  return fib(n - 1) + fib(n - 2);
}

struct A {
   int    x = 1;
   string s = "hello";
};


struct C {
   int              x = 0;
   std::vector<int> y = {1, 2, 3};
   A                a;
};

void check_variants() {
    std::variant<int, float> v, w;
    v = 42; // v contains int
    int i = std::get<int>(v);
    w = std::get<int>(v);

    std::variant<std::string> x("abc");
    x = "def";
}

int main(int argc, char **argv)
{
   check_variants();
   std::vector<int> v { 2, 3, 4 };
   std::vector<A> v2 { {2, "two"}, {3, "three"} };
   int i = 1;
   A a;
   C c;
   const char* s = "hello";
   printf("%s fib(3)=%d\n", s, fib(3));
   i = (i + 1);

   int res = a.x + c.y[1] - fib(1);

   return std::clamp(res, 0, 0);
}
