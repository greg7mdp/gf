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

int main(int argc, char **argv)
{
   int i = 1;
   A a;
   C c;
   const char* s = "hello";
   printf("%s fib(3)=%d\n", s, fib(3));

   int res = a.x + c.x - fib(1);

   return std::clamp(res, 0, 0);
}
