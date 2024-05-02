void   (string str, ...) print = #99;

float(vector f) foo = {
  return 1.14 + f_x;
};

void(vector f) main = {
  local float pi = foo(f);
  local float toto = pi * 2;
  print("hello ", "world", "\n");
};

