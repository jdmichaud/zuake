void   (string str, ...) print = #99;
string (float f) ftos = #26;

float(vector v) foo = {
  return 1.14 + v_y;
};

void() main = {
  local float pi = foo('0 2 0');
  print("pi is equal to ", ftos(pi), "\n");
};

