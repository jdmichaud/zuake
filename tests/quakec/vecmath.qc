void   (string str, ...) print = #99;
string (vector v)   vtos       = #27;

void() main = {
  local vector vin = '15 43 0';
  local vector v2 = -vin;
  print(vtos(v2), "\n");
};
