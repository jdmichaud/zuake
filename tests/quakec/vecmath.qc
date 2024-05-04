void   (string str, ...) print = #99;
string (vector v)        vtos  = #27;

void() main = {
  local vector vin = '15 43 0';
  local vector v2 = -vin;
  print("v2: ", vtos(v2), " vin: ", vtos(vin), "\n");
};
