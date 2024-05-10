void   (string str, ...)  print = #99;

void() main = {
  float a = 42;
  string b = "666";
  float c = a - 1;

  if (a == c) {
    print("simple equality NOK\n");
  } else {
    print("simple equality OK\n");
  }

  if (a == c || b == "666") {
    print("or operator OK\n");
  } else {
    print("or operator NOK\n");
  }

  if (b == "667") {
    print("string comparison NOK\n");
  } else {
    print("string comparison OK\n");
  }
}
