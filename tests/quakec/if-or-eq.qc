entity ()                 spawn = #14;
void   (string str, ...)  print = #99;

void() main = {
  float a = 42;
  string b = "666";
  float c = a - 1;

  if (a == c) {
    print("simple equality KO\n");
  } else {
    print("simple equality OK\n");
  }

  if (a == c || b == "666") {
    print("or operator OK\n");
  } else {
    print("or operator KO\n");
  }

  if (b == "667") {
    print("string comparison KO\n");
  } else {
    print("string comparison OK\n");
  }

  entity e;
  if (!e) {
    print("testing empty entity OK\n");
    e = spawn();
  } else {
    print("testing empty entity KO\n");
  }

  if (!e) {
    print("testing empty entity KO\n");
  } else {
    print("testing empty entity OK\n");
  }

  string s;
  if (!s) {
    print("testing empty string OK\n");
    s = "test";
  } else {
    print("testing empty string KO\n");
  }

  if (!s) {
    print("testing empty string KO\n");
  } else {
    print("testing empty string OK\n");
  }
}
