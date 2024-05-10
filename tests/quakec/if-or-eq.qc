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
    print("testing uninitialized entity OK\n");
  } else {
    print("testing uninitialized entity KO\n");
  }

  e = spawn();
  if (!e) {
    print("testing initialized entity KO\n");
  } else {
    print("testing initialized entity OK\n");
  }

  string s;
  if (!s) {
    print("testing uninitialized string OK\n");
  } else {
    print("testing uninitialized string KO\n");
  }

  // This seems to only be true in some flavor of QuakeC.
  // if (s == "") { // uninitialized string is equal to empty string
  //   print("testing comparing uninitialized string with empty string OK\n");
  // } else {
  //   print("testing comparing uninitialized string with empty string KO\n");
  // }

  s = "";
  if (!s) { // !"" count as false
    print("testing boolean operation on empty string KO\n");
  } else {
    print("testing boolean operation on empty string OK\n");
  }

  s = "test";
  if (!s) { // string is now initialized
    print("testing boolean operation on initialized string KO\n");
  } else {
    print("testing boolean operation on initialized string OK\n");
  }
}
