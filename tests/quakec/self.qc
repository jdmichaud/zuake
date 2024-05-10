entity ()                 spawn = #14;
string (float v)          ftos  = #26;
string (vector v)         vtos  = #27;
void   (string str, ...)  print = #99;

entity self;
.string classname;
.float height;
.vector origin;
.void() foo;

void() dude_foo {
  print("height: ", ftos(self.height), " origin ", vtos(self.origin), "\n");
}

void() main = {
  self = spawn();
  self.classname = "dude";
  self.height = 128;
  self.origin = '0 32 -64';
  self.foo = dude_foo;
  self.foo();
}
