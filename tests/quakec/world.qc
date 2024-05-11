// This file is not part of the automated test suite and is only used to test
// loading of bsp file.
string (float v)          ftos  = #26;
void   (string str, ...)  print = #99;

entity world;
entity self;

.string classname;
.string wad;
.float sounds;
.float worldtype;
.string message;

.float angle;
.float health;
.string killtarget;
.float light;
.vector mangle;
.string map;
.string model;
.vector origin;
.float spawnflags;
.float speed;
.float style;
.string target;
.string targetname;
.float wait;

void() worldspawn = {
  print("classname ", self.classname, "\n");
  print("wad ", self.wad, "\n");
  print("sounds ", ftos(self.sounds), "\n");
  print("worldtype ", ftos(self.worldtype), "\n");
  print("message ", self.message, "\n");
}

void() main = {
  print("nothing to do\n");
}
