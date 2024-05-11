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

void() main = {
  print("classname ", world.classname, "\n");
  print("wad ", world.wad, "\n");
  print("sounds ", ftos(world.sounds), "\n");
  print("worldtype ", ftos(world.worldtype), "\n");
  print("message ", world.message, "\n");
}
