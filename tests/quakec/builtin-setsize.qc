void   (entity e, vector mins, vector maxs) setsize = #4;
entity ()                                   spawn = #14;
void   (string str, ...)                    print = #99;

.vector mins;
.vector maxs;

void() main = {
  entity e = spawn();
  setsize(e, '1 2 3', '4 5 6');
  if (e.mins.x == 1 && e.mins.y == 2 && e.mins.z == 3 && e.maxs.x == 4 && e.maxs.y == 5 && e.maxs.z == 6) {
    print("builtin-setsize OK\n");
  } else {
    print("builtin-setsize KO\n");
  }
}
