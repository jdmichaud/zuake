entity ()                 spawn = #14;
string (float v)          ftos  = #26;
void   (string str, ...)  print = #99;

.float toto;
.float titi;
.float tutu;

void() main = {
  entity e1 = spawn();
  entity e2 = spawn();
  e2.tutu = 10;
  print(ftos(e2.tutu));
}
