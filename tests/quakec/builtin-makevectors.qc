void   (vector v)         makevectors = #1;
string (float v)          ftos        = #26;
string (vector v)         vtos        = #27;
float  (float val)        fabs        = #43;
void   (string str, ...)  print       = #99;

vector v_forward;
vector v_up;
vector v_right;

float(vector lhs, vector rhs, float precision) isVectorsEqual = {
  if (fabs(lhs.x - rhs.x) > precision) {
    return 0;
  }
  if (fabs(lhs.y - rhs.y) > precision) {
    return 0;
  }
  if (fabs(lhs.z - rhs.z) > precision) {
    return 0;
  }
  return 1;
}

void() main = {
  // pitch yaw roll
  makevectors('0 0 0');
  if (isVectorsEqual(v_forward, '1 0 0', 0.00001) && isVectorsEqual(v_up, '0 0 1', 0.00001) && isVectorsEqual(v_right, '0 -1 0', 0.00001)) {
    print("0 0 0 OK\n");
  } else {
    print("0 0 0 KO\n");
  }

  makevectors('90 0 0');
  if (isVectorsEqual(v_forward, '0 0 -1', 0.00001) && isVectorsEqual(v_up, '1 0 0', 0.00001) && isVectorsEqual(v_right, '0 -1 0', 0.00001)) {
    print("90 0 0 OK\n");
  } else {
    print("90 0 0 KO\n");
  }

  makevectors('0 90 0');
  if (isVectorsEqual(v_forward, '0 1 0', 0.00001) && isVectorsEqual(v_up, '0 0 1', 0.00001) && isVectorsEqual(v_right, '1 0 0', 0.00001)) {
    print("0 90 0 OK\n");
  } else {
    print("0 90 0 KO\n");
  }

  makevectors('0 0 90');
  if (isVectorsEqual(v_forward, '1 0 0', 0.00001) && isVectorsEqual(v_up, '0 -1 0', 0.00001) && isVectorsEqual(v_right, '0 0 -1', 0.00001)) {
    print("0 0 90 OK\n");
  } else {
    print("0 0 90 KO\n");
  }

  makevectors('45 45 45');
  if (isVectorsEqual(v_forward, '0.5 0.5 -0.7', 0.1) && isVectorsEqual(v_up, '0.853 -0.146 0.5', 0.001) && isVectorsEqual(v_right, '0.146 -0.853 -0.5', 0.001)) {
    print("45 45 45 OK\n");
  } else {
    print("45 45 45 KO\n");
  }
}
