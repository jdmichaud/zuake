string (float v)          ftos  = #26;
float  (float v)          sqrt  = #62;
float  (float v, float e) pow   = #97;
void   (string str, ...)  print = #99;

void main() {
    float hundy = __builtin_pow(10, 2); // 10^2 == 100
    print(ftos(hundy), "\n");      // prints: 100

    hundy = pow(10, 2);
    print(ftos(hundy), "\n");

    hundy -= 90; // 100-90 = 10
    print(ftos(hundy ** 2), "\n"); // prints: 100
    print(ftos(pow(hundy, 2)), "\n"); // prints: 100

    hundy = 10.0f;
    print(ftos(__builtin_exp(hundy)), "\n"); // prints: 22026.465
}
