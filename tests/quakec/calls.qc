string (float v)         ftos  = #26;
void   (string str, ...) print = #99;

float(float x, float y, float z) sum = {
    return x + y + z;
};

void() main = {
    local float a = 1;
    local float b = 2;
    local float c = 3;

    local float f;
    f = sum(sum(a, sum(a, b, c), c),
            sum(sum(sum(a, b, c), b, sum(a, b, c)), b, sum(a, b, sum(a, b, c))),
            sum(sum(a, b, c), b, c));
    print(ftos(f), "\n");

};
