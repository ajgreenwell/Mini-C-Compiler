int mod(int a, int b) {
  if (b < 0) return 0;
  else while (a >= b) a = a - b;
  return a;
}

int abs(int a) {
  if (a < 0) return 0 - a;
  else return a;
}

int gcd(int a, int b) {
  a = abs(a);
  b = abs(b);
  if (a == 0 or b == 0) return a + b;
  else if (b > a) return gcd(a, mod(b, a));
  else return gcd(mod(a, b), b);
}

void main() {
  int x;
  x = gcd(14039, 1529);
  print x;
}
