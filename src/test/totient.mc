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

int totient(int a) {
  int count;
  int iteration;
  while (iteration < a) {
    iteration = iteration + 1;
    if (gcd(iteration, a) == 1) count = count + 1;
    else count = count;
  }
  return count;
}

void main() {
  int x;
  x = totient(1638);
  print x;
}
