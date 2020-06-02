int f(int a) {
  int c;
  if (a < 7) {
    c = a + 3;
    }
    else {
    c = a + 4;
    }
  return c;
}

int main() {
  int ans;
  ans = f(8);
  print(ans);
  return 1;
}
