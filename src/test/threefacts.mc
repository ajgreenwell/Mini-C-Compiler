int ifact(int m) {
  int answer;
  answer = 1;
  while (m > 0) {
    answer = answer * m;
    m = m - 1;
    }
  return answer;
}

int rfact(int n) {
  if (n == 0)
    return 1;
  else
    return n * rfact(n - 1);
}

int condfact(int n) {
  return (n == 0) ? 1 : n * condfact(n - 1);
}

int main() {
  print ifact(6);
  print rfact(6);
  print condfact(6);
  return 0;
  }
