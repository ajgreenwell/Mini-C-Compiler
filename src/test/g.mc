int f(int x) {
    return x + 1;
}

int g(int a, int b) {
  b = f(a * 2);
  return b;
}

int main ( ) {
	print g(5, 6);
	print g(7, 8);
	print g(100, 101);

	return 0;
}
