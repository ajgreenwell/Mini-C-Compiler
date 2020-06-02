int f(int a) {
  int b;
  int c;
  b = 1 + 2 * 3;
  c = b * 2;
  while (b > 0) {
    b = b - 1;
    }
  return b + c;
}	 

int main( ){
	print f(10);
	print f(11);
	print f(12);
	print f(13);

	return 0;
}
