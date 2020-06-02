int f(int a) {
  int b;
  if (a > 4) {
    b = 2 + a * 8;
    }
  else {
    b = 343;
  }
  return b;
}	 

int main( ) {
	print f(5);
	print f(6);
	print f(7);
	print f(8);
	return 0;
}
