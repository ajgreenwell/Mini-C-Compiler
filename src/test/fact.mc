int fact(int n) {
    if (n == 0)
       return 1;
    else
       return n * fact(n - 1);
}

int main() {
	print fact(7);
	print fact(8);
	print fact(9);
	print fact(10);
	print fact(11);
	return fact(7);
}
