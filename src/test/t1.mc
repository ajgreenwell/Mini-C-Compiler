int mod(int a, int b)
{
	if ( b < 0 ) return 0;
	else
		print a;

	while ( a >= b ) a = (a - b);

	print a;

	return a;
}

int main()
{
	mod(13, 7);
	return 1;
}
