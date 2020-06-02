bool gt(int a, int b) 
{
	return a > b;
}

bool geq(int a, int b) 
{
	return a >= b;
}

bool lt(int a, int b) 
{
	return a < b;
}

bool leq(int a, int b) 
{
	return a <= b;
}

int main() {

	if( gt(3, 2) ) {
		print 1;
	} else {

		if( geq(2, 3) ) {
			print 2;
		} else {

			if( lt(2, 3) ) {
				print 3;
			} else {

				if( leq(2, 3) ) {
					print 4;
				} else {
					print 5;
				}

			}

		}

	}

	return 0;
}
