int main(){
	int a;
	bool b;

	a = -2;
	b = -a > 3;

	if ( not b ) {
		print 10 ;
	}
	else {
		print 230 ;
	}

	if ( not ( 3 > 3 ) ) {
		print 10 ;
	}
	else {
		print 330 ;
	}

	if ( not ( 4 > 3 ) ) {
		print 430 ;
	}
	else {
		print 10 ;
	}

	if ( not ( 2 >= 3 ) ){
		print 10 ;
	}
	else {
		print 230 ;
	}

	if ( 3 >= 3 ) {
		print 330 ;
	}
	else {
		print 10 ;
	}

	if ( 4 >= 3 ) {
		print 430 ;
	}
	else {
		print 10 ;
	}

	if ( 2 < 3 ) {
		print 230 ;
	}
	else {
		print 10 ;
	}

	if ( 3 < 3 ) {
		print 10 ;
	}
	else {
		print 330 ;
	}

	if ( 4 < 3 ) {
		print 10 ;
	}
	else {
		print 430 ;
	}

	if ( 2 <= 3 ) {
		print 230 ;
	}
	else {
		print 10 ;
	}

	if ( 3 <= 3 ) {
		print 330 ;
	}
	else {
		print 10 ;
	}

	if ( 4 <= 3 ) {
		print 10 ;
	}
	else {
		print 430 ;
	}

	return 0;
}
