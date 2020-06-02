for testfile in ./test/*.mc;
do
	path="${testfile%.*}";
	raw_fname="${path##*\/}";
	echo "---------- $raw_fname ----------";

	dune exec bin/main.exe $testfile > /dev/null;
	java -jar ./test/Mars4_5.jar "$path.asm" > "$path.test"

	diff "$path.test" "./test/ans/$raw_fname.ans"
done

rm ./test/*.dbg* ./test/*.asm* ./test/*.test
