# Generated by mc: 9:49 4.20.2019.
#
	.data
	.text
main:	addi	$sp, $sp, -4		# push fp
	sw	$fp, 0($sp)
	move	$fp, $sp		# fp <- sp
	addi	$sp, $sp, -156		# allocate locals
	li	$v0, 0
	sw	$v0, -148($fp)
	li	$v0, 1
	sw	$v0, -8($fp)
	li	$v0, 2
	sw	$v0, -12($fp)
	lw	$t1, -8($fp)
	lw	$t2, -12($fp)
	add	$v0, $t1, $t2
	sw	$v0, -16($fp)
	lw	$v0, -16($fp)
	sw	$v0, -148($fp)
	lw	$v0, -148($fp)
	sw	$v0, -24($fp)
	lw	$a0, -24($fp)
	li	$v0, 1			# $v0 gets print_int code for syscall
	syscall				# print
	li	$v0, 1
	sw	$v0, -28($fp)
	li	$v0, 2
	sw	$v0, -32($fp)
	lw	$t1, -28($fp)
	lw	$t2, -32($fp)
	add	$v0, $t1, $t2
	sw	$v0, -36($fp)
	lw	$v0, -36($fp)
	sw	$v0, -40($fp)
	lw	$a0, -40($fp)
	li	$v0, 1			# $v0 gets print_int code for syscall
	syscall				# print
	li	$v0, 3
	sw	$v0, -44($fp)
	li	$v0, 4
	sw	$v0, -48($fp)
	lw	$t1, -44($fp)
	lw	$t2, -48($fp)
	sub	$v0, $t1, $t2
	sw	$v0, -52($fp)
	lw	$v0, -52($fp)
	sw	$v0, -148($fp)
	lw	$v0, -148($fp)
	sw	$v0, -60($fp)
	lw	$a0, -60($fp)
	li	$v0, 1			# $v0 gets print_int code for syscall
	syscall				# print
	li	$v0, 3
	sw	$v0, -64($fp)
	li	$v0, 4
	sw	$v0, -68($fp)
	lw	$t1, -64($fp)
	lw	$t2, -68($fp)
	sub	$v0, $t1, $t2
	sw	$v0, -72($fp)
	lw	$v0, -72($fp)
	sw	$v0, -76($fp)
	lw	$a0, -76($fp)
	li	$v0, 1			# $v0 gets print_int code for syscall
	syscall				# print
	li	$v0, 5
	sw	$v0, -80($fp)
	li	$v0, 6
	sw	$v0, -84($fp)
	lw	$t1, -80($fp)
	lw	$t2, -84($fp)
	mul	$v0, $t1, $t2
	sw	$v0, -88($fp)
	lw	$v0, -88($fp)
	sw	$v0, -148($fp)
	lw	$v0, -148($fp)
	sw	$v0, -96($fp)
	lw	$a0, -96($fp)
	li	$v0, 1			# $v0 gets print_int code for syscall
	syscall				# print
	li	$v0, 37
	sw	$v0, -100($fp)
	li	$v0, 8
	sw	$v0, -104($fp)
	lw	$t1, -100($fp)
	lw	$t2, -104($fp)
	div	$t1, $t2
	mflo	$v0
	sw	$v0, -108($fp)
	lw	$v0, -108($fp)
	sw	$v0, -148($fp)
	lw	$v0, -148($fp)
	sw	$v0, -116($fp)
	lw	$a0, -116($fp)
	li	$v0, 1			# $v0 gets print_int code for syscall
	syscall				# print
	li	$v0, 37
	sw	$v0, -120($fp)
	li	$v0, 8
	sw	$v0, -124($fp)
	lw	$t1, -120($fp)
	lw	$t2, -124($fp)
	div	$t1, $t2
	mflo	$v0
	sw	$v0, -128($fp)
	lw	$v0, -128($fp)
	sw	$v0, -132($fp)
	lw	$a0, -132($fp)
	li	$v0, 1			# $v0 gets print_int code for syscall
	syscall				# print
	li	$v0, 37
	sw	$v0, -136($fp)
	li	$v0, 8
	sw	$v0, -140($fp)
	lw	$t1, -136($fp)
	lw	$t2, -140($fp)
	div	$t1, $t2
	mfhi	$v0
	sw	$v0, -144($fp)
	lw	$v0, -144($fp)
	sw	$v0, -148($fp)
	lw	$v0, -148($fp)
	sw	$v0, -152($fp)
	lw	$a0, -152($fp)
	li	$v0, 1			# $v0 gets print_int code for syscall
	syscall				# print
	li	$v0, 0
	sw	$v0, -156($fp)
	lw	$a0, -156($fp)
	move	$sp, $fp
	lw	$fp, 0($sp)		# pop restore fp
	addi	$sp, $sp, 4
	li	$v0, 17			# $v0 gets exit code for syscall
	syscall				# Exit here
