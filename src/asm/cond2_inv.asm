# Generated by mc: 9:49 4.20.2019.
#
	.data
	.text
main:	addi	$sp, $sp, -4		# push fp
	sw	$fp, 0($sp)
	move	$fp, $sp		# fp <- sp
	addi	$sp, $sp, -292		# allocate locals
	li	$v0, 0
	sw	$v0, -24($fp)
	li	$v0, 0
	sw	$v0, -44($fp)
	li	$v0, 0
	sw	$v0, -12($fp)
	li	$v0, 2
	sw	$v0, -16($fp)
	lw	$t1, -12($fp)
	lw	$t2, -16($fp)
	sub	$v0, $t1, $t2
	sw	$v0, -20($fp)
	lw	$v0, -20($fp)
	sw	$v0, -24($fp)
	li	$v0, 0
	sw	$v0, -28($fp)
	lw	$t1, -28($fp)
	lw	$t2, -24($fp)
	sub	$v0, $t1, $t2
	sw	$v0, -32($fp)
	li	$v0, 3
	sw	$v0, -36($fp)
	lw	$t1, -32($fp)
	lw	$t2, -36($fp)
	slt	$v0, $t2, $t1		# >
	sw	$v0, -40($fp)
	lw	$v0, -40($fp)
	sw	$v0, -44($fp)
	lw	$t1, -44($fp)
	slti	$v0, $t1, 1		# not
	sw	$v0, -48($fp)
	lw	$v0, -48($fp)
	beqz	$v0, l0
	li	$v0, 10
	sw	$v0, -52($fp)
	lw	$a0, -52($fp)
	li	$v0, 1			# $v0 gets print_int code for syscall
	syscall				# print
	j	l1
l0:	nop
	li	$v0, 230
	sw	$v0, -56($fp)
	lw	$a0, -56($fp)
	li	$v0, 1			# $v0 gets print_int code for syscall
	syscall				# print
l1:	nop
	li	$v0, 3
	sw	$v0, -60($fp)
	li	$v0, 3
	sw	$v0, -64($fp)
	lw	$t1, -60($fp)
	lw	$t2, -64($fp)
	slt	$v0, $t2, $t1		# >
	sw	$v0, -68($fp)
	lw	$t1, -68($fp)
	slti	$v0, $t1, 1		# not
	sw	$v0, -72($fp)
	lw	$v0, -72($fp)
	beqz	$v0, l2
	li	$v0, 10
	sw	$v0, -76($fp)
	lw	$a0, -76($fp)
	li	$v0, 1			# $v0 gets print_int code for syscall
	syscall				# print
	j	l3
l2:	nop
	li	$v0, 330
	sw	$v0, -80($fp)
	lw	$a0, -80($fp)
	li	$v0, 1			# $v0 gets print_int code for syscall
	syscall				# print
l3:	nop
	li	$v0, 4
	sw	$v0, -84($fp)
	li	$v0, 3
	sw	$v0, -88($fp)
	lw	$t1, -84($fp)
	lw	$t2, -88($fp)
	slt	$v0, $t2, $t1		# >
	sw	$v0, -92($fp)
	lw	$t1, -92($fp)
	slti	$v0, $t1, 1		# not
	sw	$v0, -96($fp)
	lw	$v0, -96($fp)
	beqz	$v0, l4
	li	$v0, 430
	sw	$v0, -100($fp)
	lw	$a0, -100($fp)
	li	$v0, 1			# $v0 gets print_int code for syscall
	syscall				# print
	j	l5
l4:	nop
	li	$v0, 10
	sw	$v0, -104($fp)
	lw	$a0, -104($fp)
	li	$v0, 1			# $v0 gets print_int code for syscall
	syscall				# print
l5:	nop
	li	$v0, 2
	sw	$v0, -108($fp)
	li	$v0, 3
	sw	$v0, -112($fp)
	lw	$t1, -108($fp)
	lw	$t2, -112($fp)
	sub	$v0, $t2, $t1		# >=
	slti	$v0, $v0, 1
	sw	$v0, -116($fp)
	lw	$t1, -116($fp)
	slti	$v0, $t1, 1		# not
	sw	$v0, -120($fp)
	lw	$v0, -120($fp)
	beqz	$v0, l6
	li	$v0, 10
	sw	$v0, -124($fp)
	lw	$a0, -124($fp)
	li	$v0, 1			# $v0 gets print_int code for syscall
	syscall				# print
	j	l7
l6:	nop
	li	$v0, 230
	sw	$v0, -128($fp)
	lw	$a0, -128($fp)
	li	$v0, 1			# $v0 gets print_int code for syscall
	syscall				# print
l7:	nop
	li	$v0, 3
	sw	$v0, -132($fp)
	li	$v0, 3
	sw	$v0, -136($fp)
	lw	$t1, -132($fp)
	lw	$t2, -136($fp)
	sub	$v0, $t2, $t1		# >=
	slti	$v0, $v0, 1
	sw	$v0, -140($fp)
	lw	$v0, -140($fp)
	beqz	$v0, l8
	li	$v0, 330
	sw	$v0, -144($fp)
	lw	$a0, -144($fp)
	li	$v0, 1			# $v0 gets print_int code for syscall
	syscall				# print
	j	l9
l8:	nop
	li	$v0, 10
	sw	$v0, -148($fp)
	lw	$a0, -148($fp)
	li	$v0, 1			# $v0 gets print_int code for syscall
	syscall				# print
l9:	nop
	li	$v0, 4
	sw	$v0, -152($fp)
	li	$v0, 3
	sw	$v0, -156($fp)
	lw	$t1, -152($fp)
	lw	$t2, -156($fp)
	sub	$v0, $t2, $t1		# >=
	slti	$v0, $v0, 1
	sw	$v0, -160($fp)
	lw	$v0, -160($fp)
	beqz	$v0, l10
	li	$v0, 430
	sw	$v0, -164($fp)
	lw	$a0, -164($fp)
	li	$v0, 1			# $v0 gets print_int code for syscall
	syscall				# print
	j	l11
l10:	nop
	li	$v0, 10
	sw	$v0, -168($fp)
	lw	$a0, -168($fp)
	li	$v0, 1			# $v0 gets print_int code for syscall
	syscall				# print
l11:	nop
	li	$v0, 2
	sw	$v0, -172($fp)
	li	$v0, 3
	sw	$v0, -176($fp)
	lw	$t1, -172($fp)
	lw	$t2, -176($fp)
	slt	$v0, $t1, $t2
	sw	$v0, -180($fp)
	lw	$v0, -180($fp)
	beqz	$v0, l12
	li	$v0, 230
	sw	$v0, -184($fp)
	lw	$a0, -184($fp)
	li	$v0, 1			# $v0 gets print_int code for syscall
	syscall				# print
	j	l13
l12:	nop
	li	$v0, 10
	sw	$v0, -188($fp)
	lw	$a0, -188($fp)
	li	$v0, 1			# $v0 gets print_int code for syscall
	syscall				# print
l13:	nop
	li	$v0, 3
	sw	$v0, -192($fp)
	li	$v0, 3
	sw	$v0, -196($fp)
	lw	$t1, -192($fp)
	lw	$t2, -196($fp)
	slt	$v0, $t1, $t2
	sw	$v0, -200($fp)
	lw	$v0, -200($fp)
	beqz	$v0, l14
	li	$v0, 10
	sw	$v0, -204($fp)
	lw	$a0, -204($fp)
	li	$v0, 1			# $v0 gets print_int code for syscall
	syscall				# print
	j	l15
l14:	nop
	li	$v0, 330
	sw	$v0, -208($fp)
	lw	$a0, -208($fp)
	li	$v0, 1			# $v0 gets print_int code for syscall
	syscall				# print
l15:	nop
	li	$v0, 4
	sw	$v0, -212($fp)
	li	$v0, 3
	sw	$v0, -216($fp)
	lw	$t1, -212($fp)
	lw	$t2, -216($fp)
	slt	$v0, $t1, $t2
	sw	$v0, -220($fp)
	lw	$v0, -220($fp)
	beqz	$v0, l16
	li	$v0, 10
	sw	$v0, -224($fp)
	lw	$a0, -224($fp)
	li	$v0, 1			# $v0 gets print_int code for syscall
	syscall				# print
	j	l17
l16:	nop
	li	$v0, 430
	sw	$v0, -228($fp)
	lw	$a0, -228($fp)
	li	$v0, 1			# $v0 gets print_int code for syscall
	syscall				# print
l17:	nop
	li	$v0, 2
	sw	$v0, -232($fp)
	li	$v0, 3
	sw	$v0, -236($fp)
	lw	$t1, -232($fp)
	lw	$t2, -236($fp)
	sub	$v0, $t1, $t2		# <=
	slti	$v0, $v0, 1
	sw	$v0, -240($fp)
	lw	$v0, -240($fp)
	beqz	$v0, l18
	li	$v0, 230
	sw	$v0, -244($fp)
	lw	$a0, -244($fp)
	li	$v0, 1			# $v0 gets print_int code for syscall
	syscall				# print
	j	l19
l18:	nop
	li	$v0, 10
	sw	$v0, -248($fp)
	lw	$a0, -248($fp)
	li	$v0, 1			# $v0 gets print_int code for syscall
	syscall				# print
l19:	nop
	li	$v0, 3
	sw	$v0, -252($fp)
	li	$v0, 3
	sw	$v0, -256($fp)
	lw	$t1, -252($fp)
	lw	$t2, -256($fp)
	sub	$v0, $t1, $t2		# <=
	slti	$v0, $v0, 1
	sw	$v0, -260($fp)
	lw	$v0, -260($fp)
	beqz	$v0, l20
	li	$v0, 330
	sw	$v0, -264($fp)
	lw	$a0, -264($fp)
	li	$v0, 1			# $v0 gets print_int code for syscall
	syscall				# print
	j	l21
l20:	nop
	li	$v0, 10
	sw	$v0, -268($fp)
	lw	$a0, -268($fp)
	li	$v0, 1			# $v0 gets print_int code for syscall
	syscall				# print
l21:	nop
	li	$v0, 4
	sw	$v0, -272($fp)
	li	$v0, 3
	sw	$v0, -276($fp)
	lw	$t1, -272($fp)
	lw	$t2, -276($fp)
	sub	$v0, $t1, $t2		# <=
	slti	$v0, $v0, 1
	sw	$v0, -280($fp)
	lw	$v0, -280($fp)
	beqz	$v0, l22
	li	$v0, 10
	sw	$v0, -284($fp)
	lw	$a0, -284($fp)
	li	$v0, 1			# $v0 gets print_int code for syscall
	syscall				# print
	j	l23
l22:	nop
	li	$v0, 430
	sw	$v0, -288($fp)
	lw	$a0, -288($fp)
	li	$v0, 1			# $v0 gets print_int code for syscall
	syscall				# print
l23:	nop
	li	$v0, 0
	sw	$v0, -292($fp)
	lw	$a0, -292($fp)
	move	$sp, $fp
	lw	$fp, 0($sp)		# pop restore fp
	addi	$sp, $sp, 4
	li	$v0, 17			# $v0 gets exit code for syscall
	syscall				# Exit here