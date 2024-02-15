./valid/sequence/intLeadingZeros.wacc
calling the reference compiler on ./valid/sequence/intLeadingZeros.wacc
-- Test: intLeadingZeros.wacc

-- Uploaded file: 
---------------------------------------------------------------
# integer variable declaration with leading zeroes

# Output:
# 42
# 0
#

# Program:

begin
  int x = 0000000000000000000000000000000000000042 ;
  int y = 0000000000000000000000000000000000000000 ;
  println x ;
  println y
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
intLeadingZeros.s contents are:
===========================================================
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		// push {fp, lr}
6		stp fp, lr, [sp, #-16]!
7		// push {x19, x20}
8		stp x19, x20, [sp, #-16]!
9		mov fp, sp
10		// Stack pointer unchanged, no stack allocated variables
11		mov x8, #42
12		mov x19, x8
13		mov x8, #0
14		mov x20, x8
15		// Stack pointer unchanged, no stack allocated arguments
16		mov x8, x19
17		mov x0, x8
18		// statement primitives do not return results (but will clobber r0/rax)
19		bl _printi
20		bl _println
21		// Stack pointer unchanged, no stack allocated arguments
22		mov x8, x20
23		mov x0, x8
24		// statement primitives do not return results (but will clobber r0/rax)
25		bl _printi
26		bl _println
27		// Stack pointer unchanged, no stack allocated variables
28		mov x0, #0
29		// pop {x19, x20}
30		ldp x19, x20, [sp], #16
31		// pop {fp, lr}
32		ldp fp, lr, [sp], #16
33		ret
34	
35	// length of .L._printi_str0
36		.word 2
37	.L._printi_str0:
38		.asciz "%d"
39	.align 4
40	_printi:
41		// push {lr}
42		stp lr, xzr, [sp, #-16]!
43		mov x1, x0
44		adr x0, .L._printi_str0
45		bl printf
46		mov x0, #0
47		bl fflush
48		// pop {lr}
49		ldp lr, xzr, [sp], #16
50		ret
51	
52	// length of .L._println_str0
53		.word 0
54	.L._println_str0:
55		.asciz ""
56	.align 4
57	_println:
58		// push {lr}
59		stp lr, xzr, [sp, #-16]!
60		adr x0, .L._println_str0
61		bl puts
62		mov x0, #0
63		bl fflush
64		// pop {lr}
65		ldp lr, xzr, [sp], #16
66		ret
===========================================================
-- Finished

