./valid/pairs/null.wacc
calling the reference compiler on ./valid/pairs/null.wacc
-- Test: null.wacc

-- Uploaded file: 
---------------------------------------------------------------
# null pair assignment

# Output:
# (nil)
# (nil)
#

# Program:

begin
  pair(pair, pair) p = null ;
  println p ;
  p = null ;
  println p
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
null.s contents are:
===========================================================
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		// push {fp, lr}
6		stp fp, lr, [sp, #-16]!
7		// push {x19}
8		stp x19, xzr, [sp, #-16]!
9		mov fp, sp
10		// Stack pointer unchanged, no stack allocated variables
11		mov x8, #0
12		mov x19, x8
13		// Stack pointer unchanged, no stack allocated arguments
14		mov x8, x19
15		mov x0, x8
16		// statement primitives do not return results (but will clobber r0/rax)
17		bl _printp
18		bl _println
19		mov x8, #0
20		mov x19, x8
21		// Stack pointer unchanged, no stack allocated arguments
22		mov x8, x19
23		mov x0, x8
24		// statement primitives do not return results (but will clobber r0/rax)
25		bl _printp
26		bl _println
27		// Stack pointer unchanged, no stack allocated variables
28		mov x0, #0
29		// pop {x19}
30		ldp x19, xzr, [sp], #16
31		// pop {fp, lr}
32		ldp fp, lr, [sp], #16
33		ret
34	
35	// length of .L._printp_str0
36		.word 2
37	.L._printp_str0:
38		.asciz "%p"
39	.align 4
40	_printp:
41		// push {lr}
42		stp lr, xzr, [sp, #-16]!
43		mov x1, x0
44		adr x0, .L._printp_str0
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

