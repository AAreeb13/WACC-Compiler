./valid/pairs/printNullPair.wacc
calling the reference compiler on ./valid/pairs/printNullPair.wacc
-- Test: printNullPair.wacc

-- Uploaded file: 
---------------------------------------------------------------
# print pair a null pair

# Output:
# (nil)
#

# Program:

begin
  pair(pair, pair) p = null ;
  println p
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
printNullPair.s contents are:
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
19		// Stack pointer unchanged, no stack allocated variables
20		mov x0, #0
21		// pop {x19}
22		ldp x19, xzr, [sp], #16
23		// pop {fp, lr}
24		ldp fp, lr, [sp], #16
25		ret
26	
27	// length of .L._printp_str0
28		.word 2
29	.L._printp_str0:
30		.asciz "%p"
31	.align 4
32	_printp:
33		// push {lr}
34		stp lr, xzr, [sp, #-16]!
35		mov x1, x0
36		adr x0, .L._printp_str0
37		bl printf
38		mov x0, #0
39		bl fflush
40		// pop {lr}
41		ldp lr, xzr, [sp], #16
42		ret
43	
44	// length of .L._println_str0
45		.word 0
46	.L._println_str0:
47		.asciz ""
48	.align 4
49	_println:
50		// push {lr}
51		stp lr, xzr, [sp, #-16]!
52		adr x0, .L._println_str0
53		bl puts
54		mov x0, #0
55		bl fflush
56		// pop {lr}
57		ldp lr, xzr, [sp], #16
58		ret
===========================================================
-- Finished

