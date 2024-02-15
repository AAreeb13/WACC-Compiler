./valid/if/whitespace.wacc
calling the reference compiler on ./valid/if/whitespace.wacc
-- Test: whitespace.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Whitespace only important between keyword and variable tokens

# Output:
# 1
#

# Program:

begin
	int a=13;
  if a==13then a=1else a=0fi;
  println a
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
whitespace.s contents are:
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
11		mov x8, #13
12		mov x19, x8
13		cmp x19, #13
14		b.eq .L0
15		mov x8, #0
16		mov x19, x8
17		b .L1
18	.L0:
19		mov x8, #1
20		mov x19, x8
21	.L1:
22		// Stack pointer unchanged, no stack allocated arguments
23		mov x8, x19
24		mov x0, x8
25		// statement primitives do not return results (but will clobber r0/rax)
26		bl _printi
27		bl _println
28		// Stack pointer unchanged, no stack allocated variables
29		mov x0, #0
30		// pop {x19}
31		ldp x19, xzr, [sp], #16
32		// pop {fp, lr}
33		ldp fp, lr, [sp], #16
34		ret
35	
36	// length of .L._printi_str0
37		.word 2
38	.L._printi_str0:
39		.asciz "%d"
40	.align 4
41	_printi:
42		// push {lr}
43		stp lr, xzr, [sp, #-16]!
44		mov x1, x0
45		adr x0, .L._printi_str0
46		bl printf
47		mov x0, #0
48		bl fflush
49		// pop {lr}
50		ldp lr, xzr, [sp], #16
51		ret
52	
53	// length of .L._println_str0
54		.word 0
55	.L._println_str0:
56		.asciz ""
57	.align 4
58	_println:
59		// push {lr}
60		stp lr, xzr, [sp, #-16]!
61		adr x0, .L._println_str0
62		bl puts
63		mov x0, #0
64		bl fflush
65		// pop {lr}
66		ldp lr, xzr, [sp], #16
67		ret
===========================================================
-- Finished

