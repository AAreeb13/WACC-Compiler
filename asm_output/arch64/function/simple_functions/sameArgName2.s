./valid/function/simple_functions/sameArgName2.wacc
calling the reference compiler on ./valid/function/simple_functions/sameArgName2.wacc
-- Test: sameArgName2.wacc

-- Uploaded file: 
---------------------------------------------------------------
# program with function that has same parameter name as function
# and also returns to same named variable

# Output:
# 99
#

# Program:

begin
  int f(int f) is
    return f
  end

  int f = call f(99);
  println f
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
sameArgName2.s contents are:
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
11		// Stack pointer unchanged, no stack allocated arguments
12		mov x8, #99
13		mov x0, x8
14		bl wacc_f
15		mov x16, x0
16		// Stack pointer unchanged, no stack allocated arguments
17		mov x8, x16
18		mov x19, x8
19		// Stack pointer unchanged, no stack allocated arguments
20		mov x8, x19
21		mov x0, x8
22		// statement primitives do not return results (but will clobber r0/rax)
23		bl _printi
24		bl _println
25		// Stack pointer unchanged, no stack allocated variables
26		mov x0, #0
27		// pop {x19}
28		ldp x19, xzr, [sp], #16
29		// pop {fp, lr}
30		ldp fp, lr, [sp], #16
31		ret
32	
33	wacc_f:
34		// push {fp, lr}
35		stp fp, lr, [sp, #-16]!
36		mov fp, sp
37		mov x0, x0
38		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
39		mov sp, fp
40		// pop {fp, lr}
41		ldp fp, lr, [sp], #16
42		ret
43		// 'ere be dragons: this is 100% dead code, functions always end in returns!
44	
45	// length of .L._printi_str0
46		.word 2
47	.L._printi_str0:
48		.asciz "%d"
49	.align 4
50	_printi:
51		// push {lr}
52		stp lr, xzr, [sp, #-16]!
53		mov x1, x0
54		adr x0, .L._printi_str0
55		bl printf
56		mov x0, #0
57		bl fflush
58		// pop {lr}
59		ldp lr, xzr, [sp], #16
60		ret
61	
62	// length of .L._println_str0
63		.word 0
64	.L._println_str0:
65		.asciz ""
66	.align 4
67	_println:
68		// push {lr}
69		stp lr, xzr, [sp, #-16]!
70		adr x0, .L._println_str0
71		bl puts
72		mov x0, #0
73		bl fflush
74		// pop {lr}
75		ldp lr, xzr, [sp], #16
76		ret
===========================================================
-- Finished

