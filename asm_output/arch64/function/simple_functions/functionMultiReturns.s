./valid/function/simple_functions/functionMultiReturns.wacc
calling the reference compiler on ./valid/function/simple_functions/functionMultiReturns.wacc
-- Test: functionMultiReturns.wacc

-- Uploaded file: 
---------------------------------------------------------------
# a simple function with multiple returns, importantly one at the end.

# Output:
# 1
#

# Program:

begin
  int returnInWhile() is
    while true do
      return 1 ;
      println "How on Earth did we get here?"
    done ;
    return 2
  end

  int x = call returnInWhile() ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
functionMultiReturns.s contents are:
===========================================================
0	.data
1	// length of .L.str0
2		.word 29
3	.L.str0:
4		.asciz "How on Earth did we get here?"
5	.align 4
6	.text
7	.global main
8	main:
9		// push {fp, lr}
10		stp fp, lr, [sp, #-16]!
11		// push {x19}
12		stp x19, xzr, [sp, #-16]!
13		mov fp, sp
14		// Stack pointer unchanged, no stack allocated variables
15		// Stack pointer unchanged, no stack allocated arguments
16		bl wacc_returnInWhile
17		mov x16, x0
18		// Stack pointer unchanged, no stack allocated arguments
19		mov x8, x16
20		mov x19, x8
21		// Stack pointer unchanged, no stack allocated arguments
22		mov x8, x19
23		mov x0, x8
24		// statement primitives do not return results (but will clobber r0/rax)
25		bl _printi
26		bl _println
27		// Stack pointer unchanged, no stack allocated variables
28		mov x0, #0
29		// pop {x19}
30		ldp x19, xzr, [sp], #16
31		// pop {fp, lr}
32		ldp fp, lr, [sp], #16
33		ret
34	
35	wacc_returnInWhile:
36		// push {fp, lr}
37		stp fp, lr, [sp, #-16]!
38		mov fp, sp
39		b .L0
40	.L1:
41		mov x0, #1
42		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
43		mov sp, fp
44		// pop {fp, lr}
45		ldp fp, lr, [sp], #16
46		ret
47		// Stack pointer unchanged, no stack allocated arguments
48		adrp x8, .L.str0
49		add x8, x8, :lo12:.L.str0
50		// push {x8}
51		stp x8, xzr, [sp, #-16]!
52		// pop {x8}
53		ldp x8, xzr, [sp], #16
54		mov x8, x8
55		mov x0, x8
56		// statement primitives do not return results (but will clobber r0/rax)
57		bl _prints
58		bl _println
59	.L0:
60		mov x8, #1
61		cmp x8, #1
62		b.eq .L1
63		mov x0, #2
64		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
65		mov sp, fp
66		// pop {fp, lr}
67		ldp fp, lr, [sp], #16
68		ret
69		// 'ere be dragons: this is 100% dead code, functions always end in returns!
70	
71	// length of .L._prints_str0
72		.word 4
73	.L._prints_str0:
74		.asciz "%.*s"
75	.align 4
76	_prints:
77		// push {lr}
78		stp lr, xzr, [sp, #-16]!
79		mov x2, x0
80		ldrsw x1, [x0, #-4]
81		adr x0, .L._prints_str0
82		bl printf
83		mov x0, #0
84		bl fflush
85		// pop {lr}
86		ldp lr, xzr, [sp], #16
87		ret
88	
89	// length of .L._printi_str0
90		.word 2
91	.L._printi_str0:
92		.asciz "%d"
93	.align 4
94	_printi:
95		// push {lr}
96		stp lr, xzr, [sp, #-16]!
97		mov x1, x0
98		adr x0, .L._printi_str0
99		bl printf
100		mov x0, #0
101		bl fflush
102		// pop {lr}
103		ldp lr, xzr, [sp], #16
104		ret
105	
106	// length of .L._println_str0
107		.word 0
108	.L._println_str0:
109		.asciz ""
110	.align 4
111	_println:
112		// push {lr}
113		stp lr, xzr, [sp, #-16]!
114		adr x0, .L._println_str0
115		bl puts
116		mov x0, #0
117		bl fflush
118		// pop {lr}
119		ldp lr, xzr, [sp], #16
120		ret
===========================================================
-- Finished

