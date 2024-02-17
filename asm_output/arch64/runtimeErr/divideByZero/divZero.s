./valid/runtimeErr/divideByZero/divZero.wacc
calling the reference compiler on ./valid/runtimeErr/divideByZero/divZero.wacc
-- Test: divZero.wacc

-- Uploaded file: 
---------------------------------------------------------------
# division by zero

# Output:
# #runtime_error#

# Exit:
# 255

# Program:

begin
  int x = 10 / 0 ;
  println "should not reach here"
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
divZero.s contents are:
===========================================================
0	.data
1	// length of .L.str0
2		.word 21
3	.L.str0:
4		.asciz "should not reach here"
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
15		mov w9, #0
16		mov w8, #10
17		cbz x9, _errDivZero
18		sdiv w8, w8, w9
19		sxtw x8, w8
20		// push {x8}
21		stp x8, xzr, [sp, #-16]!
22		// pop {x8}
23		ldp x8, xzr, [sp], #16
24		mov x8, x8
25		mov x19, x8
26		// Stack pointer unchanged, no stack allocated arguments
27		adrp x8, .L.str0
28		add x8, x8, :lo12:.L.str0
29		// push {x8}
30		stp x8, xzr, [sp, #-16]!
31		// pop {x8}
32		ldp x8, xzr, [sp], #16
33		mov x8, x8
34		mov x0, x8
35		// statement primitives do not return results (but will clobber r0/rax)
36		bl _prints
37		bl _println
38		// Stack pointer unchanged, no stack allocated variables
39		mov x0, #0
40		// pop {x19}
41		ldp x19, xzr, [sp], #16
42		// pop {fp, lr}
43		ldp fp, lr, [sp], #16
44		ret
45	
46	// length of .L._prints_str0
47		.word 4
48	.L._prints_str0:
49		.asciz "%.*s"
50	.align 4
51	_prints:
52		// push {lr}
53		stp lr, xzr, [sp, #-16]!
54		mov x2, x0
55		ldrsw x1, [x0, #-4]
56		adr x0, .L._prints_str0
57		bl printf
58		mov x0, #0
59		bl fflush
60		// pop {lr}
61		ldp lr, xzr, [sp], #16
62		ret
63	
64	// length of .L._println_str0
65		.word 0
66	.L._println_str0:
67		.asciz ""
68	.align 4
69	_println:
70		// push {lr}
71		stp lr, xzr, [sp, #-16]!
72		adr x0, .L._println_str0
73		bl puts
74		mov x0, #0
75		bl fflush
76		// pop {lr}
77		ldp lr, xzr, [sp], #16
78		ret
79	
80	// length of .L._errDivZero_str0
81		.word 40
82	.L._errDivZero_str0:
83		.asciz "fatal error: division or modulo by zero\n"
84	.align 4
85	_errDivZero:
86		adr x0, .L._errDivZero_str0
87		bl _prints
88		mov w0, #-1
89		bl exit
===========================================================
-- Finished

