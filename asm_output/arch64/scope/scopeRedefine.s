./valid/scope/scopeRedefine.wacc
calling the reference compiler on ./valid/scope/scopeRedefine.wacc
-- Test: scopeRedefine.wacc

-- Uploaded file: 
---------------------------------------------------------------
# more complex variable scoping test that redefines a variable

# Output:
# true
# 2
#

# Program:

begin
  int x = 1 ;
  begin
    x = 2 ;
    bool x = true ;
    println x
  end ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
scopeRedefine.s contents are:
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
11		mov x8, #1
12		mov x19, x8
13		// Stack pointer unchanged, no stack allocated variables
14		mov x8, #2
15		mov x19, x8
16		mov x8, #1
17		mov x20, x8
18		// Stack pointer unchanged, no stack allocated arguments
19		mov x8, x20
20		mov x0, x8
21		// statement primitives do not return results (but will clobber r0/rax)
22		bl _printb
23		bl _println
24		// Stack pointer unchanged, no stack allocated variables
25		// Stack pointer unchanged, no stack allocated arguments
26		mov x8, x19
27		mov x0, x8
28		// statement primitives do not return results (but will clobber r0/rax)
29		bl _printi
30		bl _println
31		// Stack pointer unchanged, no stack allocated variables
32		mov x0, #0
33		// pop {x19, x20}
34		ldp x19, x20, [sp], #16
35		// pop {fp, lr}
36		ldp fp, lr, [sp], #16
37		ret
38	
39	// length of .L._printi_str0
40		.word 2
41	.L._printi_str0:
42		.asciz "%d"
43	.align 4
44	_printi:
45		// push {lr}
46		stp lr, xzr, [sp, #-16]!
47		mov x1, x0
48		adr x0, .L._printi_str0
49		bl printf
50		mov x0, #0
51		bl fflush
52		// pop {lr}
53		ldp lr, xzr, [sp], #16
54		ret
55	
56	// length of .L._printb_str0
57		.word 5
58	.L._printb_str0:
59		.asciz "false"
60	// length of .L._printb_str1
61		.word 4
62	.L._printb_str1:
63		.asciz "true"
64	// length of .L._printb_str2
65		.word 4
66	.L._printb_str2:
67		.asciz "%.*s"
68	.align 4
69	_printb:
70		// push {lr}
71		stp lr, xzr, [sp, #-16]!
72		cmp w0, #0
73		b.ne .L_printb0
74		adr x2, .L._printb_str0
75		b .L_printb1
76	.L_printb0:
77		adr x2, .L._printb_str1
78	.L_printb1:
79		ldrsw x1, [x2, #-4]
80		adr x0, .L._printb_str2
81		bl printf
82		mov x0, #0
83		bl fflush
84		// pop {lr}
85		ldp lr, xzr, [sp], #16
86		ret
87	
88	// length of .L._println_str0
89		.word 0
90	.L._println_str0:
91		.asciz ""
92	.align 4
93	_println:
94		// push {lr}
95		stp lr, xzr, [sp, #-16]!
96		adr x0, .L._println_str0
97		bl puts
98		mov x0, #0
99		bl fflush
100		// pop {lr}
101		ldp lr, xzr, [sp], #16
102		ret
===========================================================
-- Finished

