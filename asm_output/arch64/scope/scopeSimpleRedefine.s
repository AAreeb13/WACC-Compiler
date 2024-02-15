./valid/scope/scopeSimpleRedefine.wacc
calling the reference compiler on ./valid/scope/scopeSimpleRedefine.wacc
-- Test: scopeSimpleRedefine.wacc

-- Uploaded file: 
---------------------------------------------------------------
# variable scoping test that redefines a variable

# Output:
# true
# 12
#

# Program:

begin
  int x = 12 ;
  begin
    bool x = true ;
    println x
  end ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
scopeSimpleRedefine.s contents are:
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
11		mov x8, #12
12		mov x19, x8
13		// Stack pointer unchanged, no stack allocated variables
14		mov x8, #1
15		mov x20, x8
16		// Stack pointer unchanged, no stack allocated arguments
17		mov x8, x20
18		mov x0, x8
19		// statement primitives do not return results (but will clobber r0/rax)
20		bl _printb
21		bl _println
22		// Stack pointer unchanged, no stack allocated variables
23		// Stack pointer unchanged, no stack allocated arguments
24		mov x8, x19
25		mov x0, x8
26		// statement primitives do not return results (but will clobber r0/rax)
27		bl _printi
28		bl _println
29		// Stack pointer unchanged, no stack allocated variables
30		mov x0, #0
31		// pop {x19, x20}
32		ldp x19, x20, [sp], #16
33		// pop {fp, lr}
34		ldp fp, lr, [sp], #16
35		ret
36	
37	// length of .L._printi_str0
38		.word 2
39	.L._printi_str0:
40		.asciz "%d"
41	.align 4
42	_printi:
43		// push {lr}
44		stp lr, xzr, [sp, #-16]!
45		mov x1, x0
46		adr x0, .L._printi_str0
47		bl printf
48		mov x0, #0
49		bl fflush
50		// pop {lr}
51		ldp lr, xzr, [sp], #16
52		ret
53	
54	// length of .L._printb_str0
55		.word 5
56	.L._printb_str0:
57		.asciz "false"
58	// length of .L._printb_str1
59		.word 4
60	.L._printb_str1:
61		.asciz "true"
62	// length of .L._printb_str2
63		.word 4
64	.L._printb_str2:
65		.asciz "%.*s"
66	.align 4
67	_printb:
68		// push {lr}
69		stp lr, xzr, [sp, #-16]!
70		cmp w0, #0
71		b.ne .L_printb0
72		adr x2, .L._printb_str0
73		b .L_printb1
74	.L_printb0:
75		adr x2, .L._printb_str1
76	.L_printb1:
77		ldrsw x1, [x2, #-4]
78		adr x0, .L._printb_str2
79		bl printf
80		mov x0, #0
81		bl fflush
82		// pop {lr}
83		ldp lr, xzr, [sp], #16
84		ret
85	
86	// length of .L._println_str0
87		.word 0
88	.L._println_str0:
89		.asciz ""
90	.align 4
91	_println:
92		// push {lr}
93		stp lr, xzr, [sp, #-16]!
94		adr x0, .L._println_str0
95		bl puts
96		mov x0, #0
97		bl fflush
98		// pop {lr}
99		ldp lr, xzr, [sp], #16
100		ret
===========================================================
-- Finished

