./valid/expressions/stringEqualsExpr.wacc
calling the reference compiler on ./valid/expressions/stringEqualsExpr.wacc
-- Test: stringEqualsExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating string equality

# Output:
# true
# false
# false
#

# Program:

begin
  string s1 = "Hello" ;
  string s2 = "foo" ;
  string s3 = "bar" ;
  bool b = s1 == s1 ;
  println b ;
  println s1 == s2 ;
  println s2 == s3
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
stringEqualsExpr.s contents are:
===========================================================
0	.data
1	@ length of .L.str0
2		.word 5
3	.L.str0:
4		.asciz "Hello"
5	@ length of .L.str1
6		.word 3
7	.L.str1:
8		.asciz "foo"
9	@ length of .L.str2
10		.word 3
11	.L.str2:
12		.asciz "bar"
13	.align 4
14	.text
15	.global main
16	main:
17		push {fp, lr}
18		push {r4, r5, r6, r7, r8, r10, r12}
19		mov fp, sp
20		@ Stack pointer unchanged, no stack allocated variables
21		ldr r8, =.L.str0
22		push {r8}
23		pop {r8}
24		mov r8, r8
25		mov r4, r8
26		ldr r8, =.L.str1
27		push {r8}
28		pop {r8}
29		mov r8, r8
30		mov r5, r8
31		ldr r8, =.L.str2
32		push {r8}
33		pop {r8}
34		mov r8, r8
35		mov r6, r8
36		cmp r4, r4
37		moveq r8, #1
38		movne r8, #0
39		push {r8}
40		pop {r8}
41		mov r8, r8
42		mov r7, r8
43		@ Stack pointer unchanged, no stack allocated arguments
44		mov r8, r7
45		mov r0, r8
46		@ statement primitives do not return results (but will clobber r0/rax)
47		bl _printb
48		bl _println
49		@ Stack pointer unchanged, no stack allocated arguments
50		cmp r4, r5
51		moveq r8, #1
52		movne r8, #0
53		push {r8}
54		pop {r8}
55		mov r8, r8
56		mov r0, r8
57		@ statement primitives do not return results (but will clobber r0/rax)
58		bl _printb
59		bl _println
60		@ Stack pointer unchanged, no stack allocated arguments
61		cmp r5, r6
62		moveq r8, #1
63		movne r8, #0
64		push {r8}
65		pop {r8}
66		mov r8, r8
67		mov r0, r8
68		@ statement primitives do not return results (but will clobber r0/rax)
69		bl _printb
70		bl _println
71		@ Stack pointer unchanged, no stack allocated variables
72		mov r0, #0
73		pop {r4, r5, r6, r7, r8, r10, r12}
74		pop {fp, pc}
75	
76	@ length of .L._printb_str0
77		.word 5
78	.L._printb_str0:
79		.asciz "false"
80	@ length of .L._printb_str1
81		.word 4
82	.L._printb_str1:
83		.asciz "true"
84	@ length of .L._printb_str2
85		.word 4
86	.L._printb_str2:
87		.asciz "%.*s"
88	.align 4
89	_printb:
90		push {fp, lr}
91		mov fp, sp
92		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
93		bic sp, sp, #0x7
94		cmp r0, #0
95		bne .L_printb0
96		adr r2, .L._printb_str0
97		b .L_printb1
98	.L_printb0:
99		adr r2, .L._printb_str1
100	.L_printb1:
101		ldr r1, [r2, #-4]
102		adr r0, .L._printb_str2
103		bl printf
104		mov r0, #0
105		bl fflush
106		mov sp, fp
107		pop {fp, pc}
108	
109	@ length of .L._println_str0
110		.word 0
111	.L._println_str0:
112		.asciz ""
113	.align 4
114	_println:
115		push {fp, lr}
116		mov fp, sp
117		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
118		bic sp, sp, #0x7
119		adr r0, .L._println_str0
120		bl puts
121		mov r0, #0
122		bl fflush
123		mov sp, fp
124		pop {fp, pc}
===========================================================
-- Finished

