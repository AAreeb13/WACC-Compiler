./valid/expressions/equalsOverBool.wacc
calling the reference compiler on ./valid/expressions/equalsOverBool.wacc
-- Test: equalsOverBool.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating equality and boolean operator precedence

# Output:
# true
# false
#

# Program:

begin
  bool p = true ;
  bool q = true ;
  bool r = false ;
  bool s = true ;
  println p && q != r || s ;
  println (p && q) != (r || s)
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
equalsOverBool.s contents are:
===========================================================
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		push {fp, lr}
6		push {r4, r5, r6, r7, r8, r10, r12}
7		mov fp, sp
8		@ Stack pointer unchanged, no stack allocated variables
9		mov r8, #1
10		mov r4, r8
11		mov r8, #1
12		mov r5, r8
13		mov r8, #0
14		mov r6, r8
15		mov r8, #1
16		mov r7, r8
17		@ Stack pointer unchanged, no stack allocated arguments
18		cmp r4, #1
19		bne .L0
20		cmp r5, r6
21		movne r8, #1
22		moveq r8, #0
23		push {r8}
24		pop {r8}
25		cmp r8, #1
26	.L0:
27		moveq r8, #1
28		movne r8, #0
29		push {r8}
30		pop {r8}
31		cmp r8, #1
32		beq .L1
33		cmp r7, #1
34	.L1:
35		moveq r8, #1
36		movne r8, #0
37		push {r8}
38		pop {r8}
39		mov r8, r8
40		mov r0, r8
41		@ statement primitives do not return results (but will clobber r0/rax)
42		bl _printb
43		bl _println
44		@ Stack pointer unchanged, no stack allocated arguments
45		cmp r4, #1
46		bne .L2
47		cmp r5, #1
48	.L2:
49		moveq r8, #1
50		movne r8, #0
51		push {r8}
52		cmp r6, #1
53		beq .L3
54		cmp r7, #1
55	.L3:
56		moveq r8, #1
57		movne r8, #0
58		push {r8}
59		pop {r9}
60		pop {r8}
61		cmp r8, r9
62		movne r8, #1
63		moveq r8, #0
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

