./valid/expressions/equalsOverOr.wacc
calling the reference compiler on ./valid/expressions/equalsOverOr.wacc
-- Test: equalsOverOr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating equality and boolean operator precedence

# Output:
# true
# false
#

# Program:

begin
  bool p = false ;
  bool q = true ;
  bool r = true ;
  println p == q || r ;
  println p == (q || r)
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
equalsOverOr.s contents are:
===========================================================
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		push {fp, lr}
6		push {r4, r5, r6, r8, r10, r12}
7		mov fp, sp
8		@ Stack pointer unchanged, no stack allocated variables
9		mov r8, #0
10		mov r4, r8
11		mov r8, #1
12		mov r5, r8
13		mov r8, #1
14		mov r6, r8
15		@ Stack pointer unchanged, no stack allocated arguments
16		cmp r4, r5
17		moveq r8, #1
18		movne r8, #0
19		push {r8}
20		pop {r8}
21		cmp r8, #1
22		beq .L0
23		cmp r6, #1
24	.L0:
25		moveq r8, #1
26		movne r8, #0
27		push {r8}
28		pop {r8}
29		mov r8, r8
30		mov r0, r8
31		@ statement primitives do not return results (but will clobber r0/rax)
32		bl _printb
33		bl _println
34		@ Stack pointer unchanged, no stack allocated arguments
35		cmp r5, #1
36		beq .L1
37		cmp r6, #1
38	.L1:
39		moveq r8, #1
40		movne r8, #0
41		push {r8}
42		pop {r9}
43		cmp r4, r9
44		moveq r8, #1
45		movne r8, #0
46		push {r8}
47		pop {r8}
48		mov r8, r8
49		mov r0, r8
50		@ statement primitives do not return results (but will clobber r0/rax)
51		bl _printb
52		bl _println
53		@ Stack pointer unchanged, no stack allocated variables
54		mov r0, #0
55		pop {r4, r5, r6, r8, r10, r12}
56		pop {fp, pc}
57	
58	@ length of .L._printb_str0
59		.word 5
60	.L._printb_str0:
61		.asciz "false"
62	@ length of .L._printb_str1
63		.word 4
64	.L._printb_str1:
65		.asciz "true"
66	@ length of .L._printb_str2
67		.word 4
68	.L._printb_str2:
69		.asciz "%.*s"
70	.align 4
71	_printb:
72		push {fp, lr}
73		mov fp, sp
74		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
75		bic sp, sp, #0x7
76		cmp r0, #0
77		bne .L_printb0
78		adr r2, .L._printb_str0
79		b .L_printb1
80	.L_printb0:
81		adr r2, .L._printb_str1
82	.L_printb1:
83		ldr r1, [r2, #-4]
84		adr r0, .L._printb_str2
85		bl printf
86		mov r0, #0
87		bl fflush
88		mov sp, fp
89		pop {fp, pc}
90	
91	@ length of .L._println_str0
92		.word 0
93	.L._println_str0:
94		.asciz ""
95	.align 4
96	_println:
97		push {fp, lr}
98		mov fp, sp
99		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
100		bic sp, sp, #0x7
101		adr r0, .L._println_str0
102		bl puts
103		mov r0, #0
104		bl fflush
105		mov sp, fp
106		pop {fp, pc}
===========================================================
-- Finished

