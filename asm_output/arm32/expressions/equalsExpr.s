./valid/expressions/equalsExpr.wacc
calling the reference compiler on ./valid/expressions/equalsExpr.wacc
-- Test: equalsExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating equality

# Output:
# false
# false
# true
#

# Program:

begin
  int x = 2 ;
  int y = 4 ;
  int z = 4 ;
  bool b = x == y ;
  println b ;
  println x == y ;
  println y == z
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
equalsExpr.s contents are:
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
9		mov r8, #2
10		mov r4, r8
11		mov r8, #4
12		mov r5, r8
13		mov r8, #4
14		mov r6, r8
15		cmp r4, r5
16		moveq r8, #1
17		movne r8, #0
18		push {r8}
19		pop {r8}
20		mov r8, r8
21		mov r7, r8
22		@ Stack pointer unchanged, no stack allocated arguments
23		mov r8, r7
24		mov r0, r8
25		@ statement primitives do not return results (but will clobber r0/rax)
26		bl _printb
27		bl _println
28		@ Stack pointer unchanged, no stack allocated arguments
29		cmp r4, r5
30		moveq r8, #1
31		movne r8, #0
32		push {r8}
33		pop {r8}
34		mov r8, r8
35		mov r0, r8
36		@ statement primitives do not return results (but will clobber r0/rax)
37		bl _printb
38		bl _println
39		@ Stack pointer unchanged, no stack allocated arguments
40		cmp r5, r6
41		moveq r8, #1
42		movne r8, #0
43		push {r8}
44		pop {r8}
45		mov r8, r8
46		mov r0, r8
47		@ statement primitives do not return results (but will clobber r0/rax)
48		bl _printb
49		bl _println
50		@ Stack pointer unchanged, no stack allocated variables
51		mov r0, #0
52		pop {r4, r5, r6, r7, r8, r10, r12}
53		pop {fp, pc}
54	
55	@ length of .L._printb_str0
56		.word 5
57	.L._printb_str0:
58		.asciz "false"
59	@ length of .L._printb_str1
60		.word 4
61	.L._printb_str1:
62		.asciz "true"
63	@ length of .L._printb_str2
64		.word 4
65	.L._printb_str2:
66		.asciz "%.*s"
67	.align 4
68	_printb:
69		push {fp, lr}
70		mov fp, sp
71		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
72		bic sp, sp, #0x7
73		cmp r0, #0
74		bne .L_printb0
75		adr r2, .L._printb_str0
76		b .L_printb1
77	.L_printb0:
78		adr r2, .L._printb_str1
79	.L_printb1:
80		ldr r1, [r2, #-4]
81		adr r0, .L._printb_str2
82		bl printf
83		mov r0, #0
84		bl fflush
85		mov sp, fp
86		pop {fp, pc}
87	
88	@ length of .L._println_str0
89		.word 0
90	.L._println_str0:
91		.asciz ""
92	.align 4
93	_println:
94		push {fp, lr}
95		mov fp, sp
96		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
97		bic sp, sp, #0x7
98		adr r0, .L._println_str0
99		bl puts
100		mov r0, #0
101		bl fflush
102		mov sp, fp
103		pop {fp, pc}
===========================================================
-- Finished

