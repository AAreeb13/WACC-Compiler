./valid/expressions/lessCharExpr.wacc
calling the reference compiler on ./valid/expressions/lessCharExpr.wacc
-- Test: lessCharExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating less-than on characters

# Output:
# true
# false
#

# Program:

begin
  char a = 'a' ;
  char e = 'e' ;
  char c = 'c' ;
  println a < e ;
  println e < c
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
lessCharExpr.s contents are:
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
9		mov r8, #97
10		mov r4, r8
11		mov r8, #101
12		mov r5, r8
13		mov r8, #99
14		mov r6, r8
15		@ Stack pointer unchanged, no stack allocated arguments
16		cmp r4, r5
17		movlt r8, #1
18		movge r8, #0
19		push {r8}
20		pop {r8}
21		mov r8, r8
22		mov r0, r8
23		@ statement primitives do not return results (but will clobber r0/rax)
24		bl _printb
25		bl _println
26		@ Stack pointer unchanged, no stack allocated arguments
27		cmp r5, r6
28		movlt r8, #1
29		movge r8, #0
30		push {r8}
31		pop {r8}
32		mov r8, r8
33		mov r0, r8
34		@ statement primitives do not return results (but will clobber r0/rax)
35		bl _printb
36		bl _println
37		@ Stack pointer unchanged, no stack allocated variables
38		mov r0, #0
39		pop {r4, r5, r6, r8, r10, r12}
40		pop {fp, pc}
41	
42	@ length of .L._printb_str0
43		.word 5
44	.L._printb_str0:
45		.asciz "false"
46	@ length of .L._printb_str1
47		.word 4
48	.L._printb_str1:
49		.asciz "true"
50	@ length of .L._printb_str2
51		.word 4
52	.L._printb_str2:
53		.asciz "%.*s"
54	.align 4
55	_printb:
56		push {fp, lr}
57		mov fp, sp
58		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
59		bic sp, sp, #0x7
60		cmp r0, #0
61		bne .L_printb0
62		adr r2, .L._printb_str0
63		b .L_printb1
64	.L_printb0:
65		adr r2, .L._printb_str1
66	.L_printb1:
67		ldr r1, [r2, #-4]
68		adr r0, .L._printb_str2
69		bl printf
70		mov r0, #0
71		bl fflush
72		mov sp, fp
73		pop {fp, pc}
74	
75	@ length of .L._println_str0
76		.word 0
77	.L._println_str0:
78		.asciz ""
79	.align 4
80	_println:
81		push {fp, lr}
82		mov fp, sp
83		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
84		bic sp, sp, #0x7
85		adr r0, .L._println_str0
86		bl puts
87		mov r0, #0
88		bl fflush
89		mov sp, fp
90		pop {fp, pc}
===========================================================
-- Finished

