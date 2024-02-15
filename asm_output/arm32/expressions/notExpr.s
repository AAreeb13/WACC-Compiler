./valid/expressions/notExpr.wacc
calling the reference compiler on ./valid/expressions/notExpr.wacc
-- Test: notExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating not

# Output:
# false
# true
#

# Program:

begin
  bool a = true ;
  bool b = false ;
  println !a ;
  println !b
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
notExpr.s contents are:
===========================================================
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		push {fp, lr}
6		push {r4, r5, r8, r10, r12}
7		mov fp, sp
8		@ Stack pointer unchanged, no stack allocated variables
9		mov r8, #1
10		mov r4, r8
11		mov r8, #0
12		mov r5, r8
13		@ Stack pointer unchanged, no stack allocated arguments
14		cmp r4, #1
15		movne r8, #1
16		moveq r8, #0
17		push {r8}
18		pop {r8}
19		mov r8, r8
20		mov r0, r8
21		@ statement primitives do not return results (but will clobber r0/rax)
22		bl _printb
23		bl _println
24		@ Stack pointer unchanged, no stack allocated arguments
25		cmp r5, #1
26		movne r8, #1
27		moveq r8, #0
28		push {r8}
29		pop {r8}
30		mov r8, r8
31		mov r0, r8
32		@ statement primitives do not return results (but will clobber r0/rax)
33		bl _printb
34		bl _println
35		@ Stack pointer unchanged, no stack allocated variables
36		mov r0, #0
37		pop {r4, r5, r8, r10, r12}
38		pop {fp, pc}
39	
40	@ length of .L._printb_str0
41		.word 5
42	.L._printb_str0:
43		.asciz "false"
44	@ length of .L._printb_str1
45		.word 4
46	.L._printb_str1:
47		.asciz "true"
48	@ length of .L._printb_str2
49		.word 4
50	.L._printb_str2:
51		.asciz "%.*s"
52	.align 4
53	_printb:
54		push {fp, lr}
55		mov fp, sp
56		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
57		bic sp, sp, #0x7
58		cmp r0, #0
59		bne .L_printb0
60		adr r2, .L._printb_str0
61		b .L_printb1
62	.L_printb0:
63		adr r2, .L._printb_str1
64	.L_printb1:
65		ldr r1, [r2, #-4]
66		adr r0, .L._printb_str2
67		bl printf
68		mov r0, #0
69		bl fflush
70		mov sp, fp
71		pop {fp, pc}
72	
73	@ length of .L._println_str0
74		.word 0
75	.L._println_str0:
76		.asciz ""
77	.align 4
78	_println:
79		push {fp, lr}
80		mov fp, sp
81		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
82		bic sp, sp, #0x7
83		adr r0, .L._println_str0
84		bl puts
85		mov r0, #0
86		bl fflush
87		mov sp, fp
88		pop {fp, pc}
===========================================================
-- Finished

