./valid/expressions/boolCalc.wacc
calling the reference compiler on ./valid/expressions/boolCalc.wacc
-- Test: boolCalc.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple boolean calculation

# Output:
# false
#

# Program:

begin
  bool b1 = true ;
  bool b2 = false ;
  bool b3 = b1 && b2 ;
  println b3
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
boolCalc.s contents are:
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
9		mov r8, #1
10		mov r4, r8
11		mov r8, #0
12		mov r5, r8
13		cmp r4, #1
14		bne .L0
15		cmp r5, #1
16	.L0:
17		moveq r8, #1
18		movne r8, #0
19		push {r8}
20		pop {r8}
21		mov r8, r8
22		mov r6, r8
23		@ Stack pointer unchanged, no stack allocated arguments
24		mov r8, r6
25		mov r0, r8
26		@ statement primitives do not return results (but will clobber r0/rax)
27		bl _printb
28		bl _println
29		@ Stack pointer unchanged, no stack allocated variables
30		mov r0, #0
31		pop {r4, r5, r6, r8, r10, r12}
32		pop {fp, pc}
33	
34	@ length of .L._printb_str0
35		.word 5
36	.L._printb_str0:
37		.asciz "false"
38	@ length of .L._printb_str1
39		.word 4
40	.L._printb_str1:
41		.asciz "true"
42	@ length of .L._printb_str2
43		.word 4
44	.L._printb_str2:
45		.asciz "%.*s"
46	.align 4
47	_printb:
48		push {fp, lr}
49		mov fp, sp
50		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
51		bic sp, sp, #0x7
52		cmp r0, #0
53		bne .L_printb0
54		adr r2, .L._printb_str0
55		b .L_printb1
56	.L_printb0:
57		adr r2, .L._printb_str1
58	.L_printb1:
59		ldr r1, [r2, #-4]
60		adr r0, .L._printb_str2
61		bl printf
62		mov r0, #0
63		bl fflush
64		mov sp, fp
65		pop {fp, pc}
66	
67	@ length of .L._println_str0
68		.word 0
69	.L._println_str0:
70		.asciz ""
71	.align 4
72	_println:
73		push {fp, lr}
74		mov fp, sp
75		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
76		bic sp, sp, #0x7
77		adr r0, .L._println_str0
78		bl puts
79		mov r0, #0
80		bl fflush
81		mov sp, fp
82		pop {fp, pc}
===========================================================
-- Finished

