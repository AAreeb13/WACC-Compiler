./valid/expressions/boolExpr1.wacc
calling the reference compiler on ./valid/expressions/boolExpr1.wacc
-- Test: boolExpr1.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating a moderately complex boolean expression

# Output:
# Correct
#

# Program:

begin
  bool b = ! ( ( true && false) || (true && false) );
  if b == true then
    println "Correct"
  else
    println "Wrong"
  fi
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
boolExpr1.s contents are:
===========================================================
0	.data
1	@ length of .L.str0
2		.word 5
3	.L.str0:
4		.asciz "Wrong"
5	@ length of .L.str1
6		.word 7
7	.L.str1:
8		.asciz "Correct"
9	.align 4
10	.text
11	.global main
12	main:
13		push {fp, lr}
14		push {r4, r8, r10, r12}
15		mov fp, sp
16		@ Stack pointer unchanged, no stack allocated variables
17		mov r8, #1
18		cmp r8, #1
19		bne .L0
20		mov r8, #0
21		cmp r8, #1
22	.L0:
23		moveq r8, #1
24		movne r8, #0
25		push {r8}
26		pop {r8}
27		cmp r8, #1
28		beq .L1
29		mov r8, #1
30		cmp r8, #1
31		bne .L2
32		mov r8, #0
33		cmp r8, #1
34	.L2:
35		moveq r8, #1
36		movne r8, #0
37		push {r8}
38		pop {r8}
39		cmp r8, #1
40	.L1:
41		movne r8, #1
42		moveq r8, #0
43		push {r8}
44		pop {r8}
45		mov r8, r8
46		mov r4, r8
47		cmp r4, #1
48		beq .L3
49		@ Stack pointer unchanged, no stack allocated arguments
50		ldr r8, =.L.str0
51		push {r8}
52		pop {r8}
53		mov r8, r8
54		mov r0, r8
55		@ statement primitives do not return results (but will clobber r0/rax)
56		bl _prints
57		bl _println
58		b .L4
59	.L3:
60		@ Stack pointer unchanged, no stack allocated arguments
61		ldr r8, =.L.str1
62		push {r8}
63		pop {r8}
64		mov r8, r8
65		mov r0, r8
66		@ statement primitives do not return results (but will clobber r0/rax)
67		bl _prints
68		bl _println
69	.L4:
70		@ Stack pointer unchanged, no stack allocated variables
71		mov r0, #0
72		pop {r4, r8, r10, r12}
73		pop {fp, pc}
74	
75	@ length of .L._prints_str0
76		.word 4
77	.L._prints_str0:
78		.asciz "%.*s"
79	.align 4
80	_prints:
81		push {fp, lr}
82		mov fp, sp
83		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
84		bic sp, sp, #0x7
85		mov r2, r0
86		ldr r1, [r0, #-4]
87		adr r0, .L._prints_str0
88		bl printf
89		mov r0, #0
90		bl fflush
91		mov sp, fp
92		pop {fp, pc}
93	
94	@ length of .L._println_str0
95		.word 0
96	.L._println_str0:
97		.asciz ""
98	.align 4
99	_println:
100		push {fp, lr}
101		mov fp, sp
102		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
103		bic sp, sp, #0x7
104		adr r0, .L._println_str0
105		bl puts
106		mov r0, #0
107		bl fflush
108		mov sp, fp
109		pop {fp, pc}
===========================================================
-- Finished

