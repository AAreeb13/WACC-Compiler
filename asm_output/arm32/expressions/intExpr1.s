./valid/expressions/intExpr1.wacc
calling the reference compiler on ./valid/expressions/intExpr1.wacc
-- Test: intExpr1.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating a moderately complex integer expression

# Output:
# Correct
#

# Program:

begin
  int a = ( 10 * 1 + 2 * 15 );
  if a == 40 then
    println "Correct"
  else
    println "Wrong"
  fi
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
intExpr1.s contents are:
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
17		mov r9, #1
18		mov r8, #10
19		smull r8, r9, r8, r9
20		@ Compare the sign of the lowest bits of the multiplication against the highest
21		cmp r9, r8, asr #31
22		blne _errOverflow
23		push {r8}
24		mov r9, #15
25		mov r8, #2
26		smull r8, r9, r8, r9
27		@ Compare the sign of the lowest bits of the multiplication against the highest
28		cmp r9, r8, asr #31
29		blne _errOverflow
30		push {r8}
31		pop {r9}
32		pop {r8}
33		adds r8, r8, r9
34		blvs _errOverflow
35		push {r8}
36		pop {r8}
37		mov r8, r8
38		mov r4, r8
39		cmp r4, #40
40		beq .L0
41		@ Stack pointer unchanged, no stack allocated arguments
42		ldr r8, =.L.str0
43		push {r8}
44		pop {r8}
45		mov r8, r8
46		mov r0, r8
47		@ statement primitives do not return results (but will clobber r0/rax)
48		bl _prints
49		bl _println
50		b .L1
51	.L0:
52		@ Stack pointer unchanged, no stack allocated arguments
53		ldr r8, =.L.str1
54		push {r8}
55		pop {r8}
56		mov r8, r8
57		mov r0, r8
58		@ statement primitives do not return results (but will clobber r0/rax)
59		bl _prints
60		bl _println
61	.L1:
62		@ Stack pointer unchanged, no stack allocated variables
63		mov r0, #0
64		pop {r4, r8, r10, r12}
65		pop {fp, pc}
66	
67	@ length of .L._prints_str0
68		.word 4
69	.L._prints_str0:
70		.asciz "%.*s"
71	.align 4
72	_prints:
73		push {fp, lr}
74		mov fp, sp
75		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
76		bic sp, sp, #0x7
77		mov r2, r0
78		ldr r1, [r0, #-4]
79		adr r0, .L._prints_str0
80		bl printf
81		mov r0, #0
82		bl fflush
83		mov sp, fp
84		pop {fp, pc}
85	
86	@ length of .L._println_str0
87		.word 0
88	.L._println_str0:
89		.asciz ""
90	.align 4
91	_println:
92		push {fp, lr}
93		mov fp, sp
94		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
95		bic sp, sp, #0x7
96		adr r0, .L._println_str0
97		bl puts
98		mov r0, #0
99		bl fflush
100		mov sp, fp
101		pop {fp, pc}
102	
103	_exit:
104		push {fp, lr}
105		mov fp, sp
106		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
107		bic sp, sp, #0x7
108		bl exit
109		mov sp, fp
110		pop {fp, pc}
111	
112	@ length of .L._errOverflow_str0
113		.word 52
114	.L._errOverflow_str0:
115		.asciz "fatal error: integer overflow or underflow occurred\n"
116	.align 4
117	_errOverflow:
118		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
119		bic sp, sp, #0x7
120		adr r0, .L._errOverflow_str0
121		bl _prints
122		mov r0, #255
123		bl _exit
===========================================================
-- Finished

