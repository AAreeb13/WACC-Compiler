./valid/runtimeErr/integerOverflow/intJustOverflow.wacc
calling the reference compiler on ./valid/runtimeErr/integerOverflow/intJustOverflow.wacc
-- Test: intJustOverflow.wacc

-- Uploaded file: 
---------------------------------------------------------------
# integer overflow

# Output:
# 2147483646
# 2147483647
# #runtime_error#

# Exit:
# 255

# Program:

begin
  int x = 2147483646 ;
  println x ;
  x = x + 1 ;
  println x ;
  x = x + 1 ; #err here?
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
intJustOverflow.s contents are:
===========================================================
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		push {fp, lr}
6		push {r4, r8, r10, r12}
7		mov fp, sp
8		@ Stack pointer unchanged, no stack allocated variables
9		ldr r8, =2147483646
10		mov r8, r8
11		mov r4, r8
12		@ Stack pointer unchanged, no stack allocated arguments
13		mov r8, r4
14		mov r0, r8
15		@ statement primitives do not return results (but will clobber r0/rax)
16		bl _printi
17		bl _println
18		adds r8, r4, #1
19		blvs _errOverflow
20		push {r8}
21		pop {r8}
22		mov r8, r8
23		mov r4, r8
24		@ Stack pointer unchanged, no stack allocated arguments
25		mov r8, r4
26		mov r0, r8
27		@ statement primitives do not return results (but will clobber r0/rax)
28		bl _printi
29		bl _println
30		adds r8, r4, #1
31		blvs _errOverflow
32		push {r8}
33		pop {r8}
34		mov r8, r8
35		mov r4, r8
36		@ Stack pointer unchanged, no stack allocated arguments
37		mov r8, r4
38		mov r0, r8
39		@ statement primitives do not return results (but will clobber r0/rax)
40		bl _printi
41		bl _println
42		@ Stack pointer unchanged, no stack allocated variables
43		mov r0, #0
44		pop {r4, r8, r10, r12}
45		pop {fp, pc}
46	
47	@ length of .L._prints_str0
48		.word 4
49	.L._prints_str0:
50		.asciz "%.*s"
51	.align 4
52	_prints:
53		push {fp, lr}
54		mov fp, sp
55		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
56		bic sp, sp, #0x7
57		mov r2, r0
58		ldr r1, [r0, #-4]
59		adr r0, .L._prints_str0
60		bl printf
61		mov r0, #0
62		bl fflush
63		mov sp, fp
64		pop {fp, pc}
65	
66	@ length of .L._printi_str0
67		.word 2
68	.L._printi_str0:
69		.asciz "%d"
70	.align 4
71	_printi:
72		push {fp, lr}
73		mov fp, sp
74		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
75		bic sp, sp, #0x7
76		mov r1, r0
77		adr r0, .L._printi_str0
78		bl printf
79		mov r0, #0
80		bl fflush
81		mov sp, fp
82		pop {fp, pc}
83	
84	@ length of .L._println_str0
85		.word 0
86	.L._println_str0:
87		.asciz ""
88	.align 4
89	_println:
90		push {fp, lr}
91		mov fp, sp
92		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
93		bic sp, sp, #0x7
94		adr r0, .L._println_str0
95		bl puts
96		mov r0, #0
97		bl fflush
98		mov sp, fp
99		pop {fp, pc}
100	
101	_exit:
102		push {fp, lr}
103		mov fp, sp
104		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
105		bic sp, sp, #0x7
106		bl exit
107		mov sp, fp
108		pop {fp, pc}
109	
110	@ length of .L._errOverflow_str0
111		.word 52
112	.L._errOverflow_str0:
113		.asciz "fatal error: integer overflow or underflow occurred\n"
114	.align 4
115	_errOverflow:
116		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
117		bic sp, sp, #0x7
118		adr r0, .L._errOverflow_str0
119		bl _prints
120		mov r0, #255
121		bl _exit
===========================================================
-- Finished

