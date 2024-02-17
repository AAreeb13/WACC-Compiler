./valid/runtimeErr/integerOverflow/intUnderflow.wacc
calling the reference compiler on ./valid/runtimeErr/integerOverflow/intUnderflow.wacc
-- Test: intUnderflow.wacc

-- Uploaded file: 
---------------------------------------------------------------
# integer underflow

# Output:
# -2147483647
# -2147483648
# #runtime_error#

# Exit:
# 255

# Program:

begin
  int x = -2147483647 ;
  println x ;
  x = x - 1 ;
  println x ;
  x = x - 1 ; #err here?
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
intUnderflow.s contents are:
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
9		mov r8, #-2147483647
10		mov r4, r8
11		@ Stack pointer unchanged, no stack allocated arguments
12		mov r8, r4
13		mov r0, r8
14		@ statement primitives do not return results (but will clobber r0/rax)
15		bl _printi
16		bl _println
17		subs r8, r4, #1
18		blvs _errOverflow
19		push {r8}
20		pop {r8}
21		mov r8, r8
22		mov r4, r8
23		@ Stack pointer unchanged, no stack allocated arguments
24		mov r8, r4
25		mov r0, r8
26		@ statement primitives do not return results (but will clobber r0/rax)
27		bl _printi
28		bl _println
29		subs r8, r4, #1
30		blvs _errOverflow
31		push {r8}
32		pop {r8}
33		mov r8, r8
34		mov r4, r8
35		@ Stack pointer unchanged, no stack allocated arguments
36		mov r8, r4
37		mov r0, r8
38		@ statement primitives do not return results (but will clobber r0/rax)
39		bl _printi
40		bl _println
41		@ Stack pointer unchanged, no stack allocated variables
42		mov r0, #0
43		pop {r4, r8, r10, r12}
44		pop {fp, pc}
45	
46	@ length of .L._prints_str0
47		.word 4
48	.L._prints_str0:
49		.asciz "%.*s"
50	.align 4
51	_prints:
52		push {fp, lr}
53		mov fp, sp
54		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
55		bic sp, sp, #0x7
56		mov r2, r0
57		ldr r1, [r0, #-4]
58		adr r0, .L._prints_str0
59		bl printf
60		mov r0, #0
61		bl fflush
62		mov sp, fp
63		pop {fp, pc}
64	
65	@ length of .L._printi_str0
66		.word 2
67	.L._printi_str0:
68		.asciz "%d"
69	.align 4
70	_printi:
71		push {fp, lr}
72		mov fp, sp
73		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
74		bic sp, sp, #0x7
75		mov r1, r0
76		adr r0, .L._printi_str0
77		bl printf
78		mov r0, #0
79		bl fflush
80		mov sp, fp
81		pop {fp, pc}
82	
83	@ length of .L._println_str0
84		.word 0
85	.L._println_str0:
86		.asciz ""
87	.align 4
88	_println:
89		push {fp, lr}
90		mov fp, sp
91		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
92		bic sp, sp, #0x7
93		adr r0, .L._println_str0
94		bl puts
95		mov r0, #0
96		bl fflush
97		mov sp, fp
98		pop {fp, pc}
99	
100	_exit:
101		push {fp, lr}
102		mov fp, sp
103		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
104		bic sp, sp, #0x7
105		bl exit
106		mov sp, fp
107		pop {fp, pc}
108	
109	@ length of .L._errOverflow_str0
110		.word 52
111	.L._errOverflow_str0:
112		.asciz "fatal error: integer overflow or underflow occurred\n"
113	.align 4
114	_errOverflow:
115		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
116		bic sp, sp, #0x7
117		adr r0, .L._errOverflow_str0
118		bl _prints
119		mov r0, #255
120		bl _exit
===========================================================
-- Finished

