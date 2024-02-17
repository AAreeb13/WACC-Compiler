./valid/runtimeErr/integerOverflow/intnegateOverflow3.wacc
calling the reference compiler on ./valid/runtimeErr/integerOverflow/intnegateOverflow3.wacc
-- Test: intnegateOverflow3.wacc

-- Uploaded file: 
---------------------------------------------------------------
# multiplying a negative integer

# Output:
# -20000
# #runtime_error#

# Exit:
# 255

# Program:

begin
  int x = -20000 ;
  println x ;
  x = x*100000000; #err here? 
  println x 
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
intnegateOverflow3.s contents are:
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
9		ldr r8, =-20000
10		mov r8, r8
11		mov r4, r8
12		@ Stack pointer unchanged, no stack allocated arguments
13		mov r8, r4
14		mov r0, r8
15		@ statement primitives do not return results (but will clobber r0/rax)
16		bl _printi
17		bl _println
18		ldr r9, =100000000
19		smull r8, r9, r4, r9
20		@ Compare the sign of the lowest bits of the multiplication against the highest
21		cmp r9, r8, asr #31
22		blne _errOverflow
23		push {r8}
24		pop {r8}
25		mov r8, r8
26		mov r4, r8
27		@ Stack pointer unchanged, no stack allocated arguments
28		mov r8, r4
29		mov r0, r8
30		@ statement primitives do not return results (but will clobber r0/rax)
31		bl _printi
32		bl _println
33		@ Stack pointer unchanged, no stack allocated variables
34		mov r0, #0
35		pop {r4, r8, r10, r12}
36		pop {fp, pc}
37	
38	@ length of .L._prints_str0
39		.word 4
40	.L._prints_str0:
41		.asciz "%.*s"
42	.align 4
43	_prints:
44		push {fp, lr}
45		mov fp, sp
46		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
47		bic sp, sp, #0x7
48		mov r2, r0
49		ldr r1, [r0, #-4]
50		adr r0, .L._prints_str0
51		bl printf
52		mov r0, #0
53		bl fflush
54		mov sp, fp
55		pop {fp, pc}
56	
57	@ length of .L._printi_str0
58		.word 2
59	.L._printi_str0:
60		.asciz "%d"
61	.align 4
62	_printi:
63		push {fp, lr}
64		mov fp, sp
65		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
66		bic sp, sp, #0x7
67		mov r1, r0
68		adr r0, .L._printi_str0
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
91	
92	_exit:
93		push {fp, lr}
94		mov fp, sp
95		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
96		bic sp, sp, #0x7
97		bl exit
98		mov sp, fp
99		pop {fp, pc}
100	
101	@ length of .L._errOverflow_str0
102		.word 52
103	.L._errOverflow_str0:
104		.asciz "fatal error: integer overflow or underflow occurred\n"
105	.align 4
106	_errOverflow:
107		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
108		bic sp, sp, #0x7
109		adr r0, .L._errOverflow_str0
110		bl _prints
111		mov r0, #255
112		bl _exit
===========================================================
-- Finished

