./valid/runtimeErr/integerOverflow/intnegateOverflow4.wacc
calling the reference compiler on ./valid/runtimeErr/integerOverflow/intnegateOverflow4.wacc
-- Test: intnegateOverflow4.wacc

-- Uploaded file: 
---------------------------------------------------------------
# negative overflow

# Output:
# -2000000000
# #runtime_error#

# Exit:
# 255

# Program:

begin
  int x = -2000000000 ;
  println x ;
  x = x - 2000000000; #err here? 
  println x 
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
intnegateOverflow4.s contents are:
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
9		ldr r8, =-2000000000
10		mov r8, r8
11		mov r4, r8
12		@ Stack pointer unchanged, no stack allocated arguments
13		mov r8, r4
14		mov r0, r8
15		@ statement primitives do not return results (but will clobber r0/rax)
16		bl _printi
17		bl _println
18		ldr r9, =2000000000
19		subs r8, r4, r9
20		blvs _errOverflow
21		push {r8}
22		pop {r8}
23		mov r8, r8
24		mov r4, r8
25		@ Stack pointer unchanged, no stack allocated arguments
26		mov r8, r4
27		mov r0, r8
28		@ statement primitives do not return results (but will clobber r0/rax)
29		bl _printi
30		bl _println
31		@ Stack pointer unchanged, no stack allocated variables
32		mov r0, #0
33		pop {r4, r8, r10, r12}
34		pop {fp, pc}
35	
36	@ length of .L._prints_str0
37		.word 4
38	.L._prints_str0:
39		.asciz "%.*s"
40	.align 4
41	_prints:
42		push {fp, lr}
43		mov fp, sp
44		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
45		bic sp, sp, #0x7
46		mov r2, r0
47		ldr r1, [r0, #-4]
48		adr r0, .L._prints_str0
49		bl printf
50		mov r0, #0
51		bl fflush
52		mov sp, fp
53		pop {fp, pc}
54	
55	@ length of .L._printi_str0
56		.word 2
57	.L._printi_str0:
58		.asciz "%d"
59	.align 4
60	_printi:
61		push {fp, lr}
62		mov fp, sp
63		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
64		bic sp, sp, #0x7
65		mov r1, r0
66		adr r0, .L._printi_str0
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
89	
90	_exit:
91		push {fp, lr}
92		mov fp, sp
93		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
94		bic sp, sp, #0x7
95		bl exit
96		mov sp, fp
97		pop {fp, pc}
98	
99	@ length of .L._errOverflow_str0
100		.word 52
101	.L._errOverflow_str0:
102		.asciz "fatal error: integer overflow or underflow occurred\n"
103	.align 4
104	_errOverflow:
105		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
106		bic sp, sp, #0x7
107		adr r0, .L._errOverflow_str0
108		bl _prints
109		mov r0, #255
110		bl _exit
===========================================================
-- Finished

