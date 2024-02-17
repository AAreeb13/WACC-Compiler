./valid/runtimeErr/integerOverflow/intnegateOverflow.wacc
calling the reference compiler on ./valid/runtimeErr/integerOverflow/intnegateOverflow.wacc
-- Test: intnegateOverflow.wacc

-- Uploaded file: 
---------------------------------------------------------------
# negating biggest possible negative integer

# Output:
# -2147483648
# #runtime_error#

# Exit:
# 255

# Program:

begin
  int x = -2147483648 ;
  println x ;
  x = -x; #err here?
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
intnegateOverflow.s contents are:
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
9		mov r8, #-2147483648
10		mov r4, r8
11		@ Stack pointer unchanged, no stack allocated arguments
12		mov r8, r4
13		mov r0, r8
14		@ statement primitives do not return results (but will clobber r0/rax)
15		bl _printi
16		bl _println
17		rsbs r8, r4, #0
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
29		@ Stack pointer unchanged, no stack allocated variables
30		mov r0, #0
31		pop {r4, r8, r10, r12}
32		pop {fp, pc}
33	
34	@ length of .L._prints_str0
35		.word 4
36	.L._prints_str0:
37		.asciz "%.*s"
38	.align 4
39	_prints:
40		push {fp, lr}
41		mov fp, sp
42		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
43		bic sp, sp, #0x7
44		mov r2, r0
45		ldr r1, [r0, #-4]
46		adr r0, .L._prints_str0
47		bl printf
48		mov r0, #0
49		bl fflush
50		mov sp, fp
51		pop {fp, pc}
52	
53	@ length of .L._printi_str0
54		.word 2
55	.L._printi_str0:
56		.asciz "%d"
57	.align 4
58	_printi:
59		push {fp, lr}
60		mov fp, sp
61		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
62		bic sp, sp, #0x7
63		mov r1, r0
64		adr r0, .L._printi_str0
65		bl printf
66		mov r0, #0
67		bl fflush
68		mov sp, fp
69		pop {fp, pc}
70	
71	@ length of .L._println_str0
72		.word 0
73	.L._println_str0:
74		.asciz ""
75	.align 4
76	_println:
77		push {fp, lr}
78		mov fp, sp
79		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
80		bic sp, sp, #0x7
81		adr r0, .L._println_str0
82		bl puts
83		mov r0, #0
84		bl fflush
85		mov sp, fp
86		pop {fp, pc}
87	
88	_exit:
89		push {fp, lr}
90		mov fp, sp
91		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
92		bic sp, sp, #0x7
93		bl exit
94		mov sp, fp
95		pop {fp, pc}
96	
97	@ length of .L._errOverflow_str0
98		.word 52
99	.L._errOverflow_str0:
100		.asciz "fatal error: integer overflow or underflow occurred\n"
101	.align 4
102	_errOverflow:
103		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
104		bic sp, sp, #0x7
105		adr r0, .L._errOverflow_str0
106		bl _prints
107		mov r0, #255
108		bl _exit
===========================================================
-- Finished

