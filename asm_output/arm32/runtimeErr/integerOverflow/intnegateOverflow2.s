./valid/runtimeErr/integerOverflow/intnegateOverflow2.wacc
calling the reference compiler on ./valid/runtimeErr/integerOverflow/intnegateOverflow2.wacc
-- Test: intnegateOverflow2.wacc

-- Uploaded file: 
---------------------------------------------------------------
# multiplying the biggest possible negative integer

# Output:
# -2147483648
# #runtime_error#

# Exit:
# 255

# Program:

begin
  int x = -2147483648 ;
  println x ;
  x = x*10; #err here?
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
intnegateOverflow2.s contents are:
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
17		mov r9, #10
18		smull r8, r9, r4, r9
19		@ Compare the sign of the lowest bits of the multiplication against the highest
20		cmp r9, r8, asr #31
21		blne _errOverflow
22		push {r8}
23		pop {r8}
24		mov r8, r8
25		mov r4, r8
26		@ Stack pointer unchanged, no stack allocated arguments
27		mov r8, r4
28		mov r0, r8
29		@ statement primitives do not return results (but will clobber r0/rax)
30		bl _printi
31		bl _println
32		@ Stack pointer unchanged, no stack allocated variables
33		mov r0, #0
34		pop {r4, r8, r10, r12}
35		pop {fp, pc}
36	
37	@ length of .L._prints_str0
38		.word 4
39	.L._prints_str0:
40		.asciz "%.*s"
41	.align 4
42	_prints:
43		push {fp, lr}
44		mov fp, sp
45		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
46		bic sp, sp, #0x7
47		mov r2, r0
48		ldr r1, [r0, #-4]
49		adr r0, .L._prints_str0
50		bl printf
51		mov r0, #0
52		bl fflush
53		mov sp, fp
54		pop {fp, pc}
55	
56	@ length of .L._printi_str0
57		.word 2
58	.L._printi_str0:
59		.asciz "%d"
60	.align 4
61	_printi:
62		push {fp, lr}
63		mov fp, sp
64		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
65		bic sp, sp, #0x7
66		mov r1, r0
67		adr r0, .L._printi_str0
68		bl printf
69		mov r0, #0
70		bl fflush
71		mov sp, fp
72		pop {fp, pc}
73	
74	@ length of .L._println_str0
75		.word 0
76	.L._println_str0:
77		.asciz ""
78	.align 4
79	_println:
80		push {fp, lr}
81		mov fp, sp
82		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
83		bic sp, sp, #0x7
84		adr r0, .L._println_str0
85		bl puts
86		mov r0, #0
87		bl fflush
88		mov sp, fp
89		pop {fp, pc}
90	
91	_exit:
92		push {fp, lr}
93		mov fp, sp
94		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
95		bic sp, sp, #0x7
96		bl exit
97		mov sp, fp
98		pop {fp, pc}
99	
100	@ length of .L._errOverflow_str0
101		.word 52
102	.L._errOverflow_str0:
103		.asciz "fatal error: integer overflow or underflow occurred\n"
104	.align 4
105	_errOverflow:
106		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
107		bic sp, sp, #0x7
108		adr r0, .L._errOverflow_str0
109		bl _prints
110		mov r0, #255
111		bl _exit
===========================================================
-- Finished

