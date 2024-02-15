./valid/while/whileCount.wacc
calling the reference compiler on ./valid/while/whileCount.wacc
-- Test: whileCount.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple counting while loop

# Output:
# Can you count to 10?
# 1
# 2
# 3
# 4
# 5
# 6
# 7
# 8
# 9
# 10
#

# Program:

begin
  int i = 1 ;
  println "Can you count to 10?" ;
  while i <= 10 do
    println i ;
    i = i + 1
  done
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
whileCount.s contents are:
===========================================================
0	.data
1	@ length of .L.str0
2		.word 20
3	.L.str0:
4		.asciz "Can you count to 10?"
5	.align 4
6	.text
7	.global main
8	main:
9		push {fp, lr}
10		push {r4, r8, r10, r12}
11		mov fp, sp
12		@ Stack pointer unchanged, no stack allocated variables
13		mov r8, #1
14		mov r4, r8
15		@ Stack pointer unchanged, no stack allocated arguments
16		ldr r8, =.L.str0
17		push {r8}
18		pop {r8}
19		mov r8, r8
20		mov r0, r8
21		@ statement primitives do not return results (but will clobber r0/rax)
22		bl _prints
23		bl _println
24		b .L0
25	.L1:
26		@ Stack pointer unchanged, no stack allocated arguments
27		mov r8, r4
28		mov r0, r8
29		@ statement primitives do not return results (but will clobber r0/rax)
30		bl _printi
31		bl _println
32		adds r8, r4, #1
33		blvs _errOverflow
34		push {r8}
35		pop {r8}
36		mov r8, r8
37		mov r4, r8
38	.L0:
39		cmp r4, #10
40		ble .L1
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

