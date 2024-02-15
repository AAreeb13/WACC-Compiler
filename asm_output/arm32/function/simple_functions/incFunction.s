./valid/function/simple_functions/incFunction.wacc
calling the reference compiler on ./valid/function/simple_functions/incFunction.wacc
-- Test: incFunction.wacc

-- Uploaded file: 
---------------------------------------------------------------
# a simple increment function definition and usage

# Output:
# 1
# 4
#

# Program:

begin
  int inc(int x) is
    return x + 1
  end
  int x = 0 ;
  x = call inc(x) ;
  println x ;
  x = call inc(x) ;
  x = call inc(x) ;
  x = call inc(x) ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
incFunction.s contents are:
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
9		mov r8, #0
10		mov r4, r8
11		@ Stack pointer unchanged, no stack allocated arguments
12		mov r8, r4
13		mov r0, r8
14		bl wacc_inc
15		mov r12, r0
16		@ Stack pointer unchanged, no stack allocated arguments
17		mov r8, r12
18		mov r4, r8
19		@ Stack pointer unchanged, no stack allocated arguments
20		mov r8, r4
21		mov r0, r8
22		@ statement primitives do not return results (but will clobber r0/rax)
23		bl _printi
24		bl _println
25		@ Stack pointer unchanged, no stack allocated arguments
26		mov r8, r4
27		mov r0, r8
28		bl wacc_inc
29		mov r12, r0
30		@ Stack pointer unchanged, no stack allocated arguments
31		mov r8, r12
32		mov r4, r8
33		@ Stack pointer unchanged, no stack allocated arguments
34		mov r8, r4
35		mov r0, r8
36		bl wacc_inc
37		mov r12, r0
38		@ Stack pointer unchanged, no stack allocated arguments
39		mov r8, r12
40		mov r4, r8
41		@ Stack pointer unchanged, no stack allocated arguments
42		mov r8, r4
43		mov r0, r8
44		bl wacc_inc
45		mov r12, r0
46		@ Stack pointer unchanged, no stack allocated arguments
47		mov r8, r12
48		mov r4, r8
49		@ Stack pointer unchanged, no stack allocated arguments
50		mov r8, r4
51		mov r0, r8
52		@ statement primitives do not return results (but will clobber r0/rax)
53		bl _printi
54		bl _println
55		@ Stack pointer unchanged, no stack allocated variables
56		mov r0, #0
57		pop {r4, r8, r10, r12}
58		pop {fp, pc}
59	
60	wacc_inc:
61		push {fp, lr}
62		mov fp, sp
63		adds r8, r0, #1
64		blvs _errOverflow
65		push {r8}
66		pop {r0}
67		mov r0, r0
68		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
69		mov sp, fp
70		pop {fp, pc}
71		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
72		@ the .ltorg here is used to generate the constant pool after every function
73		@ this is important for ensuring string literals and large integers are accessible!
74		.ltorg
75	
76	@ length of .L._prints_str0
77		.word 4
78	.L._prints_str0:
79		.asciz "%.*s"
80	.align 4
81	_prints:
82		push {fp, lr}
83		mov fp, sp
84		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
85		bic sp, sp, #0x7
86		mov r2, r0
87		ldr r1, [r0, #-4]
88		adr r0, .L._prints_str0
89		bl printf
90		mov r0, #0
91		bl fflush
92		mov sp, fp
93		pop {fp, pc}
94	
95	@ length of .L._printi_str0
96		.word 2
97	.L._printi_str0:
98		.asciz "%d"
99	.align 4
100	_printi:
101		push {fp, lr}
102		mov fp, sp
103		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
104		bic sp, sp, #0x7
105		mov r1, r0
106		adr r0, .L._printi_str0
107		bl printf
108		mov r0, #0
109		bl fflush
110		mov sp, fp
111		pop {fp, pc}
112	
113	@ length of .L._println_str0
114		.word 0
115	.L._println_str0:
116		.asciz ""
117	.align 4
118	_println:
119		push {fp, lr}
120		mov fp, sp
121		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
122		bic sp, sp, #0x7
123		adr r0, .L._println_str0
124		bl puts
125		mov r0, #0
126		bl fflush
127		mov sp, fp
128		pop {fp, pc}
129	
130	_exit:
131		push {fp, lr}
132		mov fp, sp
133		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
134		bic sp, sp, #0x7
135		bl exit
136		mov sp, fp
137		pop {fp, pc}
138	
139	@ length of .L._errOverflow_str0
140		.word 52
141	.L._errOverflow_str0:
142		.asciz "fatal error: integer overflow or underflow occurred\n"
143	.align 4
144	_errOverflow:
145		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
146		bic sp, sp, #0x7
147		adr r0, .L._errOverflow_str0
148		bl _prints
149		mov r0, #255
150		bl _exit
===========================================================
-- Finished

