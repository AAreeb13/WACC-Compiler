./valid/function/simple_functions/functionReturnPair.wacc
calling the reference compiler on ./valid/function/simple_functions/functionReturnPair.wacc
-- Test: functionReturnPair.wacc

-- Uploaded file: 
---------------------------------------------------------------
# creates a pair which is returned from a function

# Output:
# 10
#

# Program:

begin

  pair(int, int) getPair() is
    pair(int, int) p = newpair(10,15);
    return p
  end

  pair(int, int) p = call getPair();
  int x = fst p;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
functionReturnPair.s contents are:
===========================================================
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		push {fp, lr}
6		push {r4, r5, r8, r10, r12}
7		mov fp, sp
8		@ Stack pointer unchanged, no stack allocated variables
9		@ Stack pointer unchanged, no stack allocated arguments
10		bl wacc_getPair
11		mov r12, r0
12		@ Stack pointer unchanged, no stack allocated arguments
13		mov r8, r12
14		mov r4, r8
15		cmp r4, #0
16		bleq _errNull
17		ldr r8, [r4, #0]
18		mov r8, r8
19		mov r5, r8
20		@ Stack pointer unchanged, no stack allocated arguments
21		mov r8, r5
22		mov r0, r8
23		@ statement primitives do not return results (but will clobber r0/rax)
24		bl _printi
25		bl _println
26		@ Stack pointer unchanged, no stack allocated variables
27		mov r0, #0
28		pop {r4, r5, r8, r10, r12}
29		pop {fp, pc}
30	
31	wacc_getPair:
32		push {fp, lr}
33		push {r4}
34		mov fp, sp
35		@ Stack pointer unchanged, no stack allocated variables
36		mov r0, #8
37		bl _malloc
38		mov r12, r0
39		mov r8, #10
40		str r8, [r12, #0]
41		mov r8, #15
42		str r8, [r12, #4]
43		mov r8, r12
44		mov r4, r8
45		mov r0, r4
46		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
47		mov sp, fp
48		pop {r4}
49		pop {fp, pc}
50		@ Stack pointer unchanged, no stack allocated variables
51		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
52		@ the .ltorg here is used to generate the constant pool after every function
53		@ this is important for ensuring string literals and large integers are accessible!
54		.ltorg
55	
56	@ length of .L._prints_str0
57		.word 4
58	.L._prints_str0:
59		.asciz "%.*s"
60	.align 4
61	_prints:
62		push {fp, lr}
63		mov fp, sp
64		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
65		bic sp, sp, #0x7
66		mov r2, r0
67		ldr r1, [r0, #-4]
68		adr r0, .L._prints_str0
69		bl printf
70		mov r0, #0
71		bl fflush
72		mov sp, fp
73		pop {fp, pc}
74	
75	_malloc:
76		push {fp, lr}
77		mov fp, sp
78		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
79		bic sp, sp, #0x7
80		bl malloc
81		cmp r0, #0
82		bleq _errOutOfMemory
83		mov sp, fp
84		pop {fp, pc}
85	
86	@ length of .L._printi_str0
87		.word 2
88	.L._printi_str0:
89		.asciz "%d"
90	.align 4
91	_printi:
92		push {fp, lr}
93		mov fp, sp
94		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
95		bic sp, sp, #0x7
96		mov r1, r0
97		adr r0, .L._printi_str0
98		bl printf
99		mov r0, #0
100		bl fflush
101		mov sp, fp
102		pop {fp, pc}
103	
104	@ length of .L._println_str0
105		.word 0
106	.L._println_str0:
107		.asciz ""
108	.align 4
109	_println:
110		push {fp, lr}
111		mov fp, sp
112		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
113		bic sp, sp, #0x7
114		adr r0, .L._println_str0
115		bl puts
116		mov r0, #0
117		bl fflush
118		mov sp, fp
119		pop {fp, pc}
120	
121	_exit:
122		push {fp, lr}
123		mov fp, sp
124		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
125		bic sp, sp, #0x7
126		bl exit
127		mov sp, fp
128		pop {fp, pc}
129	
130	@ length of .L._errOutOfMemory_str0
131		.word 27
132	.L._errOutOfMemory_str0:
133		.asciz "fatal error: out of memory\n"
134	.align 4
135	_errOutOfMemory:
136		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
137		bic sp, sp, #0x7
138		adr r0, .L._errOutOfMemory_str0
139		bl _prints
140		mov r0, #255
141		bl _exit
142	
143	@ length of .L._errNull_str0
144		.word 45
145	.L._errNull_str0:
146		.asciz "fatal error: null pair dereferenced or freed\n"
147	.align 4
148	_errNull:
149		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
150		bic sp, sp, #0x7
151		adr r0, .L._errNull_str0
152		bl _prints
153		mov r0, #255
154		bl _exit
===========================================================
-- Finished

