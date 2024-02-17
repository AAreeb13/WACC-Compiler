./valid/function/simple_functions/manyArgumentsInt.wacc
calling the reference compiler on ./valid/function/simple_functions/manyArgumentsInt.wacc
-- Test: manyArgumentsInt.wacc

-- Uploaded file: 
---------------------------------------------------------------
# tests a function with more arguments than fit into registers

# Output:
# 23
#

# Program:

begin
  # w, x, y and z are stack allocated on aarch32
  # y and z are stack allocated on x86-64
  int f(int s, int t, int u, int v, int w, int x, int y, int z) is
    int i = u + v ;
    int j = w * x ;
    int k = y - z ;
    return i + j * k
  end

  int r = call f(0, 0, 1, 4, 2, 3, 7, 4) ;
  println r
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
manyArgumentsInt.s contents are:
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
9		@ Stack padded to a multiple of the required alignment
10		sub sp, sp, #20
11		mov r8, #0
12		mov r0, r8
13		mov r8, #0
14		mov r1, r8
15		mov r8, #1
16		mov r2, r8
17		mov r8, #4
18		mov r3, r8
19		mov r8, #2
20		str r8, [sp, #12]
21		mov r8, #3
22		str r8, [sp, #8]
23		mov r8, #7
24		str r8, [sp, #4]
25		mov r8, #4
26		str r8, [sp, #0]
27		bl wacc_f
28		mov r12, r0
29		@ Stack padded to a multiple of the required alignment
30		add sp, sp, #20
31		mov r8, r12
32		mov r4, r8
33		@ Stack pointer unchanged, no stack allocated arguments
34		mov r8, r4
35		mov r0, r8
36		@ statement primitives do not return results (but will clobber r0/rax)
37		bl _printi
38		bl _println
39		@ Stack pointer unchanged, no stack allocated variables
40		mov r0, #0
41		pop {r4, r8, r10, r12}
42		pop {fp, pc}
43	
44	wacc_f:
45		push {fp, lr}
46		push {r4, r5, r6}
47		mov fp, sp
48		@ Stack pointer unchanged, no stack allocated variables
49		adds r8, r2, r3
50		blvs _errOverflow
51		push {r8}
52		pop {r8}
53		mov r8, r8
54		mov r4, r8
55		ldr r9, [fp, #28]
56		ldr r8, [fp, #32]
57		smull r8, r9, r8, r9
58		@ Compare the sign of the lowest bits of the multiplication against the highest
59		cmp r9, r8, asr #31
60		blne _errOverflow
61		push {r8}
62		pop {r8}
63		mov r8, r8
64		mov r5, r8
65		ldr r9, [fp, #20]
66		ldr r8, [fp, #24]
67		subs r8, r8, r9
68		blvs _errOverflow
69		push {r8}
70		pop {r8}
71		mov r8, r8
72		mov r6, r8
73		smull r8, r9, r5, r6
74		@ Compare the sign of the lowest bits of the multiplication against the highest
75		cmp r9, r8, asr #31
76		blne _errOverflow
77		push {r8}
78		pop {r9}
79		adds r8, r4, r9
80		blvs _errOverflow
81		push {r8}
82		pop {r0}
83		mov r0, r0
84		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
85		mov sp, fp
86		pop {r4, r5, r6}
87		pop {fp, pc}
88		@ Stack pointer unchanged, no stack allocated variables
89		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
90		@ the .ltorg here is used to generate the constant pool after every function
91		@ this is important for ensuring string literals and large integers are accessible!
92		.ltorg
93	
94	@ length of .L._prints_str0
95		.word 4
96	.L._prints_str0:
97		.asciz "%.*s"
98	.align 4
99	_prints:
100		push {fp, lr}
101		mov fp, sp
102		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
103		bic sp, sp, #0x7
104		mov r2, r0
105		ldr r1, [r0, #-4]
106		adr r0, .L._prints_str0
107		bl printf
108		mov r0, #0
109		bl fflush
110		mov sp, fp
111		pop {fp, pc}
112	
113	@ length of .L._printi_str0
114		.word 2
115	.L._printi_str0:
116		.asciz "%d"
117	.align 4
118	_printi:
119		push {fp, lr}
120		mov fp, sp
121		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
122		bic sp, sp, #0x7
123		mov r1, r0
124		adr r0, .L._printi_str0
125		bl printf
126		mov r0, #0
127		bl fflush
128		mov sp, fp
129		pop {fp, pc}
130	
131	@ length of .L._println_str0
132		.word 0
133	.L._println_str0:
134		.asciz ""
135	.align 4
136	_println:
137		push {fp, lr}
138		mov fp, sp
139		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
140		bic sp, sp, #0x7
141		adr r0, .L._println_str0
142		bl puts
143		mov r0, #0
144		bl fflush
145		mov sp, fp
146		pop {fp, pc}
147	
148	_exit:
149		push {fp, lr}
150		mov fp, sp
151		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
152		bic sp, sp, #0x7
153		bl exit
154		mov sp, fp
155		pop {fp, pc}
156	
157	@ length of .L._errOverflow_str0
158		.word 52
159	.L._errOverflow_str0:
160		.asciz "fatal error: integer overflow or underflow occurred\n"
161	.align 4
162	_errOverflow:
163		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
164		bic sp, sp, #0x7
165		adr r0, .L._errOverflow_str0
166		bl _prints
167		mov r0, #255
168		bl _exit
===========================================================
-- Finished

