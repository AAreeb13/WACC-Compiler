./valid/function/simple_functions/usesArgumentWhilstMakingArgument.wacc
calling the reference compiler on ./valid/function/simple_functions/usesArgumentWhilstMakingArgument.wacc
-- Test: usesArgumentWhilstMakingArgument.wacc

-- Uploaded file: 
---------------------------------------------------------------
# tests a function with more arguments than fit into registers

# Output:
# 12
# -4
# 32
#

# Program:

begin
  int f(int u, int v) is
    int x = call g(u + v, u - v, u * v) ;
    return x
  end

  int g(int x, int y, int z) is
    println(x) ;
    println(y) ;
    println(z) ;
    return 0
  end

  int r = call f(4, 8)
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
usesArgumentWhilstMakingArgument.s contents are:
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
9		@ Stack pointer unchanged, no stack allocated arguments
10		mov r8, #4
11		mov r0, r8
12		mov r8, #8
13		mov r1, r8
14		bl wacc_f
15		mov r12, r0
16		@ Stack pointer unchanged, no stack allocated arguments
17		mov r8, r12
18		mov r4, r8
19		@ Stack pointer unchanged, no stack allocated variables
20		mov r0, #0
21		pop {r4, r8, r10, r12}
22		pop {fp, pc}
23	
24	wacc_f:
25		push {fp, lr}
26		push {r4}
27		mov fp, sp
28		@ Stack pointer unchanged, no stack allocated variables
29		push {r0, r1}
30		@ Set up R12 as a temporary second base pointer for the caller saved things
31		mov r12, sp
32		@ Stack pointer unchanged, no stack allocated arguments
33		adds r8, r0, r1
34		blvs _errOverflow
35		push {r8}
36		pop {r8}
37		mov r8, r8
38		mov r0, r8
39		ldr r8, [r12, #0]
40		subs r8, r8, r1
41		blvs _errOverflow
42		push {r8}
43		pop {r8}
44		mov r8, r8
45		mov r1, r8
46		ldr r9, [r12, #4]
47		ldr r8, [r12, #0]
48		smull r8, r9, r8, r9
49		@ Compare the sign of the lowest bits of the multiplication against the highest
50		cmp r9, r8, asr #31
51		blne _errOverflow
52		push {r8}
53		pop {r8}
54		mov r8, r8
55		mov r2, r8
56		bl wacc_g
57		mov r12, r0
58		@ Stack pointer unchanged, no stack allocated arguments
59		pop {r0, r1}
60		mov r8, r12
61		mov r4, r8
62		mov r0, r4
63		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
64		mov sp, fp
65		pop {r4}
66		pop {fp, pc}
67		@ Stack pointer unchanged, no stack allocated variables
68		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
69		@ the .ltorg here is used to generate the constant pool after every function
70		@ this is important for ensuring string literals and large integers are accessible!
71		.ltorg
72	
73	wacc_g:
74		push {fp, lr}
75		mov fp, sp
76		push {r0, r1, r2}
77		@ Set up R12 as a temporary second base pointer for the caller saved things
78		mov r12, sp
79		@ Stack pointer unchanged, no stack allocated arguments
80		mov r8, r0
81		mov r0, r8
82		@ statement primitives do not return results (but will clobber r0/rax)
83		bl _printi
84		bl _println
85		pop {r0, r1, r2}
86		push {r0, r1, r2}
87		@ Set up R12 as a temporary second base pointer for the caller saved things
88		mov r12, sp
89		@ Stack pointer unchanged, no stack allocated arguments
90		mov r8, r1
91		mov r0, r8
92		@ statement primitives do not return results (but will clobber r0/rax)
93		bl _printi
94		bl _println
95		pop {r0, r1, r2}
96		push {r0, r1, r2}
97		@ Set up R12 as a temporary second base pointer for the caller saved things
98		mov r12, sp
99		@ Stack pointer unchanged, no stack allocated arguments
100		mov r8, r2
101		mov r0, r8
102		@ statement primitives do not return results (but will clobber r0/rax)
103		bl _printi
104		bl _println
105		pop {r0, r1, r2}
106		mov r0, #0
107		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
108		mov sp, fp
109		pop {fp, pc}
110		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
111		@ the .ltorg here is used to generate the constant pool after every function
112		@ this is important for ensuring string literals and large integers are accessible!
113		.ltorg
114	
115	@ length of .L._prints_str0
116		.word 4
117	.L._prints_str0:
118		.asciz "%.*s"
119	.align 4
120	_prints:
121		push {fp, lr}
122		mov fp, sp
123		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
124		bic sp, sp, #0x7
125		mov r2, r0
126		ldr r1, [r0, #-4]
127		adr r0, .L._prints_str0
128		bl printf
129		mov r0, #0
130		bl fflush
131		mov sp, fp
132		pop {fp, pc}
133	
134	@ length of .L._printi_str0
135		.word 2
136	.L._printi_str0:
137		.asciz "%d"
138	.align 4
139	_printi:
140		push {fp, lr}
141		mov fp, sp
142		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
143		bic sp, sp, #0x7
144		mov r1, r0
145		adr r0, .L._printi_str0
146		bl printf
147		mov r0, #0
148		bl fflush
149		mov sp, fp
150		pop {fp, pc}
151	
152	@ length of .L._println_str0
153		.word 0
154	.L._println_str0:
155		.asciz ""
156	.align 4
157	_println:
158		push {fp, lr}
159		mov fp, sp
160		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
161		bic sp, sp, #0x7
162		adr r0, .L._println_str0
163		bl puts
164		mov r0, #0
165		bl fflush
166		mov sp, fp
167		pop {fp, pc}
168	
169	_exit:
170		push {fp, lr}
171		mov fp, sp
172		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
173		bic sp, sp, #0x7
174		bl exit
175		mov sp, fp
176		pop {fp, pc}
177	
178	@ length of .L._errOverflow_str0
179		.word 52
180	.L._errOverflow_str0:
181		.asciz "fatal error: integer overflow or underflow occurred\n"
182	.align 4
183	_errOverflow:
184		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
185		bic sp, sp, #0x7
186		adr r0, .L._errOverflow_str0
187		bl _prints
188		mov r0, #255
189		bl _exit
===========================================================
-- Finished

