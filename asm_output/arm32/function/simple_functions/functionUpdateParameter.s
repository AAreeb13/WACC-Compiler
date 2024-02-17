./valid/function/simple_functions/functionUpdateParameter.wacc
calling the reference compiler on ./valid/function/simple_functions/functionUpdateParameter.wacc
-- Test: functionUpdateParameter.wacc

-- Uploaded file: 
---------------------------------------------------------------
# test that the passed parameter can be updated and used
# and that y remains the same

# Output:
# y is 1
# x is 1
# x is now 5
# y is still 1
#

# Program:

begin

  int f(int x) is
    print "x is ";
    println x;
    x = 5;
    print "x is now ";
    println x;
    return x
  end

  int y = 1;
  print "y is ";
  println y;
  int x = call f(y);
  print "y is still ";
  println y
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
functionUpdateParameter.s contents are:
===========================================================
0	.data
1	@ length of .L.str0
2		.word 5
3	.L.str0:
4		.asciz "x is "
5	@ length of .L.str1
6		.word 9
7	.L.str1:
8		.asciz "x is now "
9	@ length of .L.str2
10		.word 5
11	.L.str2:
12		.asciz "y is "
13	@ length of .L.str3
14		.word 11
15	.L.str3:
16		.asciz "y is still "
17	.align 4
18	.text
19	.global main
20	main:
21		push {fp, lr}
22		push {r4, r5, r8, r10, r12}
23		mov fp, sp
24		@ Stack pointer unchanged, no stack allocated variables
25		mov r8, #1
26		mov r4, r8
27		@ Stack pointer unchanged, no stack allocated arguments
28		ldr r8, =.L.str2
29		push {r8}
30		pop {r8}
31		mov r8, r8
32		mov r0, r8
33		@ statement primitives do not return results (but will clobber r0/rax)
34		bl _prints
35		@ Stack pointer unchanged, no stack allocated arguments
36		mov r8, r4
37		mov r0, r8
38		@ statement primitives do not return results (but will clobber r0/rax)
39		bl _printi
40		bl _println
41		@ Stack pointer unchanged, no stack allocated arguments
42		mov r8, r4
43		mov r0, r8
44		bl wacc_f
45		mov r12, r0
46		@ Stack pointer unchanged, no stack allocated arguments
47		mov r8, r12
48		mov r5, r8
49		@ Stack pointer unchanged, no stack allocated arguments
50		ldr r8, =.L.str3
51		push {r8}
52		pop {r8}
53		mov r8, r8
54		mov r0, r8
55		@ statement primitives do not return results (but will clobber r0/rax)
56		bl _prints
57		@ Stack pointer unchanged, no stack allocated arguments
58		mov r8, r4
59		mov r0, r8
60		@ statement primitives do not return results (but will clobber r0/rax)
61		bl _printi
62		bl _println
63		@ Stack pointer unchanged, no stack allocated variables
64		mov r0, #0
65		pop {r4, r5, r8, r10, r12}
66		pop {fp, pc}
67	
68	wacc_f:
69		push {fp, lr}
70		mov fp, sp
71		push {r0}
72		@ Set up R12 as a temporary second base pointer for the caller saved things
73		mov r12, sp
74		@ Stack pointer unchanged, no stack allocated arguments
75		ldr r8, =.L.str0
76		push {r8}
77		pop {r8}
78		mov r8, r8
79		mov r0, r8
80		@ statement primitives do not return results (but will clobber r0/rax)
81		bl _prints
82		pop {r0}
83		push {r0}
84		@ Set up R12 as a temporary second base pointer for the caller saved things
85		mov r12, sp
86		@ Stack pointer unchanged, no stack allocated arguments
87		mov r8, r0
88		mov r0, r8
89		@ statement primitives do not return results (but will clobber r0/rax)
90		bl _printi
91		bl _println
92		pop {r0}
93		mov r8, #5
94		mov r0, r8
95		push {r0}
96		@ Set up R12 as a temporary second base pointer for the caller saved things
97		mov r12, sp
98		@ Stack pointer unchanged, no stack allocated arguments
99		ldr r8, =.L.str1
100		push {r8}
101		pop {r8}
102		mov r8, r8
103		mov r0, r8
104		@ statement primitives do not return results (but will clobber r0/rax)
105		bl _prints
106		pop {r0}
107		push {r0}
108		@ Set up R12 as a temporary second base pointer for the caller saved things
109		mov r12, sp
110		@ Stack pointer unchanged, no stack allocated arguments
111		mov r8, r0
112		mov r0, r8
113		@ statement primitives do not return results (but will clobber r0/rax)
114		bl _printi
115		bl _println
116		pop {r0}
117		mov r0, r0
118		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
119		mov sp, fp
120		pop {fp, pc}
121		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
122		@ the .ltorg here is used to generate the constant pool after every function
123		@ this is important for ensuring string literals and large integers are accessible!
124		.ltorg
125	
126	@ length of .L._prints_str0
127		.word 4
128	.L._prints_str0:
129		.asciz "%.*s"
130	.align 4
131	_prints:
132		push {fp, lr}
133		mov fp, sp
134		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
135		bic sp, sp, #0x7
136		mov r2, r0
137		ldr r1, [r0, #-4]
138		adr r0, .L._prints_str0
139		bl printf
140		mov r0, #0
141		bl fflush
142		mov sp, fp
143		pop {fp, pc}
144	
145	@ length of .L._printi_str0
146		.word 2
147	.L._printi_str0:
148		.asciz "%d"
149	.align 4
150	_printi:
151		push {fp, lr}
152		mov fp, sp
153		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
154		bic sp, sp, #0x7
155		mov r1, r0
156		adr r0, .L._printi_str0
157		bl printf
158		mov r0, #0
159		bl fflush
160		mov sp, fp
161		pop {fp, pc}
162	
163	@ length of .L._println_str0
164		.word 0
165	.L._println_str0:
166		.asciz ""
167	.align 4
168	_println:
169		push {fp, lr}
170		mov fp, sp
171		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
172		bic sp, sp, #0x7
173		adr r0, .L._println_str0
174		bl puts
175		mov r0, #0
176		bl fflush
177		mov sp, fp
178		pop {fp, pc}
===========================================================
-- Finished

