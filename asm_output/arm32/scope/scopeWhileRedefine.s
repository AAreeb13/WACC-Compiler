./valid/scope/scopeWhileRedefine.wacc
calling the reference compiler on ./valid/scope/scopeWhileRedefine.wacc
-- Test: scopeWhileRedefine.wacc

-- Uploaded file: 
---------------------------------------------------------------
# variable scoping test that redefines a variable within a while-loop

# Output:
# counting... 5
# counting... 4
# counting... 3
# counting... 2
# counting... 1
# 0 Boom!
#

# Program:

begin
  int x = 5 ;
  string y = " Boom!" ;
  while x > 0 do
    string y = "counting... " ;
    print y;
    println x;
    x = x - 1
  done ;
  print x;
  println y
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
scopeWhileRedefine.s contents are:
===========================================================
0	.data
1	@ length of .L.str0
2		.word 6
3	.L.str0:
4		.asciz " Boom!"
5	@ length of .L.str1
6		.word 12
7	.L.str1:
8		.asciz "counting... "
9	.align 4
10	.text
11	.global main
12	main:
13		push {fp, lr}
14		push {r4, r5, r6, r8, r10, r12}
15		mov fp, sp
16		@ Stack pointer unchanged, no stack allocated variables
17		mov r8, #5
18		mov r4, r8
19		ldr r8, =.L.str0
20		push {r8}
21		pop {r8}
22		mov r8, r8
23		mov r5, r8
24		b .L0
25	.L1:
26		@ Stack pointer unchanged, no stack allocated variables
27		ldr r8, =.L.str1
28		push {r8}
29		pop {r8}
30		mov r8, r8
31		mov r6, r8
32		@ Stack pointer unchanged, no stack allocated arguments
33		mov r8, r6
34		mov r0, r8
35		@ statement primitives do not return results (but will clobber r0/rax)
36		bl _prints
37		@ Stack pointer unchanged, no stack allocated arguments
38		mov r8, r4
39		mov r0, r8
40		@ statement primitives do not return results (but will clobber r0/rax)
41		bl _printi
42		bl _println
43		subs r8, r4, #1
44		blvs _errOverflow
45		push {r8}
46		pop {r8}
47		mov r8, r8
48		mov r4, r8
49		@ Stack pointer unchanged, no stack allocated variables
50	.L0:
51		cmp r4, #0
52		bgt .L1
53		@ Stack pointer unchanged, no stack allocated arguments
54		mov r8, r4
55		mov r0, r8
56		@ statement primitives do not return results (but will clobber r0/rax)
57		bl _printi
58		@ Stack pointer unchanged, no stack allocated arguments
59		mov r8, r5
60		mov r0, r8
61		@ statement primitives do not return results (but will clobber r0/rax)
62		bl _prints
63		bl _println
64		@ Stack pointer unchanged, no stack allocated variables
65		mov r0, #0
66		pop {r4, r5, r6, r8, r10, r12}
67		pop {fp, pc}
68	
69	@ length of .L._prints_str0
70		.word 4
71	.L._prints_str0:
72		.asciz "%.*s"
73	.align 4
74	_prints:
75		push {fp, lr}
76		mov fp, sp
77		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
78		bic sp, sp, #0x7
79		mov r2, r0
80		ldr r1, [r0, #-4]
81		adr r0, .L._prints_str0
82		bl printf
83		mov r0, #0
84		bl fflush
85		mov sp, fp
86		pop {fp, pc}
87	
88	@ length of .L._printi_str0
89		.word 2
90	.L._printi_str0:
91		.asciz "%d"
92	.align 4
93	_printi:
94		push {fp, lr}
95		mov fp, sp
96		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
97		bic sp, sp, #0x7
98		mov r1, r0
99		adr r0, .L._printi_str0
100		bl printf
101		mov r0, #0
102		bl fflush
103		mov sp, fp
104		pop {fp, pc}
105	
106	@ length of .L._println_str0
107		.word 0
108	.L._println_str0:
109		.asciz ""
110	.align 4
111	_println:
112		push {fp, lr}
113		mov fp, sp
114		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
115		bic sp, sp, #0x7
116		adr r0, .L._println_str0
117		bl puts
118		mov r0, #0
119		bl fflush
120		mov sp, fp
121		pop {fp, pc}
122	
123	_exit:
124		push {fp, lr}
125		mov fp, sp
126		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
127		bic sp, sp, #0x7
128		bl exit
129		mov sp, fp
130		pop {fp, pc}
131	
132	@ length of .L._errOverflow_str0
133		.word 52
134	.L._errOverflow_str0:
135		.asciz "fatal error: integer overflow or underflow occurred\n"
136	.align 4
137	_errOverflow:
138		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
139		bic sp, sp, #0x7
140		adr r0, .L._errOverflow_str0
141		bl _prints
142		mov r0, #255
143		bl _exit
===========================================================
-- Finished

