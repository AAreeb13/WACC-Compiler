./valid/while/fibonacciIterative.wacc
calling the reference compiler on ./valid/while/fibonacciIterative.wacc
-- Test: fibonacciIterative.wacc

-- Uploaded file: 
---------------------------------------------------------------
# iterative calculation of the first 20 fibonacci numbers

# Output:
# The first 20 fibonacci numbers are:
# 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, ...
#

# Program:

begin
  int i = 0 ;
  int f0 = 0 ;
  int f1 = 1 ;
  int save = 0;
  println "The first 20 fibonacci numbers are:" ;
  while i < 20 do
    print f0 ;
    print ", " ;
    save = f0 ;
    f0 = f1 ;
    f1 = save + f1 ;
    i = i + 1
  done ;
  println "..."
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
fibonacciIterative.s contents are:
===========================================================
0	.data
1	@ length of .L.str0
2		.word 35
3	.L.str0:
4		.asciz "The first 20 fibonacci numbers are:"
5	@ length of .L.str1
6		.word 2
7	.L.str1:
8		.asciz ", "
9	@ length of .L.str2
10		.word 3
11	.L.str2:
12		.asciz "..."
13	.align 4
14	.text
15	.global main
16	main:
17		push {fp, lr}
18		push {r4, r5, r6, r7, r8, r10, r12}
19		mov fp, sp
20		@ Stack pointer unchanged, no stack allocated variables
21		mov r8, #0
22		mov r4, r8
23		mov r8, #0
24		mov r5, r8
25		mov r8, #1
26		mov r6, r8
27		mov r8, #0
28		mov r7, r8
29		@ Stack pointer unchanged, no stack allocated arguments
30		ldr r8, =.L.str0
31		push {r8}
32		pop {r8}
33		mov r8, r8
34		mov r0, r8
35		@ statement primitives do not return results (but will clobber r0/rax)
36		bl _prints
37		bl _println
38		b .L0
39	.L1:
40		@ Stack pointer unchanged, no stack allocated arguments
41		mov r8, r5
42		mov r0, r8
43		@ statement primitives do not return results (but will clobber r0/rax)
44		bl _printi
45		@ Stack pointer unchanged, no stack allocated arguments
46		ldr r8, =.L.str1
47		push {r8}
48		pop {r8}
49		mov r8, r8
50		mov r0, r8
51		@ statement primitives do not return results (but will clobber r0/rax)
52		bl _prints
53		mov r8, r5
54		mov r7, r8
55		mov r8, r6
56		mov r5, r8
57		adds r8, r7, r6
58		blvs _errOverflow
59		push {r8}
60		pop {r8}
61		mov r8, r8
62		mov r6, r8
63		adds r8, r4, #1
64		blvs _errOverflow
65		push {r8}
66		pop {r8}
67		mov r8, r8
68		mov r4, r8
69	.L0:
70		cmp r4, #20
71		blt .L1
72		@ Stack pointer unchanged, no stack allocated arguments
73		ldr r8, =.L.str2
74		push {r8}
75		pop {r8}
76		mov r8, r8
77		mov r0, r8
78		@ statement primitives do not return results (but will clobber r0/rax)
79		bl _prints
80		bl _println
81		@ Stack pointer unchanged, no stack allocated variables
82		mov r0, #0
83		pop {r4, r5, r6, r7, r8, r10, r12}
84		pop {fp, pc}
85	
86	@ length of .L._prints_str0
87		.word 4
88	.L._prints_str0:
89		.asciz "%.*s"
90	.align 4
91	_prints:
92		push {fp, lr}
93		mov fp, sp
94		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
95		bic sp, sp, #0x7
96		mov r2, r0
97		ldr r1, [r0, #-4]
98		adr r0, .L._prints_str0
99		bl printf
100		mov r0, #0
101		bl fflush
102		mov sp, fp
103		pop {fp, pc}
104	
105	@ length of .L._printi_str0
106		.word 2
107	.L._printi_str0:
108		.asciz "%d"
109	.align 4
110	_printi:
111		push {fp, lr}
112		mov fp, sp
113		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
114		bic sp, sp, #0x7
115		mov r1, r0
116		adr r0, .L._printi_str0
117		bl printf
118		mov r0, #0
119		bl fflush
120		mov sp, fp
121		pop {fp, pc}
122	
123	@ length of .L._println_str0
124		.word 0
125	.L._println_str0:
126		.asciz ""
127	.align 4
128	_println:
129		push {fp, lr}
130		mov fp, sp
131		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
132		bic sp, sp, #0x7
133		adr r0, .L._println_str0
134		bl puts
135		mov r0, #0
136		bl fflush
137		mov sp, fp
138		pop {fp, pc}
139	
140	_exit:
141		push {fp, lr}
142		mov fp, sp
143		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
144		bic sp, sp, #0x7
145		bl exit
146		mov sp, fp
147		pop {fp, pc}
148	
149	@ length of .L._errOverflow_str0
150		.word 52
151	.L._errOverflow_str0:
152		.asciz "fatal error: integer overflow or underflow occurred\n"
153	.align 4
154	_errOverflow:
155		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
156		bic sp, sp, #0x7
157		adr r0, .L._errOverflow_str0
158		bl _prints
159		mov r0, #255
160		bl _exit
===========================================================
-- Finished

