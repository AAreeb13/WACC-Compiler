./valid/while/min.wacc
calling the reference compiler on ./valid/while/min.wacc
-- Test: min.wacc

-- Uploaded file: 
---------------------------------------------------------------
# find the min of two numbers

# Output:
# min value = 10
#

# Program:

begin
  int i = 0 ;
  int x = 10 ;
  int y = 17 ;
  while y > 0 && x > 0 do
    x = x - 1 ;
    y = y - 1 ;
    i = i + 1
  done ;
  print "min value = " ;
  println i
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
min.s contents are:
===========================================================
0	.data
1	@ length of .L.str0
2		.word 12
3	.L.str0:
4		.asciz "min value = "
5	.align 4
6	.text
7	.global main
8	main:
9		push {fp, lr}
10		push {r4, r5, r6, r8, r10, r12}
11		mov fp, sp
12		@ Stack pointer unchanged, no stack allocated variables
13		mov r8, #0
14		mov r4, r8
15		mov r8, #10
16		mov r5, r8
17		mov r8, #17
18		mov r6, r8
19		b .L0
20	.L1:
21		subs r8, r5, #1
22		blvs _errOverflow
23		push {r8}
24		pop {r8}
25		mov r8, r8
26		mov r5, r8
27		subs r8, r6, #1
28		blvs _errOverflow
29		push {r8}
30		pop {r8}
31		mov r8, r8
32		mov r6, r8
33		adds r8, r4, #1
34		blvs _errOverflow
35		push {r8}
36		pop {r8}
37		mov r8, r8
38		mov r4, r8
39	.L0:
40		cmp r6, #0
41		movgt r8, #1
42		movle r8, #0
43		push {r8}
44		pop {r8}
45		cmp r8, #1
46		bne .L2
47		cmp r5, #0
48		movgt r8, #1
49		movle r8, #0
50		push {r8}
51		pop {r8}
52		cmp r8, #1
53	.L2:
54		beq .L1
55		@ Stack pointer unchanged, no stack allocated arguments
56		ldr r8, =.L.str0
57		push {r8}
58		pop {r8}
59		mov r8, r8
60		mov r0, r8
61		@ statement primitives do not return results (but will clobber r0/rax)
62		bl _prints
63		@ Stack pointer unchanged, no stack allocated arguments
64		mov r8, r4
65		mov r0, r8
66		@ statement primitives do not return results (but will clobber r0/rax)
67		bl _printi
68		bl _println
69		@ Stack pointer unchanged, no stack allocated variables
70		mov r0, #0
71		pop {r4, r5, r6, r8, r10, r12}
72		pop {fp, pc}
73	
74	@ length of .L._prints_str0
75		.word 4
76	.L._prints_str0:
77		.asciz "%.*s"
78	.align 4
79	_prints:
80		push {fp, lr}
81		mov fp, sp
82		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
83		bic sp, sp, #0x7
84		mov r2, r0
85		ldr r1, [r0, #-4]
86		adr r0, .L._prints_str0
87		bl printf
88		mov r0, #0
89		bl fflush
90		mov sp, fp
91		pop {fp, pc}
92	
93	@ length of .L._printi_str0
94		.word 2
95	.L._printi_str0:
96		.asciz "%d"
97	.align 4
98	_printi:
99		push {fp, lr}
100		mov fp, sp
101		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
102		bic sp, sp, #0x7
103		mov r1, r0
104		adr r0, .L._printi_str0
105		bl printf
106		mov r0, #0
107		bl fflush
108		mov sp, fp
109		pop {fp, pc}
110	
111	@ length of .L._println_str0
112		.word 0
113	.L._println_str0:
114		.asciz ""
115	.align 4
116	_println:
117		push {fp, lr}
118		mov fp, sp
119		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
120		bic sp, sp, #0x7
121		adr r0, .L._println_str0
122		bl puts
123		mov r0, #0
124		bl fflush
125		mov sp, fp
126		pop {fp, pc}
127	
128	_exit:
129		push {fp, lr}
130		mov fp, sp
131		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
132		bic sp, sp, #0x7
133		bl exit
134		mov sp, fp
135		pop {fp, pc}
136	
137	@ length of .L._errOverflow_str0
138		.word 52
139	.L._errOverflow_str0:
140		.asciz "fatal error: integer overflow or underflow occurred\n"
141	.align 4
142	_errOverflow:
143		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
144		bic sp, sp, #0x7
145		adr r0, .L._errOverflow_str0
146		bl _prints
147		mov r0, #255
148		bl _exit
===========================================================
-- Finished

