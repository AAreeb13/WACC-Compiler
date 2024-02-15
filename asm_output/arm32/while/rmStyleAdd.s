./valid/while/rmStyleAdd.wacc
calling the reference compiler on ./valid/while/rmStyleAdd.wacc
-- Test: rmStyleAdd.wacc

-- Uploaded file: 
---------------------------------------------------------------
# register machine style addition

# Output:
# initial value of x: 3
# (+)(+)(+)(+)(+)(+)(+)
# final value of x: 10
#

# Program:

begin
  int x = 3 ;
  int y = 7 ;
  print "initial value of x: " ;
  println x ;
  while y > 0 do
    print "(+)" ;
    x = x + 1 ;
    y = y - 1
  done ;
  println "" ;
  print "final value of x: " ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
rmStyleAdd.s contents are:
===========================================================
0	.data
1	@ length of .L.str0
2		.word 20
3	.L.str0:
4		.asciz "initial value of x: "
5	@ length of .L.str1
6		.word 3
7	.L.str1:
8		.asciz "(+)"
9	@ length of .L.str2
10		.word 0
11	.L.str2:
12		.asciz ""
13	@ length of .L.str3
14		.word 18
15	.L.str3:
16		.asciz "final value of x: "
17	.align 4
18	.text
19	.global main
20	main:
21		push {fp, lr}
22		push {r4, r5, r8, r10, r12}
23		mov fp, sp
24		@ Stack pointer unchanged, no stack allocated variables
25		mov r8, #3
26		mov r4, r8
27		mov r8, #7
28		mov r5, r8
29		@ Stack pointer unchanged, no stack allocated arguments
30		ldr r8, =.L.str0
31		push {r8}
32		pop {r8}
33		mov r8, r8
34		mov r0, r8
35		@ statement primitives do not return results (but will clobber r0/rax)
36		bl _prints
37		@ Stack pointer unchanged, no stack allocated arguments
38		mov r8, r4
39		mov r0, r8
40		@ statement primitives do not return results (but will clobber r0/rax)
41		bl _printi
42		bl _println
43		b .L0
44	.L1:
45		@ Stack pointer unchanged, no stack allocated arguments
46		ldr r8, =.L.str1
47		push {r8}
48		pop {r8}
49		mov r8, r8
50		mov r0, r8
51		@ statement primitives do not return results (but will clobber r0/rax)
52		bl _prints
53		adds r8, r4, #1
54		blvs _errOverflow
55		push {r8}
56		pop {r8}
57		mov r8, r8
58		mov r4, r8
59		subs r8, r5, #1
60		blvs _errOverflow
61		push {r8}
62		pop {r8}
63		mov r8, r8
64		mov r5, r8
65	.L0:
66		cmp r5, #0
67		bgt .L1
68		@ Stack pointer unchanged, no stack allocated arguments
69		ldr r8, =.L.str2
70		push {r8}
71		pop {r8}
72		mov r8, r8
73		mov r0, r8
74		@ statement primitives do not return results (but will clobber r0/rax)
75		bl _prints
76		bl _println
77		@ Stack pointer unchanged, no stack allocated arguments
78		ldr r8, =.L.str3
79		push {r8}
80		pop {r8}
81		mov r8, r8
82		mov r0, r8
83		@ statement primitives do not return results (but will clobber r0/rax)
84		bl _prints
85		@ Stack pointer unchanged, no stack allocated arguments
86		mov r8, r4
87		mov r0, r8
88		@ statement primitives do not return results (but will clobber r0/rax)
89		bl _printi
90		bl _println
91		@ Stack pointer unchanged, no stack allocated variables
92		mov r0, #0
93		pop {r4, r5, r8, r10, r12}
94		pop {fp, pc}
95	
96	@ length of .L._prints_str0
97		.word 4
98	.L._prints_str0:
99		.asciz "%.*s"
100	.align 4
101	_prints:
102		push {fp, lr}
103		mov fp, sp
104		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
105		bic sp, sp, #0x7
106		mov r2, r0
107		ldr r1, [r0, #-4]
108		adr r0, .L._prints_str0
109		bl printf
110		mov r0, #0
111		bl fflush
112		mov sp, fp
113		pop {fp, pc}
114	
115	@ length of .L._printi_str0
116		.word 2
117	.L._printi_str0:
118		.asciz "%d"
119	.align 4
120	_printi:
121		push {fp, lr}
122		mov fp, sp
123		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
124		bic sp, sp, #0x7
125		mov r1, r0
126		adr r0, .L._printi_str0
127		bl printf
128		mov r0, #0
129		bl fflush
130		mov sp, fp
131		pop {fp, pc}
132	
133	@ length of .L._println_str0
134		.word 0
135	.L._println_str0:
136		.asciz ""
137	.align 4
138	_println:
139		push {fp, lr}
140		mov fp, sp
141		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
142		bic sp, sp, #0x7
143		adr r0, .L._println_str0
144		bl puts
145		mov r0, #0
146		bl fflush
147		mov sp, fp
148		pop {fp, pc}
149	
150	_exit:
151		push {fp, lr}
152		mov fp, sp
153		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
154		bic sp, sp, #0x7
155		bl exit
156		mov sp, fp
157		pop {fp, pc}
158	
159	@ length of .L._errOverflow_str0
160		.word 52
161	.L._errOverflow_str0:
162		.asciz "fatal error: integer overflow or underflow occurred\n"
163	.align 4
164	_errOverflow:
165		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
166		bic sp, sp, #0x7
167		adr r0, .L._errOverflow_str0
168		bl _prints
169		mov r0, #255
170		bl _exit
===========================================================
-- Finished

