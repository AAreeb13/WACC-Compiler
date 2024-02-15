./valid/pairs/printPair.wacc
calling the reference compiler on ./valid/pairs/printPair.wacc
-- Test: printPair.wacc

-- Uploaded file: 
---------------------------------------------------------------
# print pair program

# Output:
# #addrs# = (10, a)
#

# Program:

begin
  pair(int, char) p = newpair(10, 'a') ;
  print p ;
  print " = (" ;
  int x = fst p ;
  print x ;
  print ", " ;
  char c = snd p ;
  print c ;
  println ')'
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
printPair.s contents are:
===========================================================
0	.data
1	@ length of .L.str0
2		.word 4
3	.L.str0:
4		.asciz " = ("
5	@ length of .L.str1
6		.word 2
7	.L.str1:
8		.asciz ", "
9	.align 4
10	.text
11	.global main
12	main:
13		push {fp, lr}
14		push {r4, r5, r6, r8, r10, r12}
15		mov fp, sp
16		@ Stack pointer unchanged, no stack allocated variables
17		mov r0, #8
18		bl _malloc
19		mov r12, r0
20		mov r8, #10
21		str r8, [r12, #0]
22		mov r8, #97
23		str r8, [r12, #4]
24		mov r8, r12
25		mov r4, r8
26		@ Stack pointer unchanged, no stack allocated arguments
27		mov r8, r4
28		mov r0, r8
29		@ statement primitives do not return results (but will clobber r0/rax)
30		bl _printp
31		@ Stack pointer unchanged, no stack allocated arguments
32		ldr r8, =.L.str0
33		push {r8}
34		pop {r8}
35		mov r8, r8
36		mov r0, r8
37		@ statement primitives do not return results (but will clobber r0/rax)
38		bl _prints
39		cmp r4, #0
40		bleq _errNull
41		ldr r8, [r4, #0]
42		mov r8, r8
43		mov r5, r8
44		@ Stack pointer unchanged, no stack allocated arguments
45		mov r8, r5
46		mov r0, r8
47		@ statement primitives do not return results (but will clobber r0/rax)
48		bl _printi
49		@ Stack pointer unchanged, no stack allocated arguments
50		ldr r8, =.L.str1
51		push {r8}
52		pop {r8}
53		mov r8, r8
54		mov r0, r8
55		@ statement primitives do not return results (but will clobber r0/rax)
56		bl _prints
57		cmp r4, #0
58		bleq _errNull
59		ldr r8, [r4, #4]
60		mov r8, r8
61		mov r6, r8
62		@ Stack pointer unchanged, no stack allocated arguments
63		mov r8, r6
64		mov r0, r8
65		@ statement primitives do not return results (but will clobber r0/rax)
66		bl _printc
67		@ Stack pointer unchanged, no stack allocated arguments
68		mov r8, #41
69		mov r0, r8
70		@ statement primitives do not return results (but will clobber r0/rax)
71		bl _printc
72		bl _println
73		@ Stack pointer unchanged, no stack allocated variables
74		mov r0, #0
75		pop {r4, r5, r6, r8, r10, r12}
76		pop {fp, pc}
77	
78	@ length of .L._prints_str0
79		.word 4
80	.L._prints_str0:
81		.asciz "%.*s"
82	.align 4
83	_prints:
84		push {fp, lr}
85		mov fp, sp
86		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
87		bic sp, sp, #0x7
88		mov r2, r0
89		ldr r1, [r0, #-4]
90		adr r0, .L._prints_str0
91		bl printf
92		mov r0, #0
93		bl fflush
94		mov sp, fp
95		pop {fp, pc}
96	
97	@ length of .L._printp_str0
98		.word 2
99	.L._printp_str0:
100		.asciz "%p"
101	.align 4
102	_printp:
103		push {fp, lr}
104		mov fp, sp
105		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
106		bic sp, sp, #0x7
107		mov r1, r0
108		adr r0, .L._printp_str0
109		bl printf
110		mov r0, #0
111		bl fflush
112		mov sp, fp
113		pop {fp, pc}
114	
115	_malloc:
116		push {fp, lr}
117		mov fp, sp
118		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
119		bic sp, sp, #0x7
120		bl malloc
121		cmp r0, #0
122		bleq _errOutOfMemory
123		mov sp, fp
124		pop {fp, pc}
125	
126	@ length of .L._printi_str0
127		.word 2
128	.L._printi_str0:
129		.asciz "%d"
130	.align 4
131	_printi:
132		push {fp, lr}
133		mov fp, sp
134		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
135		bic sp, sp, #0x7
136		mov r1, r0
137		adr r0, .L._printi_str0
138		bl printf
139		mov r0, #0
140		bl fflush
141		mov sp, fp
142		pop {fp, pc}
143	
144	@ length of .L._printc_str0
145		.word 2
146	.L._printc_str0:
147		.asciz "%c"
148	.align 4
149	_printc:
150		push {fp, lr}
151		mov fp, sp
152		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
153		bic sp, sp, #0x7
154		mov r1, r0
155		adr r0, .L._printc_str0
156		bl printf
157		mov r0, #0
158		bl fflush
159		mov sp, fp
160		pop {fp, pc}
161	
162	@ length of .L._println_str0
163		.word 0
164	.L._println_str0:
165		.asciz ""
166	.align 4
167	_println:
168		push {fp, lr}
169		mov fp, sp
170		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
171		bic sp, sp, #0x7
172		adr r0, .L._println_str0
173		bl puts
174		mov r0, #0
175		bl fflush
176		mov sp, fp
177		pop {fp, pc}
178	
179	_exit:
180		push {fp, lr}
181		mov fp, sp
182		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
183		bic sp, sp, #0x7
184		bl exit
185		mov sp, fp
186		pop {fp, pc}
187	
188	@ length of .L._errOutOfMemory_str0
189		.word 27
190	.L._errOutOfMemory_str0:
191		.asciz "fatal error: out of memory\n"
192	.align 4
193	_errOutOfMemory:
194		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
195		bic sp, sp, #0x7
196		adr r0, .L._errOutOfMemory_str0
197		bl _prints
198		mov r0, #255
199		bl _exit
200	
201	@ length of .L._errNull_str0
202		.word 45
203	.L._errNull_str0:
204		.asciz "fatal error: null pair dereferenced or freed\n"
205	.align 4
206	_errNull:
207		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
208		bic sp, sp, #0x7
209		adr r0, .L._errNull_str0
210		bl _prints
211		mov r0, #255
212		bl _exit
===========================================================
-- Finished

