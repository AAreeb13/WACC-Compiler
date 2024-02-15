./valid/pairs/printPairOfNulls.wacc
calling the reference compiler on ./valid/pairs/printPairOfNulls.wacc
-- Test: printPairOfNulls.wacc

-- Uploaded file: 
---------------------------------------------------------------
# print a pair of null pairs

# Output:
# #addrs# = ((nil),(nil))
#

# Program:

begin
  pair(pair, pair) p = newpair(null, null) ;
  print p ;
  print " = (" ;
  pair(pair, pair) q = fst p ;
  print q ;
  print "," ;
  pair(int, bool) r = snd p ;
  print r ;
  println ")"
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
printPairOfNulls.s contents are:
===========================================================
0	.data
1	@ length of .L.str0
2		.word 4
3	.L.str0:
4		.asciz " = ("
5	@ length of .L.str1
6		.word 1
7	.L.str1:
8		.asciz ","
9	@ length of .L.str2
10		.word 1
11	.L.str2:
12		.asciz ")"
13	.align 4
14	.text
15	.global main
16	main:
17		push {fp, lr}
18		push {r4, r5, r6, r8, r10, r12}
19		mov fp, sp
20		@ Stack pointer unchanged, no stack allocated variables
21		mov r0, #8
22		bl _malloc
23		mov r12, r0
24		mov r8, #0
25		str r8, [r12, #0]
26		mov r8, #0
27		str r8, [r12, #4]
28		mov r8, r12
29		mov r4, r8
30		@ Stack pointer unchanged, no stack allocated arguments
31		mov r8, r4
32		mov r0, r8
33		@ statement primitives do not return results (but will clobber r0/rax)
34		bl _printp
35		@ Stack pointer unchanged, no stack allocated arguments
36		ldr r8, =.L.str0
37		push {r8}
38		pop {r8}
39		mov r8, r8
40		mov r0, r8
41		@ statement primitives do not return results (but will clobber r0/rax)
42		bl _prints
43		cmp r4, #0
44		bleq _errNull
45		ldr r8, [r4, #0]
46		mov r8, r8
47		mov r5, r8
48		@ Stack pointer unchanged, no stack allocated arguments
49		mov r8, r5
50		mov r0, r8
51		@ statement primitives do not return results (but will clobber r0/rax)
52		bl _printp
53		@ Stack pointer unchanged, no stack allocated arguments
54		ldr r8, =.L.str1
55		push {r8}
56		pop {r8}
57		mov r8, r8
58		mov r0, r8
59		@ statement primitives do not return results (but will clobber r0/rax)
60		bl _prints
61		cmp r4, #0
62		bleq _errNull
63		ldr r8, [r4, #4]
64		mov r8, r8
65		mov r6, r8
66		@ Stack pointer unchanged, no stack allocated arguments
67		mov r8, r6
68		mov r0, r8
69		@ statement primitives do not return results (but will clobber r0/rax)
70		bl _printp
71		@ Stack pointer unchanged, no stack allocated arguments
72		ldr r8, =.L.str2
73		push {r8}
74		pop {r8}
75		mov r8, r8
76		mov r0, r8
77		@ statement primitives do not return results (but will clobber r0/rax)
78		bl _prints
79		bl _println
80		@ Stack pointer unchanged, no stack allocated variables
81		mov r0, #0
82		pop {r4, r5, r6, r8, r10, r12}
83		pop {fp, pc}
84	
85	@ length of .L._prints_str0
86		.word 4
87	.L._prints_str0:
88		.asciz "%.*s"
89	.align 4
90	_prints:
91		push {fp, lr}
92		mov fp, sp
93		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
94		bic sp, sp, #0x7
95		mov r2, r0
96		ldr r1, [r0, #-4]
97		adr r0, .L._prints_str0
98		bl printf
99		mov r0, #0
100		bl fflush
101		mov sp, fp
102		pop {fp, pc}
103	
104	@ length of .L._printp_str0
105		.word 2
106	.L._printp_str0:
107		.asciz "%p"
108	.align 4
109	_printp:
110		push {fp, lr}
111		mov fp, sp
112		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
113		bic sp, sp, #0x7
114		mov r1, r0
115		adr r0, .L._printp_str0
116		bl printf
117		mov r0, #0
118		bl fflush
119		mov sp, fp
120		pop {fp, pc}
121	
122	_malloc:
123		push {fp, lr}
124		mov fp, sp
125		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
126		bic sp, sp, #0x7
127		bl malloc
128		cmp r0, #0
129		bleq _errOutOfMemory
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
159	@ length of .L._errOutOfMemory_str0
160		.word 27
161	.L._errOutOfMemory_str0:
162		.asciz "fatal error: out of memory\n"
163	.align 4
164	_errOutOfMemory:
165		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
166		bic sp, sp, #0x7
167		adr r0, .L._errOutOfMemory_str0
168		bl _prints
169		mov r0, #255
170		bl _exit
171	
172	@ length of .L._errNull_str0
173		.word 45
174	.L._errNull_str0:
175		.asciz "fatal error: null pair dereferenced or freed\n"
176	.align 4
177	_errNull:
178		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
179		bic sp, sp, #0x7
180		adr r0, .L._errNull_str0
181		bl _prints
182		mov r0, #255
183		bl _exit
===========================================================
-- Finished

