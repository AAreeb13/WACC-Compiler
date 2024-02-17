./valid/expressions/sequentialCount.wacc
calling the reference compiler on ./valid/expressions/sequentialCount.wacc
-- Test: sequentialCount.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple sequential counting

# Output:
# Can you count to 10?
# 1
# 2
# 3
# 4
# 5
# 6
# 7
# 8
# 9
# 10
#

# Program:

begin
  int i = 1 ;
  println "Can you count to 10?" ;
  println i ;
  i = i + 1 ;
  println i ;
  i = i + 1 ;
  println i ;
  i = i + 1 ;
  println i ;
  i = i + 1 ;
  println i ;
  i = i + 1 ;
  println i ;
  i = i + 1 ;
  println i ;
  i = i + 1 ;
  println i ;
  i = i + 1 ;
  println i ;
  i = i + 1 ;
  println i
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
sequentialCount.s contents are:
===========================================================
0	.data
1	@ length of .L.str0
2		.word 20
3	.L.str0:
4		.asciz "Can you count to 10?"
5	.align 4
6	.text
7	.global main
8	main:
9		push {fp, lr}
10		push {r4, r8, r10, r12}
11		mov fp, sp
12		@ Stack pointer unchanged, no stack allocated variables
13		mov r8, #1
14		mov r4, r8
15		@ Stack pointer unchanged, no stack allocated arguments
16		ldr r8, =.L.str0
17		push {r8}
18		pop {r8}
19		mov r8, r8
20		mov r0, r8
21		@ statement primitives do not return results (but will clobber r0/rax)
22		bl _prints
23		bl _println
24		@ Stack pointer unchanged, no stack allocated arguments
25		mov r8, r4
26		mov r0, r8
27		@ statement primitives do not return results (but will clobber r0/rax)
28		bl _printi
29		bl _println
30		adds r8, r4, #1
31		blvs _errOverflow
32		push {r8}
33		pop {r8}
34		mov r8, r8
35		mov r4, r8
36		@ Stack pointer unchanged, no stack allocated arguments
37		mov r8, r4
38		mov r0, r8
39		@ statement primitives do not return results (but will clobber r0/rax)
40		bl _printi
41		bl _println
42		adds r8, r4, #1
43		blvs _errOverflow
44		push {r8}
45		pop {r8}
46		mov r8, r8
47		mov r4, r8
48		@ Stack pointer unchanged, no stack allocated arguments
49		mov r8, r4
50		mov r0, r8
51		@ statement primitives do not return results (but will clobber r0/rax)
52		bl _printi
53		bl _println
54		adds r8, r4, #1
55		blvs _errOverflow
56		push {r8}
57		pop {r8}
58		mov r8, r8
59		mov r4, r8
60		@ Stack pointer unchanged, no stack allocated arguments
61		mov r8, r4
62		mov r0, r8
63		@ statement primitives do not return results (but will clobber r0/rax)
64		bl _printi
65		bl _println
66		adds r8, r4, #1
67		blvs _errOverflow
68		push {r8}
69		pop {r8}
70		mov r8, r8
71		mov r4, r8
72		@ Stack pointer unchanged, no stack allocated arguments
73		mov r8, r4
74		mov r0, r8
75		@ statement primitives do not return results (but will clobber r0/rax)
76		bl _printi
77		bl _println
78		adds r8, r4, #1
79		blvs _errOverflow
80		push {r8}
81		pop {r8}
82		mov r8, r8
83		mov r4, r8
84		@ Stack pointer unchanged, no stack allocated arguments
85		mov r8, r4
86		mov r0, r8
87		@ statement primitives do not return results (but will clobber r0/rax)
88		bl _printi
89		bl _println
90		adds r8, r4, #1
91		blvs _errOverflow
92		push {r8}
93		pop {r8}
94		mov r8, r8
95		mov r4, r8
96		@ Stack pointer unchanged, no stack allocated arguments
97		mov r8, r4
98		mov r0, r8
99		@ statement primitives do not return results (but will clobber r0/rax)
100		bl _printi
101		bl _println
102		adds r8, r4, #1
103		blvs _errOverflow
104		push {r8}
105		pop {r8}
106		mov r8, r8
107		mov r4, r8
108		@ Stack pointer unchanged, no stack allocated arguments
109		mov r8, r4
110		mov r0, r8
111		@ statement primitives do not return results (but will clobber r0/rax)
112		bl _printi
113		bl _println
114		adds r8, r4, #1
115		blvs _errOverflow
116		push {r8}
117		pop {r8}
118		mov r8, r8
119		mov r4, r8
120		@ Stack pointer unchanged, no stack allocated arguments
121		mov r8, r4
122		mov r0, r8
123		@ statement primitives do not return results (but will clobber r0/rax)
124		bl _printi
125		bl _println
126		adds r8, r4, #1
127		blvs _errOverflow
128		push {r8}
129		pop {r8}
130		mov r8, r8
131		mov r4, r8
132		@ Stack pointer unchanged, no stack allocated arguments
133		mov r8, r4
134		mov r0, r8
135		@ statement primitives do not return results (but will clobber r0/rax)
136		bl _printi
137		bl _println
138		@ Stack pointer unchanged, no stack allocated variables
139		mov r0, #0
140		pop {r4, r8, r10, r12}
141		pop {fp, pc}
142	
143	@ length of .L._prints_str0
144		.word 4
145	.L._prints_str0:
146		.asciz "%.*s"
147	.align 4
148	_prints:
149		push {fp, lr}
150		mov fp, sp
151		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
152		bic sp, sp, #0x7
153		mov r2, r0
154		ldr r1, [r0, #-4]
155		adr r0, .L._prints_str0
156		bl printf
157		mov r0, #0
158		bl fflush
159		mov sp, fp
160		pop {fp, pc}
161	
162	@ length of .L._printi_str0
163		.word 2
164	.L._printi_str0:
165		.asciz "%d"
166	.align 4
167	_printi:
168		push {fp, lr}
169		mov fp, sp
170		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
171		bic sp, sp, #0x7
172		mov r1, r0
173		adr r0, .L._printi_str0
174		bl printf
175		mov r0, #0
176		bl fflush
177		mov sp, fp
178		pop {fp, pc}
179	
180	@ length of .L._println_str0
181		.word 0
182	.L._println_str0:
183		.asciz ""
184	.align 4
185	_println:
186		push {fp, lr}
187		mov fp, sp
188		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
189		bic sp, sp, #0x7
190		adr r0, .L._println_str0
191		bl puts
192		mov r0, #0
193		bl fflush
194		mov sp, fp
195		pop {fp, pc}
196	
197	_exit:
198		push {fp, lr}
199		mov fp, sp
200		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
201		bic sp, sp, #0x7
202		bl exit
203		mov sp, fp
204		pop {fp, pc}
205	
206	@ length of .L._errOverflow_str0
207		.word 52
208	.L._errOverflow_str0:
209		.asciz "fatal error: integer overflow or underflow occurred\n"
210	.align 4
211	_errOverflow:
212		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
213		bic sp, sp, #0x7
214		adr r0, .L._errOverflow_str0
215		bl _prints
216		mov r0, #255
217		bl _exit
===========================================================
-- Finished

