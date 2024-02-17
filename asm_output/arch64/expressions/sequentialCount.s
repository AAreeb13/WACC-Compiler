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
1	// length of .L.str0
2		.word 20
3	.L.str0:
4		.asciz "Can you count to 10?"
5	.align 4
6	.text
7	.global main
8	main:
9		// push {fp, lr}
10		stp fp, lr, [sp, #-16]!
11		// push {x19}
12		stp x19, xzr, [sp, #-16]!
13		mov fp, sp
14		// Stack pointer unchanged, no stack allocated variables
15		mov x8, #1
16		mov x19, x8
17		// Stack pointer unchanged, no stack allocated arguments
18		adrp x8, .L.str0
19		add x8, x8, :lo12:.L.str0
20		// push {x8}
21		stp x8, xzr, [sp, #-16]!
22		// pop {x8}
23		ldp x8, xzr, [sp], #16
24		mov x8, x8
25		mov x0, x8
26		// statement primitives do not return results (but will clobber r0/rax)
27		bl _prints
28		bl _println
29		// Stack pointer unchanged, no stack allocated arguments
30		mov x8, x19
31		mov x0, x8
32		// statement primitives do not return results (but will clobber r0/rax)
33		bl _printi
34		bl _println
35		adds w8, w19, #1
36		b.vs _errOverflow
37		sxtw x8, w8
38		// push {x8}
39		stp x8, xzr, [sp, #-16]!
40		// pop {x8}
41		ldp x8, xzr, [sp], #16
42		mov x8, x8
43		mov x19, x8
44		// Stack pointer unchanged, no stack allocated arguments
45		mov x8, x19
46		mov x0, x8
47		// statement primitives do not return results (but will clobber r0/rax)
48		bl _printi
49		bl _println
50		adds w8, w19, #1
51		b.vs _errOverflow
52		sxtw x8, w8
53		// push {x8}
54		stp x8, xzr, [sp, #-16]!
55		// pop {x8}
56		ldp x8, xzr, [sp], #16
57		mov x8, x8
58		mov x19, x8
59		// Stack pointer unchanged, no stack allocated arguments
60		mov x8, x19
61		mov x0, x8
62		// statement primitives do not return results (but will clobber r0/rax)
63		bl _printi
64		bl _println
65		adds w8, w19, #1
66		b.vs _errOverflow
67		sxtw x8, w8
68		// push {x8}
69		stp x8, xzr, [sp, #-16]!
70		// pop {x8}
71		ldp x8, xzr, [sp], #16
72		mov x8, x8
73		mov x19, x8
74		// Stack pointer unchanged, no stack allocated arguments
75		mov x8, x19
76		mov x0, x8
77		// statement primitives do not return results (but will clobber r0/rax)
78		bl _printi
79		bl _println
80		adds w8, w19, #1
81		b.vs _errOverflow
82		sxtw x8, w8
83		// push {x8}
84		stp x8, xzr, [sp, #-16]!
85		// pop {x8}
86		ldp x8, xzr, [sp], #16
87		mov x8, x8
88		mov x19, x8
89		// Stack pointer unchanged, no stack allocated arguments
90		mov x8, x19
91		mov x0, x8
92		// statement primitives do not return results (but will clobber r0/rax)
93		bl _printi
94		bl _println
95		adds w8, w19, #1
96		b.vs _errOverflow
97		sxtw x8, w8
98		// push {x8}
99		stp x8, xzr, [sp, #-16]!
100		// pop {x8}
101		ldp x8, xzr, [sp], #16
102		mov x8, x8
103		mov x19, x8
104		// Stack pointer unchanged, no stack allocated arguments
105		mov x8, x19
106		mov x0, x8
107		// statement primitives do not return results (but will clobber r0/rax)
108		bl _printi
109		bl _println
110		adds w8, w19, #1
111		b.vs _errOverflow
112		sxtw x8, w8
113		// push {x8}
114		stp x8, xzr, [sp, #-16]!
115		// pop {x8}
116		ldp x8, xzr, [sp], #16
117		mov x8, x8
118		mov x19, x8
119		// Stack pointer unchanged, no stack allocated arguments
120		mov x8, x19
121		mov x0, x8
122		// statement primitives do not return results (but will clobber r0/rax)
123		bl _printi
124		bl _println
125		adds w8, w19, #1
126		b.vs _errOverflow
127		sxtw x8, w8
128		// push {x8}
129		stp x8, xzr, [sp, #-16]!
130		// pop {x8}
131		ldp x8, xzr, [sp], #16
132		mov x8, x8
133		mov x19, x8
134		// Stack pointer unchanged, no stack allocated arguments
135		mov x8, x19
136		mov x0, x8
137		// statement primitives do not return results (but will clobber r0/rax)
138		bl _printi
139		bl _println
140		adds w8, w19, #1
141		b.vs _errOverflow
142		sxtw x8, w8
143		// push {x8}
144		stp x8, xzr, [sp, #-16]!
145		// pop {x8}
146		ldp x8, xzr, [sp], #16
147		mov x8, x8
148		mov x19, x8
149		// Stack pointer unchanged, no stack allocated arguments
150		mov x8, x19
151		mov x0, x8
152		// statement primitives do not return results (but will clobber r0/rax)
153		bl _printi
154		bl _println
155		adds w8, w19, #1
156		b.vs _errOverflow
157		sxtw x8, w8
158		// push {x8}
159		stp x8, xzr, [sp, #-16]!
160		// pop {x8}
161		ldp x8, xzr, [sp], #16
162		mov x8, x8
163		mov x19, x8
164		// Stack pointer unchanged, no stack allocated arguments
165		mov x8, x19
166		mov x0, x8
167		// statement primitives do not return results (but will clobber r0/rax)
168		bl _printi
169		bl _println
170		// Stack pointer unchanged, no stack allocated variables
171		mov x0, #0
172		// pop {x19}
173		ldp x19, xzr, [sp], #16
174		// pop {fp, lr}
175		ldp fp, lr, [sp], #16
176		ret
177	
178	// length of .L._prints_str0
179		.word 4
180	.L._prints_str0:
181		.asciz "%.*s"
182	.align 4
183	_prints:
184		// push {lr}
185		stp lr, xzr, [sp, #-16]!
186		mov x2, x0
187		ldrsw x1, [x0, #-4]
188		adr x0, .L._prints_str0
189		bl printf
190		mov x0, #0
191		bl fflush
192		// pop {lr}
193		ldp lr, xzr, [sp], #16
194		ret
195	
196	// length of .L._printi_str0
197		.word 2
198	.L._printi_str0:
199		.asciz "%d"
200	.align 4
201	_printi:
202		// push {lr}
203		stp lr, xzr, [sp, #-16]!
204		mov x1, x0
205		adr x0, .L._printi_str0
206		bl printf
207		mov x0, #0
208		bl fflush
209		// pop {lr}
210		ldp lr, xzr, [sp], #16
211		ret
212	
213	// length of .L._println_str0
214		.word 0
215	.L._println_str0:
216		.asciz ""
217	.align 4
218	_println:
219		// push {lr}
220		stp lr, xzr, [sp, #-16]!
221		adr x0, .L._println_str0
222		bl puts
223		mov x0, #0
224		bl fflush
225		// pop {lr}
226		ldp lr, xzr, [sp], #16
227		ret
228	
229	// length of .L._errOverflow_str0
230		.word 52
231	.L._errOverflow_str0:
232		.asciz "fatal error: integer overflow or underflow occurred\n"
233	.align 4
234	_errOverflow:
235		adr x0, .L._errOverflow_str0
236		bl _prints
237		mov w0, #-1
238		bl exit
===========================================================
-- Finished

