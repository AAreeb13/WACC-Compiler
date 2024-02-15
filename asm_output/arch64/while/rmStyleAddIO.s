./valid/while/rmStyleAddIO.wacc
calling the reference compiler on ./valid/while/rmStyleAddIO.wacc
-- Test: rmStyleAddIO.wacc

-- Uploaded file: 
---------------------------------------------------------------
# register machine style addition

# Input: 2 40

# Output:
# Enter the first number: Enter the second number: Initial value of x: 2
# (+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)
# final value of x: 42
#

# Program:

begin
  int x = 0 ;
  int y = 0 ;
  print "Enter the first number: " ;
  read x ;
  print "Enter the second number: " ;
  read y ;
  print "Initial value of x: " ;
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
rmStyleAddIO.s contents are:
===========================================================
0	.data
1	// length of .L.str0
2		.word 24
3	.L.str0:
4		.asciz "Enter the first number: "
5	// length of .L.str1
6		.word 25
7	.L.str1:
8		.asciz "Enter the second number: "
9	// length of .L.str2
10		.word 20
11	.L.str2:
12		.asciz "Initial value of x: "
13	// length of .L.str3
14		.word 3
15	.L.str3:
16		.asciz "(+)"
17	// length of .L.str4
18		.word 0
19	.L.str4:
20		.asciz ""
21	// length of .L.str5
22		.word 18
23	.L.str5:
24		.asciz "final value of x: "
25	.align 4
26	.text
27	.global main
28	main:
29		// push {fp, lr}
30		stp fp, lr, [sp, #-16]!
31		// push {x19, x20}
32		stp x19, x20, [sp, #-16]!
33		mov fp, sp
34		// Stack pointer unchanged, no stack allocated variables
35		mov x8, #0
36		mov x19, x8
37		mov x8, #0
38		mov x20, x8
39		// Stack pointer unchanged, no stack allocated arguments
40		adrp x8, .L.str0
41		add x8, x8, :lo12:.L.str0
42		// push {x8}
43		stp x8, xzr, [sp, #-16]!
44		// pop {x8}
45		ldp x8, xzr, [sp], #16
46		mov x8, x8
47		mov x0, x8
48		// statement primitives do not return results (but will clobber r0/rax)
49		bl _prints
50		// Stack pointer unchanged, no stack allocated arguments
51		// load the current value in the destination of the read so it supports defaults
52		mov x8, x19
53		mov x0, x8
54		bl _readi
55		mov x16, x0
56		mov x8, x16
57		mov x19, x8
58		// Stack pointer unchanged, no stack allocated arguments
59		adrp x8, .L.str1
60		add x8, x8, :lo12:.L.str1
61		// push {x8}
62		stp x8, xzr, [sp, #-16]!
63		// pop {x8}
64		ldp x8, xzr, [sp], #16
65		mov x8, x8
66		mov x0, x8
67		// statement primitives do not return results (but will clobber r0/rax)
68		bl _prints
69		// Stack pointer unchanged, no stack allocated arguments
70		// load the current value in the destination of the read so it supports defaults
71		mov x8, x20
72		mov x0, x8
73		bl _readi
74		mov x16, x0
75		mov x8, x16
76		mov x20, x8
77		// Stack pointer unchanged, no stack allocated arguments
78		adrp x8, .L.str2
79		add x8, x8, :lo12:.L.str2
80		// push {x8}
81		stp x8, xzr, [sp, #-16]!
82		// pop {x8}
83		ldp x8, xzr, [sp], #16
84		mov x8, x8
85		mov x0, x8
86		// statement primitives do not return results (but will clobber r0/rax)
87		bl _prints
88		// Stack pointer unchanged, no stack allocated arguments
89		mov x8, x19
90		mov x0, x8
91		// statement primitives do not return results (but will clobber r0/rax)
92		bl _printi
93		bl _println
94		b .L0
95	.L1:
96		// Stack pointer unchanged, no stack allocated arguments
97		adrp x8, .L.str3
98		add x8, x8, :lo12:.L.str3
99		// push {x8}
100		stp x8, xzr, [sp, #-16]!
101		// pop {x8}
102		ldp x8, xzr, [sp], #16
103		mov x8, x8
104		mov x0, x8
105		// statement primitives do not return results (but will clobber r0/rax)
106		bl _prints
107		adds w8, w19, #1
108		b.vs _errOverflow
109		sxtw x8, w8
110		// push {x8}
111		stp x8, xzr, [sp, #-16]!
112		// pop {x8}
113		ldp x8, xzr, [sp], #16
114		mov x8, x8
115		mov x19, x8
116		subs w8, w20, #1
117		b.vs _errOverflow
118		sxtw x8, w8
119		// push {x8}
120		stp x8, xzr, [sp, #-16]!
121		// pop {x8}
122		ldp x8, xzr, [sp], #16
123		mov x8, x8
124		mov x20, x8
125	.L0:
126		cmp x20, #0
127		b.gt .L1
128		// Stack pointer unchanged, no stack allocated arguments
129		adrp x8, .L.str4
130		add x8, x8, :lo12:.L.str4
131		// push {x8}
132		stp x8, xzr, [sp, #-16]!
133		// pop {x8}
134		ldp x8, xzr, [sp], #16
135		mov x8, x8
136		mov x0, x8
137		// statement primitives do not return results (but will clobber r0/rax)
138		bl _prints
139		bl _println
140		// Stack pointer unchanged, no stack allocated arguments
141		adrp x8, .L.str5
142		add x8, x8, :lo12:.L.str5
143		// push {x8}
144		stp x8, xzr, [sp, #-16]!
145		// pop {x8}
146		ldp x8, xzr, [sp], #16
147		mov x8, x8
148		mov x0, x8
149		// statement primitives do not return results (but will clobber r0/rax)
150		bl _prints
151		// Stack pointer unchanged, no stack allocated arguments
152		mov x8, x19
153		mov x0, x8
154		// statement primitives do not return results (but will clobber r0/rax)
155		bl _printi
156		bl _println
157		// Stack pointer unchanged, no stack allocated variables
158		mov x0, #0
159		// pop {x19, x20}
160		ldp x19, x20, [sp], #16
161		// pop {fp, lr}
162		ldp fp, lr, [sp], #16
163		ret
164	
165	// length of .L._prints_str0
166		.word 4
167	.L._prints_str0:
168		.asciz "%.*s"
169	.align 4
170	_prints:
171		// push {lr}
172		stp lr, xzr, [sp, #-16]!
173		mov x2, x0
174		ldrsw x1, [x0, #-4]
175		adr x0, .L._prints_str0
176		bl printf
177		mov x0, #0
178		bl fflush
179		// pop {lr}
180		ldp lr, xzr, [sp], #16
181		ret
182	
183	// length of .L._readi_str0
184		.word 2
185	.L._readi_str0:
186		.asciz "%d"
187	.align 4
188	_readi:
189		// X0 contains the "original" value of the destination of the read
190		// allocate space on the stack to store the read: preserve alignment!
191		// the passed default argument should be stored in case of EOF
192		// aarch64 mandates 16-byte SP alignment at all times, might as well merge the stores
193		// push {x0, lr}
194		stp x0, lr, [sp, #-16]!
195		mov x1, sp
196		adr x0, .L._readi_str0
197		bl scanf
198		// pop {x0, lr}
199		ldp x0, lr, [sp], #16
200		ret
201	
202	// length of .L._printi_str0
203		.word 2
204	.L._printi_str0:
205		.asciz "%d"
206	.align 4
207	_printi:
208		// push {lr}
209		stp lr, xzr, [sp, #-16]!
210		mov x1, x0
211		adr x0, .L._printi_str0
212		bl printf
213		mov x0, #0
214		bl fflush
215		// pop {lr}
216		ldp lr, xzr, [sp], #16
217		ret
218	
219	// length of .L._println_str0
220		.word 0
221	.L._println_str0:
222		.asciz ""
223	.align 4
224	_println:
225		// push {lr}
226		stp lr, xzr, [sp, #-16]!
227		adr x0, .L._println_str0
228		bl puts
229		mov x0, #0
230		bl fflush
231		// pop {lr}
232		ldp lr, xzr, [sp], #16
233		ret
234	
235	// length of .L._errOverflow_str0
236		.word 52
237	.L._errOverflow_str0:
238		.asciz "fatal error: integer overflow or underflow occurred\n"
239	.align 4
240	_errOverflow:
241		adr x0, .L._errOverflow_str0
242		bl _prints
243		mov w0, #-1
244		bl exit
===========================================================
-- Finished

