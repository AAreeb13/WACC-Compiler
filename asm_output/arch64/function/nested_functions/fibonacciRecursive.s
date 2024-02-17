./valid/function/nested_functions/fibonacciRecursive.wacc
calling the reference compiler on ./valid/function/nested_functions/fibonacciRecursive.wacc
-- Test: fibonacciRecursive.wacc

-- Uploaded file: 
---------------------------------------------------------------
# recursive calculation of the first 20 fibonacci numbers

# Output:
# The first 20 fibonacci numbers are:
# 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181...
#

# Program:

begin
  int fibonacci(int n, bool toPrint) is
    if n <= 1
    then
      return n
    else
      skip
    fi ;
    int f1 = call fibonacci(n - 1, toPrint) ;
    if toPrint
    then
      print f1 ;
      print ", "
    else
      skip
    fi ;
    int f2 = call fibonacci(n - 2, false) ;
    return f1 + f2
  end

  println "The first 20 fibonacci numbers are:" ;
  print "0, " ;
  int result = call fibonacci(19, true) ;
  print result ;
  println "..."
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
fibonacciRecursive.s contents are:
===========================================================
0	.data
1	// length of .L.str0
2		.word 2
3	.L.str0:
4		.asciz ", "
5	// length of .L.str1
6		.word 35
7	.L.str1:
8		.asciz "The first 20 fibonacci numbers are:"
9	// length of .L.str2
10		.word 3
11	.L.str2:
12		.asciz "0, "
13	// length of .L.str3
14		.word 3
15	.L.str3:
16		.asciz "..."
17	.align 4
18	.text
19	.global main
20	main:
21		// push {fp, lr}
22		stp fp, lr, [sp, #-16]!
23		// push {x19}
24		stp x19, xzr, [sp, #-16]!
25		mov fp, sp
26		// Stack pointer unchanged, no stack allocated variables
27		// Stack pointer unchanged, no stack allocated arguments
28		adrp x8, .L.str1
29		add x8, x8, :lo12:.L.str1
30		// push {x8}
31		stp x8, xzr, [sp, #-16]!
32		// pop {x8}
33		ldp x8, xzr, [sp], #16
34		mov x8, x8
35		mov x0, x8
36		// statement primitives do not return results (but will clobber r0/rax)
37		bl _prints
38		bl _println
39		// Stack pointer unchanged, no stack allocated arguments
40		adrp x8, .L.str2
41		add x8, x8, :lo12:.L.str2
42		// push {x8}
43		stp x8, xzr, [sp, #-16]!
44		// pop {x8}
45		ldp x8, xzr, [sp], #16
46		mov x8, x8
47		mov x0, x8
48		// statement primitives do not return results (but will clobber r0/rax)
49		bl _prints
50		// Stack pointer unchanged, no stack allocated arguments
51		mov x8, #19
52		mov x0, x8
53		mov x8, #1
54		mov x1, x8
55		bl wacc_fibonacci
56		mov x16, x0
57		// Stack pointer unchanged, no stack allocated arguments
58		mov x8, x16
59		mov x19, x8
60		// Stack pointer unchanged, no stack allocated arguments
61		mov x8, x19
62		mov x0, x8
63		// statement primitives do not return results (but will clobber r0/rax)
64		bl _printi
65		// Stack pointer unchanged, no stack allocated arguments
66		adrp x8, .L.str3
67		add x8, x8, :lo12:.L.str3
68		// push {x8}
69		stp x8, xzr, [sp, #-16]!
70		// pop {x8}
71		ldp x8, xzr, [sp], #16
72		mov x8, x8
73		mov x0, x8
74		// statement primitives do not return results (but will clobber r0/rax)
75		bl _prints
76		bl _println
77		// Stack pointer unchanged, no stack allocated variables
78		mov x0, #0
79		// pop {x19}
80		ldp x19, xzr, [sp], #16
81		// pop {fp, lr}
82		ldp fp, lr, [sp], #16
83		ret
84	
85	wacc_fibonacci:
86		// push {fp, lr}
87		stp fp, lr, [sp, #-16]!
88		// push {x19, x20}
89		stp x19, x20, [sp, #-16]!
90		mov fp, sp
91		// Stack pointer unchanged, no stack allocated variables
92		cmp x0, #1
93		b.le .L0
94		b .L1
95	.L0:
96		mov x0, x0
97		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
98		mov sp, fp
99		// pop {x19, x20}
100		ldp x19, x20, [sp], #16
101		// pop {fp, lr}
102		ldp fp, lr, [sp], #16
103		ret
104	.L1:
105		// push {x0, x1}
106		stp x0, x1, [sp, #-16]!
107		// Set up X16 as a temporary second base pointer for the caller saved things
108		mov x16, sp
109		// Stack pointer unchanged, no stack allocated arguments
110		subs w8, w0, #1
111		b.vs _errOverflow
112		sxtw x8, w8
113		// push {x8}
114		stp x8, xzr, [sp, #-16]!
115		// pop {x8}
116		ldp x8, xzr, [sp], #16
117		mov x8, x8
118		mov x0, x8
119		mov x8, x1
120		mov x1, x8
121		bl wacc_fibonacci
122		mov x16, x0
123		// Stack pointer unchanged, no stack allocated arguments
124		// pop {x0, x1}
125		ldp x0, x1, [sp], #16
126		mov x8, x16
127		mov x19, x8
128		cmp x1, #1
129		b.eq .L2
130		b .L3
131	.L2:
132		// push {x0, x1}
133		stp x0, x1, [sp, #-16]!
134		// Set up X16 as a temporary second base pointer for the caller saved things
135		mov x16, sp
136		// Stack pointer unchanged, no stack allocated arguments
137		mov x8, x19
138		mov x0, x8
139		// statement primitives do not return results (but will clobber r0/rax)
140		bl _printi
141		// pop {x0, x1}
142		ldp x0, x1, [sp], #16
143		// push {x0, x1}
144		stp x0, x1, [sp, #-16]!
145		// Set up X16 as a temporary second base pointer for the caller saved things
146		mov x16, sp
147		// Stack pointer unchanged, no stack allocated arguments
148		adrp x8, .L.str0
149		add x8, x8, :lo12:.L.str0
150		// push {x8}
151		stp x8, xzr, [sp, #-16]!
152		// pop {x8}
153		ldp x8, xzr, [sp], #16
154		mov x8, x8
155		mov x0, x8
156		// statement primitives do not return results (but will clobber r0/rax)
157		bl _prints
158		// pop {x0, x1}
159		ldp x0, x1, [sp], #16
160	.L3:
161		// push {x0, x1}
162		stp x0, x1, [sp, #-16]!
163		// Set up X16 as a temporary second base pointer for the caller saved things
164		mov x16, sp
165		// Stack pointer unchanged, no stack allocated arguments
166		subs w8, w0, #2
167		b.vs _errOverflow
168		sxtw x8, w8
169		// push {x8}
170		stp x8, xzr, [sp, #-16]!
171		// pop {x8}
172		ldp x8, xzr, [sp], #16
173		mov x8, x8
174		mov x0, x8
175		mov x8, #0
176		mov x1, x8
177		bl wacc_fibonacci
178		mov x16, x0
179		// Stack pointer unchanged, no stack allocated arguments
180		// pop {x0, x1}
181		ldp x0, x1, [sp], #16
182		mov x8, x16
183		mov x20, x8
184		adds w8, w19, w20
185		b.vs _errOverflow
186		sxtw x8, w8
187		// push {x8}
188		stp x8, xzr, [sp, #-16]!
189		// pop {x0}
190		ldp x0, xzr, [sp], #16
191		mov x0, x0
192		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
193		mov sp, fp
194		// pop {x19, x20}
195		ldp x19, x20, [sp], #16
196		// pop {fp, lr}
197		ldp fp, lr, [sp], #16
198		ret
199		// Stack pointer unchanged, no stack allocated variables
200		// 'ere be dragons: this is 100% dead code, functions always end in returns!
201	
202	// length of .L._prints_str0
203		.word 4
204	.L._prints_str0:
205		.asciz "%.*s"
206	.align 4
207	_prints:
208		// push {lr}
209		stp lr, xzr, [sp, #-16]!
210		mov x2, x0
211		ldrsw x1, [x0, #-4]
212		adr x0, .L._prints_str0
213		bl printf
214		mov x0, #0
215		bl fflush
216		// pop {lr}
217		ldp lr, xzr, [sp], #16
218		ret
219	
220	// length of .L._printi_str0
221		.word 2
222	.L._printi_str0:
223		.asciz "%d"
224	.align 4
225	_printi:
226		// push {lr}
227		stp lr, xzr, [sp, #-16]!
228		mov x1, x0
229		adr x0, .L._printi_str0
230		bl printf
231		mov x0, #0
232		bl fflush
233		// pop {lr}
234		ldp lr, xzr, [sp], #16
235		ret
236	
237	// length of .L._println_str0
238		.word 0
239	.L._println_str0:
240		.asciz ""
241	.align 4
242	_println:
243		// push {lr}
244		stp lr, xzr, [sp, #-16]!
245		adr x0, .L._println_str0
246		bl puts
247		mov x0, #0
248		bl fflush
249		// pop {lr}
250		ldp lr, xzr, [sp], #16
251		ret
252	
253	// length of .L._errOverflow_str0
254		.word 52
255	.L._errOverflow_str0:
256		.asciz "fatal error: integer overflow or underflow occurred\n"
257	.align 4
258	_errOverflow:
259		adr x0, .L._errOverflow_str0
260		bl _prints
261		mov w0, #-1
262		bl exit
===========================================================
-- Finished

