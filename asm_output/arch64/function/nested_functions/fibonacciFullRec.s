./valid/function/nested_functions/fibonacciFullRec.wacc
calling the reference compiler on ./valid/function/nested_functions/fibonacciFullRec.wacc
-- Test: fibonacciFullRec.wacc

-- Uploaded file: 
---------------------------------------------------------------
# recursively calculate the nth fibonacci number

# Input: 30

# Output:
# This program calculates the nth fibonacci number recursively.
# Please enter n (should not be too large): The input n is 30
# The nth fibonacci number is 832040
#

# Program:

begin
  int fibonacci(int n) is
    if n <= 1
    then
      return n
    else
      skip
    fi ;
    int f1 = call fibonacci(n - 1) ;
    int f2 = call fibonacci(n - 2) ;
    return f1 + f2
  end

  println "This program calculates the nth fibonacci number recursively." ;
  print "Please enter n (should not be too large): " ;
  int n = 0;
  read n ;
  print "The input n is " ;
  println n ;
  print "The nth fibonacci number is " ;
  int result = call fibonacci(n) ;
  println  result
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
fibonacciFullRec.s contents are:
===========================================================
0	.data
1	// length of .L.str0
2		.word 61
3	.L.str0:
4		.asciz "This program calculates the nth fibonacci number recursively."
5	// length of .L.str1
6		.word 42
7	.L.str1:
8		.asciz "Please enter n (should not be too large): "
9	// length of .L.str2
10		.word 15
11	.L.str2:
12		.asciz "The input n is "
13	// length of .L.str3
14		.word 28
15	.L.str3:
16		.asciz "The nth fibonacci number is "
17	.align 4
18	.text
19	.global main
20	main:
21		// push {fp, lr}
22		stp fp, lr, [sp, #-16]!
23		// push {x19, x20}
24		stp x19, x20, [sp, #-16]!
25		mov fp, sp
26		// Stack pointer unchanged, no stack allocated variables
27		// Stack pointer unchanged, no stack allocated arguments
28		adrp x8, .L.str0
29		add x8, x8, :lo12:.L.str0
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
40		adrp x8, .L.str1
41		add x8, x8, :lo12:.L.str1
42		// push {x8}
43		stp x8, xzr, [sp, #-16]!
44		// pop {x8}
45		ldp x8, xzr, [sp], #16
46		mov x8, x8
47		mov x0, x8
48		// statement primitives do not return results (but will clobber r0/rax)
49		bl _prints
50		mov x8, #0
51		mov x19, x8
52		// Stack pointer unchanged, no stack allocated arguments
53		// load the current value in the destination of the read so it supports defaults
54		mov x8, x19
55		mov x0, x8
56		bl _readi
57		mov x16, x0
58		mov x8, x16
59		mov x19, x8
60		// Stack pointer unchanged, no stack allocated arguments
61		adrp x8, .L.str2
62		add x8, x8, :lo12:.L.str2
63		// push {x8}
64		stp x8, xzr, [sp, #-16]!
65		// pop {x8}
66		ldp x8, xzr, [sp], #16
67		mov x8, x8
68		mov x0, x8
69		// statement primitives do not return results (but will clobber r0/rax)
70		bl _prints
71		// Stack pointer unchanged, no stack allocated arguments
72		mov x8, x19
73		mov x0, x8
74		// statement primitives do not return results (but will clobber r0/rax)
75		bl _printi
76		bl _println
77		// Stack pointer unchanged, no stack allocated arguments
78		adrp x8, .L.str3
79		add x8, x8, :lo12:.L.str3
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
91		bl wacc_fibonacci
92		mov x16, x0
93		// Stack pointer unchanged, no stack allocated arguments
94		mov x8, x16
95		mov x20, x8
96		// Stack pointer unchanged, no stack allocated arguments
97		mov x8, x20
98		mov x0, x8
99		// statement primitives do not return results (but will clobber r0/rax)
100		bl _printi
101		bl _println
102		// Stack pointer unchanged, no stack allocated variables
103		mov x0, #0
104		// pop {x19, x20}
105		ldp x19, x20, [sp], #16
106		// pop {fp, lr}
107		ldp fp, lr, [sp], #16
108		ret
109	
110	wacc_fibonacci:
111		// push {fp, lr}
112		stp fp, lr, [sp, #-16]!
113		// push {x19, x20}
114		stp x19, x20, [sp, #-16]!
115		mov fp, sp
116		// Stack pointer unchanged, no stack allocated variables
117		cmp x0, #1
118		b.le .L0
119		b .L1
120	.L0:
121		mov x0, x0
122		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
123		mov sp, fp
124		// pop {x19, x20}
125		ldp x19, x20, [sp], #16
126		// pop {fp, lr}
127		ldp fp, lr, [sp], #16
128		ret
129	.L1:
130		// push {x0}
131		stp x0, xzr, [sp, #-16]!
132		// Set up X16 as a temporary second base pointer for the caller saved things
133		mov x16, sp
134		// Stack pointer unchanged, no stack allocated arguments
135		subs w8, w0, #1
136		b.vs _errOverflow
137		sxtw x8, w8
138		// push {x8}
139		stp x8, xzr, [sp, #-16]!
140		// pop {x8}
141		ldp x8, xzr, [sp], #16
142		mov x8, x8
143		mov x0, x8
144		bl wacc_fibonacci
145		mov x16, x0
146		// Stack pointer unchanged, no stack allocated arguments
147		// pop {x0}
148		ldp x0, xzr, [sp], #16
149		mov x8, x16
150		mov x19, x8
151		// push {x0}
152		stp x0, xzr, [sp, #-16]!
153		// Set up X16 as a temporary second base pointer for the caller saved things
154		mov x16, sp
155		// Stack pointer unchanged, no stack allocated arguments
156		subs w8, w0, #2
157		b.vs _errOverflow
158		sxtw x8, w8
159		// push {x8}
160		stp x8, xzr, [sp, #-16]!
161		// pop {x8}
162		ldp x8, xzr, [sp], #16
163		mov x8, x8
164		mov x0, x8
165		bl wacc_fibonacci
166		mov x16, x0
167		// Stack pointer unchanged, no stack allocated arguments
168		// pop {x0}
169		ldp x0, xzr, [sp], #16
170		mov x8, x16
171		mov x20, x8
172		adds w8, w19, w20
173		b.vs _errOverflow
174		sxtw x8, w8
175		// push {x8}
176		stp x8, xzr, [sp, #-16]!
177		// pop {x0}
178		ldp x0, xzr, [sp], #16
179		mov x0, x0
180		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
181		mov sp, fp
182		// pop {x19, x20}
183		ldp x19, x20, [sp], #16
184		// pop {fp, lr}
185		ldp fp, lr, [sp], #16
186		ret
187		// Stack pointer unchanged, no stack allocated variables
188		// 'ere be dragons: this is 100% dead code, functions always end in returns!
189	
190	// length of .L._prints_str0
191		.word 4
192	.L._prints_str0:
193		.asciz "%.*s"
194	.align 4
195	_prints:
196		// push {lr}
197		stp lr, xzr, [sp, #-16]!
198		mov x2, x0
199		ldrsw x1, [x0, #-4]
200		adr x0, .L._prints_str0
201		bl printf
202		mov x0, #0
203		bl fflush
204		// pop {lr}
205		ldp lr, xzr, [sp], #16
206		ret
207	
208	// length of .L._readi_str0
209		.word 2
210	.L._readi_str0:
211		.asciz "%d"
212	.align 4
213	_readi:
214		// X0 contains the "original" value of the destination of the read
215		// allocate space on the stack to store the read: preserve alignment!
216		// the passed default argument should be stored in case of EOF
217		// aarch64 mandates 16-byte SP alignment at all times, might as well merge the stores
218		// push {x0, lr}
219		stp x0, lr, [sp, #-16]!
220		mov x1, sp
221		adr x0, .L._readi_str0
222		bl scanf
223		// pop {x0, lr}
224		ldp x0, lr, [sp], #16
225		ret
226	
227	// length of .L._printi_str0
228		.word 2
229	.L._printi_str0:
230		.asciz "%d"
231	.align 4
232	_printi:
233		// push {lr}
234		stp lr, xzr, [sp, #-16]!
235		mov x1, x0
236		adr x0, .L._printi_str0
237		bl printf
238		mov x0, #0
239		bl fflush
240		// pop {lr}
241		ldp lr, xzr, [sp], #16
242		ret
243	
244	// length of .L._println_str0
245		.word 0
246	.L._println_str0:
247		.asciz ""
248	.align 4
249	_println:
250		// push {lr}
251		stp lr, xzr, [sp, #-16]!
252		adr x0, .L._println_str0
253		bl puts
254		mov x0, #0
255		bl fflush
256		// pop {lr}
257		ldp lr, xzr, [sp], #16
258		ret
259	
260	// length of .L._errOverflow_str0
261		.word 52
262	.L._errOverflow_str0:
263		.asciz "fatal error: integer overflow or underflow occurred\n"
264	.align 4
265	_errOverflow:
266		adr x0, .L._errOverflow_str0
267		bl _prints
268		mov w0, #-1
269		bl exit
===========================================================
-- Finished

