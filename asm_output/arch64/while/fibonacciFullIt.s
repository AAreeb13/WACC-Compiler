./valid/while/fibonacciFullIt.wacc
calling the reference compiler on ./valid/while/fibonacciFullIt.wacc
-- Test: fibonacciFullIt.wacc

-- Uploaded file: 
---------------------------------------------------------------
# iteratively calculate the given fibonacci number

# Input: 30

# Output:
# This program calculates the nth fibonacci number iteratively.
# Please enter n (should not be too large): The input n is 30
# The nth fibonacci number is 832040
#

# Program:

begin
  println "This program calculates the nth fibonacci number iteratively." ;
  print "Please enter n (should not be too large): " ;
  int n = 0;
  read n ;
  print "The input n is " ;
  println n ;
  print "The nth fibonacci number is " ;
  int f0 = 0 ;
  int f1 = 1 ;
  int save = 0;
  while n > 0 do
    save = f0 ;
    f0 = f1 ;
    f1 = save + f1 ;
    n = n - 1
  done ;
  println f0
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
fibonacciFullIt.s contents are:
===========================================================
0	.data
1	// length of .L.str0
2		.word 61
3	.L.str0:
4		.asciz "This program calculates the nth fibonacci number iteratively."
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
23		// push {x19, x20, x21, x22}
24		stp x19, x20, [sp, #-32]!
25		stp x21, x22, [sp, #16]
26		mov fp, sp
27		// Stack pointer unchanged, no stack allocated variables
28		// Stack pointer unchanged, no stack allocated arguments
29		adrp x8, .L.str0
30		add x8, x8, :lo12:.L.str0
31		// push {x8}
32		stp x8, xzr, [sp, #-16]!
33		// pop {x8}
34		ldp x8, xzr, [sp], #16
35		mov x8, x8
36		mov x0, x8
37		// statement primitives do not return results (but will clobber r0/rax)
38		bl _prints
39		bl _println
40		// Stack pointer unchanged, no stack allocated arguments
41		adrp x8, .L.str1
42		add x8, x8, :lo12:.L.str1
43		// push {x8}
44		stp x8, xzr, [sp, #-16]!
45		// pop {x8}
46		ldp x8, xzr, [sp], #16
47		mov x8, x8
48		mov x0, x8
49		// statement primitives do not return results (but will clobber r0/rax)
50		bl _prints
51		mov x8, #0
52		mov x19, x8
53		// Stack pointer unchanged, no stack allocated arguments
54		// load the current value in the destination of the read so it supports defaults
55		mov x8, x19
56		mov x0, x8
57		bl _readi
58		mov x16, x0
59		mov x8, x16
60		mov x19, x8
61		// Stack pointer unchanged, no stack allocated arguments
62		adrp x8, .L.str2
63		add x8, x8, :lo12:.L.str2
64		// push {x8}
65		stp x8, xzr, [sp, #-16]!
66		// pop {x8}
67		ldp x8, xzr, [sp], #16
68		mov x8, x8
69		mov x0, x8
70		// statement primitives do not return results (but will clobber r0/rax)
71		bl _prints
72		// Stack pointer unchanged, no stack allocated arguments
73		mov x8, x19
74		mov x0, x8
75		// statement primitives do not return results (but will clobber r0/rax)
76		bl _printi
77		bl _println
78		// Stack pointer unchanged, no stack allocated arguments
79		adrp x8, .L.str3
80		add x8, x8, :lo12:.L.str3
81		// push {x8}
82		stp x8, xzr, [sp, #-16]!
83		// pop {x8}
84		ldp x8, xzr, [sp], #16
85		mov x8, x8
86		mov x0, x8
87		// statement primitives do not return results (but will clobber r0/rax)
88		bl _prints
89		mov x8, #0
90		mov x20, x8
91		mov x8, #1
92		mov x21, x8
93		mov x8, #0
94		mov x22, x8
95		b .L0
96	.L1:
97		mov x8, x20
98		mov x22, x8
99		mov x8, x21
100		mov x20, x8
101		adds w8, w22, w21
102		b.vs _errOverflow
103		sxtw x8, w8
104		// push {x8}
105		stp x8, xzr, [sp, #-16]!
106		// pop {x8}
107		ldp x8, xzr, [sp], #16
108		mov x8, x8
109		mov x21, x8
110		subs w8, w19, #1
111		b.vs _errOverflow
112		sxtw x8, w8
113		// push {x8}
114		stp x8, xzr, [sp, #-16]!
115		// pop {x8}
116		ldp x8, xzr, [sp], #16
117		mov x8, x8
118		mov x19, x8
119	.L0:
120		cmp x19, #0
121		b.gt .L1
122		// Stack pointer unchanged, no stack allocated arguments
123		mov x8, x20
124		mov x0, x8
125		// statement primitives do not return results (but will clobber r0/rax)
126		bl _printi
127		bl _println
128		// Stack pointer unchanged, no stack allocated variables
129		mov x0, #0
130		// pop {x19, x20, x21, x22}
131		ldp x21, x22, [sp, #16]
132		ldp x19, x20, [sp], #32
133		// pop {fp, lr}
134		ldp fp, lr, [sp], #16
135		ret
136	
137	// length of .L._prints_str0
138		.word 4
139	.L._prints_str0:
140		.asciz "%.*s"
141	.align 4
142	_prints:
143		// push {lr}
144		stp lr, xzr, [sp, #-16]!
145		mov x2, x0
146		ldrsw x1, [x0, #-4]
147		adr x0, .L._prints_str0
148		bl printf
149		mov x0, #0
150		bl fflush
151		// pop {lr}
152		ldp lr, xzr, [sp], #16
153		ret
154	
155	// length of .L._readi_str0
156		.word 2
157	.L._readi_str0:
158		.asciz "%d"
159	.align 4
160	_readi:
161		// X0 contains the "original" value of the destination of the read
162		// allocate space on the stack to store the read: preserve alignment!
163		// the passed default argument should be stored in case of EOF
164		// aarch64 mandates 16-byte SP alignment at all times, might as well merge the stores
165		// push {x0, lr}
166		stp x0, lr, [sp, #-16]!
167		mov x1, sp
168		adr x0, .L._readi_str0
169		bl scanf
170		// pop {x0, lr}
171		ldp x0, lr, [sp], #16
172		ret
173	
174	// length of .L._printi_str0
175		.word 2
176	.L._printi_str0:
177		.asciz "%d"
178	.align 4
179	_printi:
180		// push {lr}
181		stp lr, xzr, [sp, #-16]!
182		mov x1, x0
183		adr x0, .L._printi_str0
184		bl printf
185		mov x0, #0
186		bl fflush
187		// pop {lr}
188		ldp lr, xzr, [sp], #16
189		ret
190	
191	// length of .L._println_str0
192		.word 0
193	.L._println_str0:
194		.asciz ""
195	.align 4
196	_println:
197		// push {lr}
198		stp lr, xzr, [sp, #-16]!
199		adr x0, .L._println_str0
200		bl puts
201		mov x0, #0
202		bl fflush
203		// pop {lr}
204		ldp lr, xzr, [sp], #16
205		ret
206	
207	// length of .L._errOverflow_str0
208		.word 52
209	.L._errOverflow_str0:
210		.asciz "fatal error: integer overflow or underflow occurred\n"
211	.align 4
212	_errOverflow:
213		adr x0, .L._errOverflow_str0
214		bl _prints
215		mov w0, #-1
216		bl exit
===========================================================
-- Finished

