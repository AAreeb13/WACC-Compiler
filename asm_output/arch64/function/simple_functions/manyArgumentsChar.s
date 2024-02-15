./valid/function/simple_functions/manyArgumentsChar.wacc
calling the reference compiler on ./valid/function/simple_functions/manyArgumentsChar.wacc
-- Test: manyArgumentsChar.wacc

-- Uploaded file: 
---------------------------------------------------------------
# tests a function with more arguments than fit into registers

# Output:
# A
# b
#

# Program:

begin
  # w, x, y and z are stack allocated on aarch32
  # y and z are stack allocated on x86-64
  char f(int s, int t, int u, int v, int w, int x, char y, bool z) is
    int i = u + v ;
    int j = w + x ;
    if z
        then return chr (ord y - i * j)
        else return y
    fi
  end

  char r1 = call f(0, 0, 3, 5, 1, 3, 'a', true) ;
  println r1 ;

  char r2 = call f(0, 0, 3, 5, 1, 3, 'b', false) ;
  println r2
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
manyArgumentsChar.s contents are:
===========================================================
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		// push {fp, lr}
6		stp fp, lr, [sp, #-16]!
7		// push {x19, x20}
8		stp x19, x20, [sp, #-16]!
9		mov fp, sp
10		// Stack pointer unchanged, no stack allocated variables
11		// Stack pointer unchanged, no stack allocated arguments
12		mov x8, #0
13		mov x0, x8
14		mov x8, #0
15		mov x1, x8
16		mov x8, #3
17		mov x2, x8
18		mov x8, #5
19		mov x3, x8
20		mov x8, #1
21		mov x4, x8
22		mov x8, #3
23		mov x5, x8
24		mov x8, #97
25		mov x6, x8
26		mov x8, #1
27		mov x7, x8
28		bl wacc_f
29		mov x16, x0
30		// Stack pointer unchanged, no stack allocated arguments
31		mov x8, x16
32		mov x19, x8
33		// Stack pointer unchanged, no stack allocated arguments
34		mov x8, x19
35		mov x0, x8
36		// statement primitives do not return results (but will clobber r0/rax)
37		bl _printc
38		bl _println
39		// Stack pointer unchanged, no stack allocated arguments
40		mov x8, #0
41		mov x0, x8
42		mov x8, #0
43		mov x1, x8
44		mov x8, #3
45		mov x2, x8
46		mov x8, #5
47		mov x3, x8
48		mov x8, #1
49		mov x4, x8
50		mov x8, #3
51		mov x5, x8
52		mov x8, #98
53		mov x6, x8
54		mov x8, #0
55		mov x7, x8
56		bl wacc_f
57		mov x16, x0
58		// Stack pointer unchanged, no stack allocated arguments
59		mov x8, x16
60		mov x20, x8
61		// Stack pointer unchanged, no stack allocated arguments
62		mov x8, x20
63		mov x0, x8
64		// statement primitives do not return results (but will clobber r0/rax)
65		bl _printc
66		bl _println
67		// Stack pointer unchanged, no stack allocated variables
68		mov x0, #0
69		// pop {x19, x20}
70		ldp x19, x20, [sp], #16
71		// pop {fp, lr}
72		ldp fp, lr, [sp], #16
73		ret
74	
75	wacc_f:
76		// push {fp, lr}
77		stp fp, lr, [sp, #-16]!
78		// push {x19, x20}
79		stp x19, x20, [sp, #-16]!
80		mov fp, sp
81		// Stack pointer unchanged, no stack allocated variables
82		adds w8, w2, w3
83		b.vs _errOverflow
84		sxtw x8, w8
85		// push {x8}
86		stp x8, xzr, [sp, #-16]!
87		// pop {x8}
88		ldp x8, xzr, [sp], #16
89		mov x8, x8
90		mov x19, x8
91		adds w8, w4, w5
92		b.vs _errOverflow
93		sxtw x8, w8
94		// push {x8}
95		stp x8, xzr, [sp, #-16]!
96		// pop {x8}
97		ldp x8, xzr, [sp], #16
98		mov x8, x8
99		mov x20, x8
100		cmp x7, #1
101		b.eq .L0
102		mov x0, x6
103		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
104		mov sp, fp
105		// pop {x19, x20}
106		ldp x19, x20, [sp], #16
107		// pop {fp, lr}
108		ldp fp, lr, [sp], #16
109		ret
110		b .L1
111	.L0:
112		smull x8, w19, w20
113		// take the 31st bit, sign extend it to 64 bits
114		sbfx x9, x8, #31, #1
115		// now take the top 32 bits of the result, shift and sign extend to 64 bits
116		cmp x9, x8, asr #32
117		// if they are not equal then overflow occured
118		b.ne _errOverflow
119		sxtw x8, w8
120		// push {x8}
121		stp x8, xzr, [sp, #-16]!
122		// pop {x9}
123		ldp x9, xzr, [sp], #16
124		subs w8, w6, w9
125		b.vs _errOverflow
126		sxtw x8, w8
127		// push {x8}
128		stp x8, xzr, [sp, #-16]!
129		// pop {x8}
130		ldp x8, xzr, [sp], #16
131		mov x8, x8
132		tst x8, #0xffffffffffffff80
133		csel x1, x8, x1, ne
134		b.ne _errBadChar
135		// push {x8}
136		stp x8, xzr, [sp, #-16]!
137		// pop {x0}
138		ldp x0, xzr, [sp], #16
139		mov x0, x0
140		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
141		mov sp, fp
142		// pop {x19, x20}
143		ldp x19, x20, [sp], #16
144		// pop {fp, lr}
145		ldp fp, lr, [sp], #16
146		ret
147	.L1:
148		// Stack pointer unchanged, no stack allocated variables
149		// 'ere be dragons: this is 100% dead code, functions always end in returns!
150	
151	// length of .L._prints_str0
152		.word 4
153	.L._prints_str0:
154		.asciz "%.*s"
155	.align 4
156	_prints:
157		// push {lr}
158		stp lr, xzr, [sp, #-16]!
159		mov x2, x0
160		ldrsw x1, [x0, #-4]
161		adr x0, .L._prints_str0
162		bl printf
163		mov x0, #0
164		bl fflush
165		// pop {lr}
166		ldp lr, xzr, [sp], #16
167		ret
168	
169	// length of .L._printc_str0
170		.word 2
171	.L._printc_str0:
172		.asciz "%c"
173	.align 4
174	_printc:
175		// push {lr}
176		stp lr, xzr, [sp, #-16]!
177		mov x1, x0
178		adr x0, .L._printc_str0
179		bl printf
180		mov x0, #0
181		bl fflush
182		// pop {lr}
183		ldp lr, xzr, [sp], #16
184		ret
185	
186	// length of .L._println_str0
187		.word 0
188	.L._println_str0:
189		.asciz ""
190	.align 4
191	_println:
192		// push {lr}
193		stp lr, xzr, [sp, #-16]!
194		adr x0, .L._println_str0
195		bl puts
196		mov x0, #0
197		bl fflush
198		// pop {lr}
199		ldp lr, xzr, [sp], #16
200		ret
201	
202	// length of .L._errBadChar_str0
203		.word 50
204	.L._errBadChar_str0:
205		.asciz "fatal error: int %d is not ascii character 0-127 \n"
206	.align 4
207	_errBadChar:
208		adr x0, .L._errBadChar_str0
209		bl printf
210		mov x0, #0
211		bl fflush
212		mov w0, #-1
213		bl exit
214	
215	// length of .L._errOverflow_str0
216		.word 52
217	.L._errOverflow_str0:
218		.asciz "fatal error: integer overflow or underflow occurred\n"
219	.align 4
220	_errOverflow:
221		adr x0, .L._errOverflow_str0
222		bl _prints
223		mov w0, #-1
224		bl exit
===========================================================
-- Finished

