./valid/function/nested_functions/mutualRecursion.wacc
calling the reference compiler on ./valid/function/nested_functions/mutualRecursion.wacc
-- Test: mutualRecursion.wacc

-- Uploaded file: 
---------------------------------------------------------------
# a pair of mutually recursive functions

# Output:
# r1: sending 8
# r2: received 8
# r1: sending 7
# r2: received 7
# r1: sending 6
# r2: received 6
# r1: sending 5
# r2: received 5
# r1: sending 4
# r2: received 4
# r1: sending 3
# r2: received 3
# r1: sending 2
# r2: received 2
# r1: sending 1
# r2: received 1
#

# Program:

begin
  int r1(int x) is
    if x == 0
    then
      skip
    else
      print "r1: sending " ;
      println x ;
      int y = call r2(x)
    fi ;
    return 42
  end

  int r2(int y) is
    print "r2: received " ;
    println y ;
    int z = call r1(y - 1) ;
    return 44
  end

  int x = 0 ;
  x = call r1(8)
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
mutualRecursion.s contents are:
===========================================================
0	.data
1	// length of .L.str0
2		.word 12
3	.L.str0:
4		.asciz "r1: sending "
5	// length of .L.str1
6		.word 13
7	.L.str1:
8		.asciz "r2: received "
9	.align 4
10	.text
11	.global main
12	main:
13		// push {fp, lr}
14		stp fp, lr, [sp, #-16]!
15		// push {x19}
16		stp x19, xzr, [sp, #-16]!
17		mov fp, sp
18		// Stack pointer unchanged, no stack allocated variables
19		mov x8, #0
20		mov x19, x8
21		// Stack pointer unchanged, no stack allocated arguments
22		mov x8, #8
23		mov x0, x8
24		bl wacc_r1
25		mov x16, x0
26		// Stack pointer unchanged, no stack allocated arguments
27		mov x8, x16
28		mov x19, x8
29		// Stack pointer unchanged, no stack allocated variables
30		mov x0, #0
31		// pop {x19}
32		ldp x19, xzr, [sp], #16
33		// pop {fp, lr}
34		ldp fp, lr, [sp], #16
35		ret
36	
37	wacc_r1:
38		// push {fp, lr}
39		stp fp, lr, [sp, #-16]!
40		// push {x19}
41		stp x19, xzr, [sp, #-16]!
42		mov fp, sp
43		cmp x0, #0
44		b.eq .L0
45		// Stack pointer unchanged, no stack allocated variables
46		// push {x0}
47		stp x0, xzr, [sp, #-16]!
48		// Set up X16 as a temporary second base pointer for the caller saved things
49		mov x16, sp
50		// Stack pointer unchanged, no stack allocated arguments
51		adrp x8, .L.str0
52		add x8, x8, :lo12:.L.str0
53		// push {x8}
54		stp x8, xzr, [sp, #-16]!
55		// pop {x8}
56		ldp x8, xzr, [sp], #16
57		mov x8, x8
58		mov x0, x8
59		// statement primitives do not return results (but will clobber r0/rax)
60		bl _prints
61		// pop {x0}
62		ldp x0, xzr, [sp], #16
63		// push {x0}
64		stp x0, xzr, [sp, #-16]!
65		// Set up X16 as a temporary second base pointer for the caller saved things
66		mov x16, sp
67		// Stack pointer unchanged, no stack allocated arguments
68		mov x8, x0
69		mov x0, x8
70		// statement primitives do not return results (but will clobber r0/rax)
71		bl _printi
72		bl _println
73		// pop {x0}
74		ldp x0, xzr, [sp], #16
75		// push {x0}
76		stp x0, xzr, [sp, #-16]!
77		// Set up X16 as a temporary second base pointer for the caller saved things
78		mov x16, sp
79		// Stack pointer unchanged, no stack allocated arguments
80		mov x8, x0
81		mov x0, x8
82		bl wacc_r2
83		mov x16, x0
84		// Stack pointer unchanged, no stack allocated arguments
85		// pop {x0}
86		ldp x0, xzr, [sp], #16
87		mov x8, x16
88		mov x19, x8
89		// Stack pointer unchanged, no stack allocated variables
90		b .L1
91	.L0:
92	.L1:
93		mov x0, #42
94		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
95		mov sp, fp
96		// pop {x19}
97		ldp x19, xzr, [sp], #16
98		// pop {fp, lr}
99		ldp fp, lr, [sp], #16
100		ret
101		// 'ere be dragons: this is 100% dead code, functions always end in returns!
102	
103	wacc_r2:
104		// push {fp, lr}
105		stp fp, lr, [sp, #-16]!
106		// push {x19}
107		stp x19, xzr, [sp, #-16]!
108		mov fp, sp
109		// Stack pointer unchanged, no stack allocated variables
110		// push {x0}
111		stp x0, xzr, [sp, #-16]!
112		// Set up X16 as a temporary second base pointer for the caller saved things
113		mov x16, sp
114		// Stack pointer unchanged, no stack allocated arguments
115		adrp x8, .L.str1
116		add x8, x8, :lo12:.L.str1
117		// push {x8}
118		stp x8, xzr, [sp, #-16]!
119		// pop {x8}
120		ldp x8, xzr, [sp], #16
121		mov x8, x8
122		mov x0, x8
123		// statement primitives do not return results (but will clobber r0/rax)
124		bl _prints
125		// pop {x0}
126		ldp x0, xzr, [sp], #16
127		// push {x0}
128		stp x0, xzr, [sp, #-16]!
129		// Set up X16 as a temporary second base pointer for the caller saved things
130		mov x16, sp
131		// Stack pointer unchanged, no stack allocated arguments
132		mov x8, x0
133		mov x0, x8
134		// statement primitives do not return results (but will clobber r0/rax)
135		bl _printi
136		bl _println
137		// pop {x0}
138		ldp x0, xzr, [sp], #16
139		// push {x0}
140		stp x0, xzr, [sp, #-16]!
141		// Set up X16 as a temporary second base pointer for the caller saved things
142		mov x16, sp
143		// Stack pointer unchanged, no stack allocated arguments
144		subs w8, w0, #1
145		b.vs _errOverflow
146		sxtw x8, w8
147		// push {x8}
148		stp x8, xzr, [sp, #-16]!
149		// pop {x8}
150		ldp x8, xzr, [sp], #16
151		mov x8, x8
152		mov x0, x8
153		bl wacc_r1
154		mov x16, x0
155		// Stack pointer unchanged, no stack allocated arguments
156		// pop {x0}
157		ldp x0, xzr, [sp], #16
158		mov x8, x16
159		mov x19, x8
160		mov x0, #44
161		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
162		mov sp, fp
163		// pop {x19}
164		ldp x19, xzr, [sp], #16
165		// pop {fp, lr}
166		ldp fp, lr, [sp], #16
167		ret
168		// Stack pointer unchanged, no stack allocated variables
169		// 'ere be dragons: this is 100% dead code, functions always end in returns!
170	
171	// length of .L._prints_str0
172		.word 4
173	.L._prints_str0:
174		.asciz "%.*s"
175	.align 4
176	_prints:
177		// push {lr}
178		stp lr, xzr, [sp, #-16]!
179		mov x2, x0
180		ldrsw x1, [x0, #-4]
181		adr x0, .L._prints_str0
182		bl printf
183		mov x0, #0
184		bl fflush
185		// pop {lr}
186		ldp lr, xzr, [sp], #16
187		ret
188	
189	// length of .L._printi_str0
190		.word 2
191	.L._printi_str0:
192		.asciz "%d"
193	.align 4
194	_printi:
195		// push {lr}
196		stp lr, xzr, [sp, #-16]!
197		mov x1, x0
198		adr x0, .L._printi_str0
199		bl printf
200		mov x0, #0
201		bl fflush
202		// pop {lr}
203		ldp lr, xzr, [sp], #16
204		ret
205	
206	// length of .L._println_str0
207		.word 0
208	.L._println_str0:
209		.asciz ""
210	.align 4
211	_println:
212		// push {lr}
213		stp lr, xzr, [sp, #-16]!
214		adr x0, .L._println_str0
215		bl puts
216		mov x0, #0
217		bl fflush
218		// pop {lr}
219		ldp lr, xzr, [sp], #16
220		ret
221	
222	// length of .L._errOverflow_str0
223		.word 52
224	.L._errOverflow_str0:
225		.asciz "fatal error: integer overflow or underflow occurred\n"
226	.align 4
227	_errOverflow:
228		adr x0, .L._errOverflow_str0
229		bl _prints
230		mov w0, #-1
231		bl exit
===========================================================
-- Finished

