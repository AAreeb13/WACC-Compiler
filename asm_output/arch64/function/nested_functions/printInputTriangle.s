./valid/function/nested_functions/printInputTriangle.wacc
calling the reference compiler on ./valid/function/nested_functions/printInputTriangle.wacc
-- Test: printInputTriangle.wacc

-- Uploaded file: 
---------------------------------------------------------------
# print a user-specified sized triangle

# Input: 13

# Output:
# Please enter the size of the triangle to print:
# -------------
# ------------
# -----------
# ----------
# ---------
# --------
# -------
# ------
# -----
# ----
# ---
# --
# -
#

# Program:

begin
  int f(int x) is
    if x == 0 then
      skip
    else
      int i = x ;
      while i > 0 do
        print "-" ;
        i = i - 1
      done ;
      println "" ;
      int s = call f(x - 1)
    fi ;
    return 0
  end

  println "Please enter the size of the triangle to print:" ;
  int x = 0;

  read x;
  int s = call f(x)
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
printInputTriangle.s contents are:
===========================================================
0	.data
1	// length of .L.str0
2		.word 1
3	.L.str0:
4		.asciz "-"
5	// length of .L.str1
6		.word 0
7	.L.str1:
8		.asciz ""
9	// length of .L.str2
10		.word 47
11	.L.str2:
12		.asciz "Please enter the size of the triangle to print:"
13	.align 4
14	.text
15	.global main
16	main:
17		// push {fp, lr}
18		stp fp, lr, [sp, #-16]!
19		// push {x19, x20}
20		stp x19, x20, [sp, #-16]!
21		mov fp, sp
22		// Stack pointer unchanged, no stack allocated variables
23		// Stack pointer unchanged, no stack allocated arguments
24		adrp x8, .L.str2
25		add x8, x8, :lo12:.L.str2
26		// push {x8}
27		stp x8, xzr, [sp, #-16]!
28		// pop {x8}
29		ldp x8, xzr, [sp], #16
30		mov x8, x8
31		mov x0, x8
32		// statement primitives do not return results (but will clobber r0/rax)
33		bl _prints
34		bl _println
35		mov x8, #0
36		mov x19, x8
37		// Stack pointer unchanged, no stack allocated arguments
38		// load the current value in the destination of the read so it supports defaults
39		mov x8, x19
40		mov x0, x8
41		bl _readi
42		mov x16, x0
43		mov x8, x16
44		mov x19, x8
45		// Stack pointer unchanged, no stack allocated arguments
46		mov x8, x19
47		mov x0, x8
48		bl wacc_f
49		mov x16, x0
50		// Stack pointer unchanged, no stack allocated arguments
51		mov x8, x16
52		mov x20, x8
53		// Stack pointer unchanged, no stack allocated variables
54		mov x0, #0
55		// pop {x19, x20}
56		ldp x19, x20, [sp], #16
57		// pop {fp, lr}
58		ldp fp, lr, [sp], #16
59		ret
60	
61	wacc_f:
62		// push {fp, lr}
63		stp fp, lr, [sp, #-16]!
64		// push {x19, x20}
65		stp x19, x20, [sp, #-16]!
66		mov fp, sp
67		cmp x0, #0
68		b.eq .L0
69		// Stack pointer unchanged, no stack allocated variables
70		mov x8, x0
71		mov x19, x8
72		b .L2
73	.L3:
74		// push {x0}
75		stp x0, xzr, [sp, #-16]!
76		// Set up X16 as a temporary second base pointer for the caller saved things
77		mov x16, sp
78		// Stack pointer unchanged, no stack allocated arguments
79		adrp x8, .L.str0
80		add x8, x8, :lo12:.L.str0
81		// push {x8}
82		stp x8, xzr, [sp, #-16]!
83		// pop {x8}
84		ldp x8, xzr, [sp], #16
85		mov x8, x8
86		mov x0, x8
87		// statement primitives do not return results (but will clobber r0/rax)
88		bl _prints
89		// pop {x0}
90		ldp x0, xzr, [sp], #16
91		subs w8, w19, #1
92		b.vs _errOverflow
93		sxtw x8, w8
94		// push {x8}
95		stp x8, xzr, [sp, #-16]!
96		// pop {x8}
97		ldp x8, xzr, [sp], #16
98		mov x8, x8
99		mov x19, x8
100	.L2:
101		cmp x19, #0
102		b.gt .L3
103		// push {x0}
104		stp x0, xzr, [sp, #-16]!
105		// Set up X16 as a temporary second base pointer for the caller saved things
106		mov x16, sp
107		// Stack pointer unchanged, no stack allocated arguments
108		adrp x8, .L.str1
109		add x8, x8, :lo12:.L.str1
110		// push {x8}
111		stp x8, xzr, [sp, #-16]!
112		// pop {x8}
113		ldp x8, xzr, [sp], #16
114		mov x8, x8
115		mov x0, x8
116		// statement primitives do not return results (but will clobber r0/rax)
117		bl _prints
118		bl _println
119		// pop {x0}
120		ldp x0, xzr, [sp], #16
121		// push {x0}
122		stp x0, xzr, [sp, #-16]!
123		// Set up X16 as a temporary second base pointer for the caller saved things
124		mov x16, sp
125		// Stack pointer unchanged, no stack allocated arguments
126		subs w8, w0, #1
127		b.vs _errOverflow
128		sxtw x8, w8
129		// push {x8}
130		stp x8, xzr, [sp, #-16]!
131		// pop {x8}
132		ldp x8, xzr, [sp], #16
133		mov x8, x8
134		mov x0, x8
135		bl wacc_f
136		mov x16, x0
137		// Stack pointer unchanged, no stack allocated arguments
138		// pop {x0}
139		ldp x0, xzr, [sp], #16
140		mov x8, x16
141		mov x20, x8
142		// Stack pointer unchanged, no stack allocated variables
143		b .L1
144	.L0:
145	.L1:
146		mov x0, #0
147		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
148		mov sp, fp
149		// pop {x19, x20}
150		ldp x19, x20, [sp], #16
151		// pop {fp, lr}
152		ldp fp, lr, [sp], #16
153		ret
154		// 'ere be dragons: this is 100% dead code, functions always end in returns!
155	
156	// length of .L._prints_str0
157		.word 4
158	.L._prints_str0:
159		.asciz "%.*s"
160	.align 4
161	_prints:
162		// push {lr}
163		stp lr, xzr, [sp, #-16]!
164		mov x2, x0
165		ldrsw x1, [x0, #-4]
166		adr x0, .L._prints_str0
167		bl printf
168		mov x0, #0
169		bl fflush
170		// pop {lr}
171		ldp lr, xzr, [sp], #16
172		ret
173	
174	// length of .L._readi_str0
175		.word 2
176	.L._readi_str0:
177		.asciz "%d"
178	.align 4
179	_readi:
180		// X0 contains the "original" value of the destination of the read
181		// allocate space on the stack to store the read: preserve alignment!
182		// the passed default argument should be stored in case of EOF
183		// aarch64 mandates 16-byte SP alignment at all times, might as well merge the stores
184		// push {x0, lr}
185		stp x0, lr, [sp, #-16]!
186		mov x1, sp
187		adr x0, .L._readi_str0
188		bl scanf
189		// pop {x0, lr}
190		ldp x0, lr, [sp], #16
191		ret
192	
193	// length of .L._println_str0
194		.word 0
195	.L._println_str0:
196		.asciz ""
197	.align 4
198	_println:
199		// push {lr}
200		stp lr, xzr, [sp, #-16]!
201		adr x0, .L._println_str0
202		bl puts
203		mov x0, #0
204		bl fflush
205		// pop {lr}
206		ldp lr, xzr, [sp], #16
207		ret
208	
209	// length of .L._errOverflow_str0
210		.word 52
211	.L._errOverflow_str0:
212		.asciz "fatal error: integer overflow or underflow occurred\n"
213	.align 4
214	_errOverflow:
215		adr x0, .L._errOverflow_str0
216		bl _prints
217		mov w0, #-1
218		bl exit
===========================================================
-- Finished

