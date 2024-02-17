./valid/IO/IOLoop.wacc
calling the reference compiler on ./valid/IO/IOLoop.wacc
-- Test: IOLoop.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple input/output loop

# Input: 1 Y 2 Y 3 Y 4 Y 5 Y 142 N

# Output:
# Please input an integer: echo input: 1
# Do you want to continue entering input?
# (enter Y for 'yes' and N for 'no')
# Please input an integer: echo input: 2
# Do you want to continue entering input?
# (enter Y for 'yes' and N for 'no')
# Please input an integer: echo input: 3
# Do you want to continue entering input?
# (enter Y for 'yes' and N for 'no')
# Please input an integer: echo input: 4
# Do you want to continue entering input?
# (enter Y for 'yes' and N for 'no')
# Please input an integer: echo input: 5
# Do you want to continue entering input?
# (enter Y for 'yes' and N for 'no')
# Please input an integer: echo input: 142
# Do you want to continue entering input?
# (enter Y for 'yes' and N for 'no')
#

# Program:

begin
  char continue = 'Y' ;
  int buff = 0 ;
  while continue != 'N' do
    print "Please input an integer: " ;
    read buff ;
    print "echo input: " ;
    println buff ;
    println "Do you want to continue entering input?" ;
    println "(enter Y for \'yes\' and N for \'no\')" ;
    read continue
  done
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
IOLoop.s contents are:
===========================================================
0	.data
1	// length of .L.str0
2		.word 25
3	.L.str0:
4		.asciz "Please input an integer: "
5	// length of .L.str1
6		.word 12
7	.L.str1:
8		.asciz "echo input: "
9	// length of .L.str2
10		.word 39
11	.L.str2:
12		.asciz "Do you want to continue entering input?"
13	// length of .L.str3
14		.word 34
15	.L.str3:
16		.asciz "(enter Y for 'yes' and N for 'no')"
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
27		mov x8, #89
28		mov x19, x8
29		mov x8, #0
30		mov x20, x8
31		b .L0
32	.L1:
33		// Stack pointer unchanged, no stack allocated arguments
34		adrp x8, .L.str0
35		add x8, x8, :lo12:.L.str0
36		// push {x8}
37		stp x8, xzr, [sp, #-16]!
38		// pop {x8}
39		ldp x8, xzr, [sp], #16
40		mov x8, x8
41		mov x0, x8
42		// statement primitives do not return results (but will clobber r0/rax)
43		bl _prints
44		// Stack pointer unchanged, no stack allocated arguments
45		// load the current value in the destination of the read so it supports defaults
46		mov x8, x20
47		mov x0, x8
48		bl _readi
49		mov x16, x0
50		mov x8, x16
51		mov x20, x8
52		// Stack pointer unchanged, no stack allocated arguments
53		adrp x8, .L.str1
54		add x8, x8, :lo12:.L.str1
55		// push {x8}
56		stp x8, xzr, [sp, #-16]!
57		// pop {x8}
58		ldp x8, xzr, [sp], #16
59		mov x8, x8
60		mov x0, x8
61		// statement primitives do not return results (but will clobber r0/rax)
62		bl _prints
63		// Stack pointer unchanged, no stack allocated arguments
64		mov x8, x20
65		mov x0, x8
66		// statement primitives do not return results (but will clobber r0/rax)
67		bl _printi
68		bl _println
69		// Stack pointer unchanged, no stack allocated arguments
70		adrp x8, .L.str2
71		add x8, x8, :lo12:.L.str2
72		// push {x8}
73		stp x8, xzr, [sp, #-16]!
74		// pop {x8}
75		ldp x8, xzr, [sp], #16
76		mov x8, x8
77		mov x0, x8
78		// statement primitives do not return results (but will clobber r0/rax)
79		bl _prints
80		bl _println
81		// Stack pointer unchanged, no stack allocated arguments
82		adrp x8, .L.str3
83		add x8, x8, :lo12:.L.str3
84		// push {x8}
85		stp x8, xzr, [sp, #-16]!
86		// pop {x8}
87		ldp x8, xzr, [sp], #16
88		mov x8, x8
89		mov x0, x8
90		// statement primitives do not return results (but will clobber r0/rax)
91		bl _prints
92		bl _println
93		// Stack pointer unchanged, no stack allocated arguments
94		// load the current value in the destination of the read so it supports defaults
95		mov x8, x19
96		mov x0, x8
97		bl _readc
98		mov x16, x0
99		mov x8, x16
100		mov x19, x8
101	.L0:
102		cmp x19, #78
103		b.ne .L1
104		// Stack pointer unchanged, no stack allocated variables
105		mov x0, #0
106		// pop {x19, x20}
107		ldp x19, x20, [sp], #16
108		// pop {fp, lr}
109		ldp fp, lr, [sp], #16
110		ret
111	
112	// length of .L._readc_str0
113		.word 3
114	.L._readc_str0:
115		.asciz " %c"
116	.align 4
117	_readc:
118		// X0 contains the "original" value of the destination of the read
119		// allocate space on the stack to store the read: preserve alignment!
120		// the passed default argument should be stored in case of EOF
121		// aarch64 mandates 16-byte SP alignment at all times, might as well merge the stores
122		// push {x0, lr}
123		stp x0, lr, [sp, #-16]!
124		mov x1, sp
125		adr x0, .L._readc_str0
126		bl scanf
127		// pop {x0, lr}
128		ldp x0, lr, [sp], #16
129		ret
130	
131	// length of .L._prints_str0
132		.word 4
133	.L._prints_str0:
134		.asciz "%.*s"
135	.align 4
136	_prints:
137		// push {lr}
138		stp lr, xzr, [sp, #-16]!
139		mov x2, x0
140		ldrsw x1, [x0, #-4]
141		adr x0, .L._prints_str0
142		bl printf
143		mov x0, #0
144		bl fflush
145		// pop {lr}
146		ldp lr, xzr, [sp], #16
147		ret
148	
149	// length of .L._readi_str0
150		.word 2
151	.L._readi_str0:
152		.asciz "%d"
153	.align 4
154	_readi:
155		// X0 contains the "original" value of the destination of the read
156		// allocate space on the stack to store the read: preserve alignment!
157		// the passed default argument should be stored in case of EOF
158		// aarch64 mandates 16-byte SP alignment at all times, might as well merge the stores
159		// push {x0, lr}
160		stp x0, lr, [sp, #-16]!
161		mov x1, sp
162		adr x0, .L._readi_str0
163		bl scanf
164		// pop {x0, lr}
165		ldp x0, lr, [sp], #16
166		ret
167	
168	// length of .L._printi_str0
169		.word 2
170	.L._printi_str0:
171		.asciz "%d"
172	.align 4
173	_printi:
174		// push {lr}
175		stp lr, xzr, [sp, #-16]!
176		mov x1, x0
177		adr x0, .L._printi_str0
178		bl printf
179		mov x0, #0
180		bl fflush
181		// pop {lr}
182		ldp lr, xzr, [sp], #16
183		ret
184	
185	// length of .L._println_str0
186		.word 0
187	.L._println_str0:
188		.asciz ""
189	.align 4
190	_println:
191		// push {lr}
192		stp lr, xzr, [sp, #-16]!
193		adr x0, .L._println_str0
194		bl puts
195		mov x0, #0
196		bl fflush
197		// pop {lr}
198		ldp lr, xzr, [sp], #16
199		ret
===========================================================
-- Finished

