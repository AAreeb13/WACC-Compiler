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
1	@ length of .L.str0
2		.word 24
3	.L.str0:
4		.asciz "Enter the first number: "
5	@ length of .L.str1
6		.word 25
7	.L.str1:
8		.asciz "Enter the second number: "
9	@ length of .L.str2
10		.word 20
11	.L.str2:
12		.asciz "Initial value of x: "
13	@ length of .L.str3
14		.word 3
15	.L.str3:
16		.asciz "(+)"
17	@ length of .L.str4
18		.word 0
19	.L.str4:
20		.asciz ""
21	@ length of .L.str5
22		.word 18
23	.L.str5:
24		.asciz "final value of x: "
25	.align 4
26	.text
27	.global main
28	main:
29		push {fp, lr}
30		push {r4, r5, r8, r10, r12}
31		mov fp, sp
32		@ Stack pointer unchanged, no stack allocated variables
33		mov r8, #0
34		mov r4, r8
35		mov r8, #0
36		mov r5, r8
37		@ Stack pointer unchanged, no stack allocated arguments
38		ldr r8, =.L.str0
39		push {r8}
40		pop {r8}
41		mov r8, r8
42		mov r0, r8
43		@ statement primitives do not return results (but will clobber r0/rax)
44		bl _prints
45		@ Stack pointer unchanged, no stack allocated arguments
46		@ load the current value in the destination of the read so it supports defaults
47		mov r8, r4
48		mov r0, r8
49		bl _readi
50		mov r12, r0
51		mov r8, r12
52		mov r4, r8
53		@ Stack pointer unchanged, no stack allocated arguments
54		ldr r8, =.L.str1
55		push {r8}
56		pop {r8}
57		mov r8, r8
58		mov r0, r8
59		@ statement primitives do not return results (but will clobber r0/rax)
60		bl _prints
61		@ Stack pointer unchanged, no stack allocated arguments
62		@ load the current value in the destination of the read so it supports defaults
63		mov r8, r5
64		mov r0, r8
65		bl _readi
66		mov r12, r0
67		mov r8, r12
68		mov r5, r8
69		@ Stack pointer unchanged, no stack allocated arguments
70		ldr r8, =.L.str2
71		push {r8}
72		pop {r8}
73		mov r8, r8
74		mov r0, r8
75		@ statement primitives do not return results (but will clobber r0/rax)
76		bl _prints
77		@ Stack pointer unchanged, no stack allocated arguments
78		mov r8, r4
79		mov r0, r8
80		@ statement primitives do not return results (but will clobber r0/rax)
81		bl _printi
82		bl _println
83		b .L0
84	.L1:
85		@ Stack pointer unchanged, no stack allocated arguments
86		ldr r8, =.L.str3
87		push {r8}
88		pop {r8}
89		mov r8, r8
90		mov r0, r8
91		@ statement primitives do not return results (but will clobber r0/rax)
92		bl _prints
93		adds r8, r4, #1
94		blvs _errOverflow
95		push {r8}
96		pop {r8}
97		mov r8, r8
98		mov r4, r8
99		subs r8, r5, #1
100		blvs _errOverflow
101		push {r8}
102		pop {r8}
103		mov r8, r8
104		mov r5, r8
105	.L0:
106		cmp r5, #0
107		bgt .L1
108		@ Stack pointer unchanged, no stack allocated arguments
109		ldr r8, =.L.str4
110		push {r8}
111		pop {r8}
112		mov r8, r8
113		mov r0, r8
114		@ statement primitives do not return results (but will clobber r0/rax)
115		bl _prints
116		bl _println
117		@ Stack pointer unchanged, no stack allocated arguments
118		ldr r8, =.L.str5
119		push {r8}
120		pop {r8}
121		mov r8, r8
122		mov r0, r8
123		@ statement primitives do not return results (but will clobber r0/rax)
124		bl _prints
125		@ Stack pointer unchanged, no stack allocated arguments
126		mov r8, r4
127		mov r0, r8
128		@ statement primitives do not return results (but will clobber r0/rax)
129		bl _printi
130		bl _println
131		@ Stack pointer unchanged, no stack allocated variables
132		mov r0, #0
133		pop {r4, r5, r8, r10, r12}
134		pop {fp, pc}
135	
136	@ length of .L._prints_str0
137		.word 4
138	.L._prints_str0:
139		.asciz "%.*s"
140	.align 4
141	_prints:
142		push {fp, lr}
143		mov fp, sp
144		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
145		bic sp, sp, #0x7
146		mov r2, r0
147		ldr r1, [r0, #-4]
148		adr r0, .L._prints_str0
149		bl printf
150		mov r0, #0
151		bl fflush
152		mov sp, fp
153		pop {fp, pc}
154	
155	@ length of .L._readi_str0
156		.word 2
157	.L._readi_str0:
158		.asciz "%d"
159	.align 4
160	_readi:
161		push {fp, lr}
162		mov fp, sp
163		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
164		bic sp, sp, #0x7
165		@ R0 contains the "original" value of the destination of the read
166		@ allocate space on the stack to store the read: preserve alignment!
167		@ the passed default argument should be stored in case of EOF
168		sub sp, sp, #8
169		str r0, [sp, #0]
170		mov r1, sp
171		adr r0, .L._readi_str0
172		bl scanf
173		ldr r0, [sp, #0]
174		add sp, sp, #8
175		mov sp, fp
176		pop {fp, pc}
177	
178	@ length of .L._printi_str0
179		.word 2
180	.L._printi_str0:
181		.asciz "%d"
182	.align 4
183	_printi:
184		push {fp, lr}
185		mov fp, sp
186		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
187		bic sp, sp, #0x7
188		mov r1, r0
189		adr r0, .L._printi_str0
190		bl printf
191		mov r0, #0
192		bl fflush
193		mov sp, fp
194		pop {fp, pc}
195	
196	@ length of .L._println_str0
197		.word 0
198	.L._println_str0:
199		.asciz ""
200	.align 4
201	_println:
202		push {fp, lr}
203		mov fp, sp
204		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
205		bic sp, sp, #0x7
206		adr r0, .L._println_str0
207		bl puts
208		mov r0, #0
209		bl fflush
210		mov sp, fp
211		pop {fp, pc}
212	
213	_exit:
214		push {fp, lr}
215		mov fp, sp
216		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
217		bic sp, sp, #0x7
218		bl exit
219		mov sp, fp
220		pop {fp, pc}
221	
222	@ length of .L._errOverflow_str0
223		.word 52
224	.L._errOverflow_str0:
225		.asciz "fatal error: integer overflow or underflow occurred\n"
226	.align 4
227	_errOverflow:
228		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
229		bic sp, sp, #0x7
230		adr r0, .L._errOverflow_str0
231		bl _prints
232		mov r0, #255
233		bl _exit
===========================================================
-- Finished

