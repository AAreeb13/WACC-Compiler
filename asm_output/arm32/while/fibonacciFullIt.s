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
1	@ length of .L.str0
2		.word 61
3	.L.str0:
4		.asciz "This program calculates the nth fibonacci number iteratively."
5	@ length of .L.str1
6		.word 42
7	.L.str1:
8		.asciz "Please enter n (should not be too large): "
9	@ length of .L.str2
10		.word 15
11	.L.str2:
12		.asciz "The input n is "
13	@ length of .L.str3
14		.word 28
15	.L.str3:
16		.asciz "The nth fibonacci number is "
17	.align 4
18	.text
19	.global main
20	main:
21		push {fp, lr}
22		push {r4, r5, r6, r7, r8, r10, r12}
23		mov fp, sp
24		@ Stack pointer unchanged, no stack allocated variables
25		@ Stack pointer unchanged, no stack allocated arguments
26		ldr r8, =.L.str0
27		push {r8}
28		pop {r8}
29		mov r8, r8
30		mov r0, r8
31		@ statement primitives do not return results (but will clobber r0/rax)
32		bl _prints
33		bl _println
34		@ Stack pointer unchanged, no stack allocated arguments
35		ldr r8, =.L.str1
36		push {r8}
37		pop {r8}
38		mov r8, r8
39		mov r0, r8
40		@ statement primitives do not return results (but will clobber r0/rax)
41		bl _prints
42		mov r8, #0
43		mov r4, r8
44		@ Stack pointer unchanged, no stack allocated arguments
45		@ load the current value in the destination of the read so it supports defaults
46		mov r8, r4
47		mov r0, r8
48		bl _readi
49		mov r12, r0
50		mov r8, r12
51		mov r4, r8
52		@ Stack pointer unchanged, no stack allocated arguments
53		ldr r8, =.L.str2
54		push {r8}
55		pop {r8}
56		mov r8, r8
57		mov r0, r8
58		@ statement primitives do not return results (but will clobber r0/rax)
59		bl _prints
60		@ Stack pointer unchanged, no stack allocated arguments
61		mov r8, r4
62		mov r0, r8
63		@ statement primitives do not return results (but will clobber r0/rax)
64		bl _printi
65		bl _println
66		@ Stack pointer unchanged, no stack allocated arguments
67		ldr r8, =.L.str3
68		push {r8}
69		pop {r8}
70		mov r8, r8
71		mov r0, r8
72		@ statement primitives do not return results (but will clobber r0/rax)
73		bl _prints
74		mov r8, #0
75		mov r5, r8
76		mov r8, #1
77		mov r6, r8
78		mov r8, #0
79		mov r7, r8
80		b .L0
81	.L1:
82		mov r8, r5
83		mov r7, r8
84		mov r8, r6
85		mov r5, r8
86		adds r8, r7, r6
87		blvs _errOverflow
88		push {r8}
89		pop {r8}
90		mov r8, r8
91		mov r6, r8
92		subs r8, r4, #1
93		blvs _errOverflow
94		push {r8}
95		pop {r8}
96		mov r8, r8
97		mov r4, r8
98	.L0:
99		cmp r4, #0
100		bgt .L1
101		@ Stack pointer unchanged, no stack allocated arguments
102		mov r8, r5
103		mov r0, r8
104		@ statement primitives do not return results (but will clobber r0/rax)
105		bl _printi
106		bl _println
107		@ Stack pointer unchanged, no stack allocated variables
108		mov r0, #0
109		pop {r4, r5, r6, r7, r8, r10, r12}
110		pop {fp, pc}
111	
112	@ length of .L._prints_str0
113		.word 4
114	.L._prints_str0:
115		.asciz "%.*s"
116	.align 4
117	_prints:
118		push {fp, lr}
119		mov fp, sp
120		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
121		bic sp, sp, #0x7
122		mov r2, r0
123		ldr r1, [r0, #-4]
124		adr r0, .L._prints_str0
125		bl printf
126		mov r0, #0
127		bl fflush
128		mov sp, fp
129		pop {fp, pc}
130	
131	@ length of .L._readi_str0
132		.word 2
133	.L._readi_str0:
134		.asciz "%d"
135	.align 4
136	_readi:
137		push {fp, lr}
138		mov fp, sp
139		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
140		bic sp, sp, #0x7
141		@ R0 contains the "original" value of the destination of the read
142		@ allocate space on the stack to store the read: preserve alignment!
143		@ the passed default argument should be stored in case of EOF
144		sub sp, sp, #8
145		str r0, [sp, #0]
146		mov r1, sp
147		adr r0, .L._readi_str0
148		bl scanf
149		ldr r0, [sp, #0]
150		add sp, sp, #8
151		mov sp, fp
152		pop {fp, pc}
153	
154	@ length of .L._printi_str0
155		.word 2
156	.L._printi_str0:
157		.asciz "%d"
158	.align 4
159	_printi:
160		push {fp, lr}
161		mov fp, sp
162		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
163		bic sp, sp, #0x7
164		mov r1, r0
165		adr r0, .L._printi_str0
166		bl printf
167		mov r0, #0
168		bl fflush
169		mov sp, fp
170		pop {fp, pc}
171	
172	@ length of .L._println_str0
173		.word 0
174	.L._println_str0:
175		.asciz ""
176	.align 4
177	_println:
178		push {fp, lr}
179		mov fp, sp
180		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
181		bic sp, sp, #0x7
182		adr r0, .L._println_str0
183		bl puts
184		mov r0, #0
185		bl fflush
186		mov sp, fp
187		pop {fp, pc}
188	
189	_exit:
190		push {fp, lr}
191		mov fp, sp
192		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
193		bic sp, sp, #0x7
194		bl exit
195		mov sp, fp
196		pop {fp, pc}
197	
198	@ length of .L._errOverflow_str0
199		.word 52
200	.L._errOverflow_str0:
201		.asciz "fatal error: integer overflow or underflow occurred\n"
202	.align 4
203	_errOverflow:
204		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
205		bic sp, sp, #0x7
206		adr r0, .L._errOverflow_str0
207		bl _prints
208		mov r0, #255
209		bl _exit
===========================================================
-- Finished

