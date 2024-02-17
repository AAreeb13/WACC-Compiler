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
1	@ length of .L.str0
2		.word 2
3	.L.str0:
4		.asciz ", "
5	@ length of .L.str1
6		.word 35
7	.L.str1:
8		.asciz "The first 20 fibonacci numbers are:"
9	@ length of .L.str2
10		.word 3
11	.L.str2:
12		.asciz "0, "
13	@ length of .L.str3
14		.word 3
15	.L.str3:
16		.asciz "..."
17	.align 4
18	.text
19	.global main
20	main:
21		push {fp, lr}
22		push {r4, r8, r10, r12}
23		mov fp, sp
24		@ Stack pointer unchanged, no stack allocated variables
25		@ Stack pointer unchanged, no stack allocated arguments
26		ldr r8, =.L.str1
27		push {r8}
28		pop {r8}
29		mov r8, r8
30		mov r0, r8
31		@ statement primitives do not return results (but will clobber r0/rax)
32		bl _prints
33		bl _println
34		@ Stack pointer unchanged, no stack allocated arguments
35		ldr r8, =.L.str2
36		push {r8}
37		pop {r8}
38		mov r8, r8
39		mov r0, r8
40		@ statement primitives do not return results (but will clobber r0/rax)
41		bl _prints
42		@ Stack pointer unchanged, no stack allocated arguments
43		mov r8, #19
44		mov r0, r8
45		mov r8, #1
46		mov r1, r8
47		bl wacc_fibonacci
48		mov r12, r0
49		@ Stack pointer unchanged, no stack allocated arguments
50		mov r8, r12
51		mov r4, r8
52		@ Stack pointer unchanged, no stack allocated arguments
53		mov r8, r4
54		mov r0, r8
55		@ statement primitives do not return results (but will clobber r0/rax)
56		bl _printi
57		@ Stack pointer unchanged, no stack allocated arguments
58		ldr r8, =.L.str3
59		push {r8}
60		pop {r8}
61		mov r8, r8
62		mov r0, r8
63		@ statement primitives do not return results (but will clobber r0/rax)
64		bl _prints
65		bl _println
66		@ Stack pointer unchanged, no stack allocated variables
67		mov r0, #0
68		pop {r4, r8, r10, r12}
69		pop {fp, pc}
70	
71	wacc_fibonacci:
72		push {fp, lr}
73		push {r4, r5}
74		mov fp, sp
75		@ Stack pointer unchanged, no stack allocated variables
76		cmp r0, #1
77		ble .L0
78		b .L1
79	.L0:
80		mov r0, r0
81		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
82		mov sp, fp
83		pop {r4, r5}
84		pop {fp, pc}
85	.L1:
86		push {r0, r1}
87		@ Set up R12 as a temporary second base pointer for the caller saved things
88		mov r12, sp
89		@ Stack pointer unchanged, no stack allocated arguments
90		subs r8, r0, #1
91		blvs _errOverflow
92		push {r8}
93		pop {r8}
94		mov r8, r8
95		mov r0, r8
96		mov r8, r1
97		mov r1, r8
98		bl wacc_fibonacci
99		mov r12, r0
100		@ Stack pointer unchanged, no stack allocated arguments
101		pop {r0, r1}
102		mov r8, r12
103		mov r4, r8
104		cmp r1, #1
105		beq .L2
106		b .L3
107	.L2:
108		push {r0, r1}
109		@ Set up R12 as a temporary second base pointer for the caller saved things
110		mov r12, sp
111		@ Stack pointer unchanged, no stack allocated arguments
112		mov r8, r4
113		mov r0, r8
114		@ statement primitives do not return results (but will clobber r0/rax)
115		bl _printi
116		pop {r0, r1}
117		push {r0, r1}
118		@ Set up R12 as a temporary second base pointer for the caller saved things
119		mov r12, sp
120		@ Stack pointer unchanged, no stack allocated arguments
121		ldr r8, =.L.str0
122		push {r8}
123		pop {r8}
124		mov r8, r8
125		mov r0, r8
126		@ statement primitives do not return results (but will clobber r0/rax)
127		bl _prints
128		pop {r0, r1}
129	.L3:
130		push {r0, r1}
131		@ Set up R12 as a temporary second base pointer for the caller saved things
132		mov r12, sp
133		@ Stack pointer unchanged, no stack allocated arguments
134		subs r8, r0, #2
135		blvs _errOverflow
136		push {r8}
137		pop {r8}
138		mov r8, r8
139		mov r0, r8
140		mov r8, #0
141		mov r1, r8
142		bl wacc_fibonacci
143		mov r12, r0
144		@ Stack pointer unchanged, no stack allocated arguments
145		pop {r0, r1}
146		mov r8, r12
147		mov r5, r8
148		adds r8, r4, r5
149		blvs _errOverflow
150		push {r8}
151		pop {r0}
152		mov r0, r0
153		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
154		mov sp, fp
155		pop {r4, r5}
156		pop {fp, pc}
157		@ Stack pointer unchanged, no stack allocated variables
158		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
159		@ the .ltorg here is used to generate the constant pool after every function
160		@ this is important for ensuring string literals and large integers are accessible!
161		.ltorg
162	
163	@ length of .L._prints_str0
164		.word 4
165	.L._prints_str0:
166		.asciz "%.*s"
167	.align 4
168	_prints:
169		push {fp, lr}
170		mov fp, sp
171		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
172		bic sp, sp, #0x7
173		mov r2, r0
174		ldr r1, [r0, #-4]
175		adr r0, .L._prints_str0
176		bl printf
177		mov r0, #0
178		bl fflush
179		mov sp, fp
180		pop {fp, pc}
181	
182	@ length of .L._printi_str0
183		.word 2
184	.L._printi_str0:
185		.asciz "%d"
186	.align 4
187	_printi:
188		push {fp, lr}
189		mov fp, sp
190		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
191		bic sp, sp, #0x7
192		mov r1, r0
193		adr r0, .L._printi_str0
194		bl printf
195		mov r0, #0
196		bl fflush
197		mov sp, fp
198		pop {fp, pc}
199	
200	@ length of .L._println_str0
201		.word 0
202	.L._println_str0:
203		.asciz ""
204	.align 4
205	_println:
206		push {fp, lr}
207		mov fp, sp
208		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
209		bic sp, sp, #0x7
210		adr r0, .L._println_str0
211		bl puts
212		mov r0, #0
213		bl fflush
214		mov sp, fp
215		pop {fp, pc}
216	
217	_exit:
218		push {fp, lr}
219		mov fp, sp
220		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
221		bic sp, sp, #0x7
222		bl exit
223		mov sp, fp
224		pop {fp, pc}
225	
226	@ length of .L._errOverflow_str0
227		.word 52
228	.L._errOverflow_str0:
229		.asciz "fatal error: integer overflow or underflow occurred\n"
230	.align 4
231	_errOverflow:
232		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
233		bic sp, sp, #0x7
234		adr r0, .L._errOverflow_str0
235		bl _prints
236		mov r0, #255
237		bl _exit
===========================================================
-- Finished

