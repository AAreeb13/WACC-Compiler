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
1	@ length of .L.str0
2		.word 61
3	.L.str0:
4		.asciz "This program calculates the nth fibonacci number recursively."
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
22		push {r4, r5, r8, r10, r12}
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
74		@ Stack pointer unchanged, no stack allocated arguments
75		mov r8, r4
76		mov r0, r8
77		bl wacc_fibonacci
78		mov r12, r0
79		@ Stack pointer unchanged, no stack allocated arguments
80		mov r8, r12
81		mov r5, r8
82		@ Stack pointer unchanged, no stack allocated arguments
83		mov r8, r5
84		mov r0, r8
85		@ statement primitives do not return results (but will clobber r0/rax)
86		bl _printi
87		bl _println
88		@ Stack pointer unchanged, no stack allocated variables
89		mov r0, #0
90		pop {r4, r5, r8, r10, r12}
91		pop {fp, pc}
92	
93	wacc_fibonacci:
94		push {fp, lr}
95		push {r4, r5}
96		mov fp, sp
97		@ Stack pointer unchanged, no stack allocated variables
98		cmp r0, #1
99		ble .L0
100		b .L1
101	.L0:
102		mov r0, r0
103		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
104		mov sp, fp
105		pop {r4, r5}
106		pop {fp, pc}
107	.L1:
108		push {r0}
109		@ Set up R12 as a temporary second base pointer for the caller saved things
110		mov r12, sp
111		@ Stack pointer unchanged, no stack allocated arguments
112		subs r8, r0, #1
113		blvs _errOverflow
114		push {r8}
115		pop {r8}
116		mov r8, r8
117		mov r0, r8
118		bl wacc_fibonacci
119		mov r12, r0
120		@ Stack pointer unchanged, no stack allocated arguments
121		pop {r0}
122		mov r8, r12
123		mov r4, r8
124		push {r0}
125		@ Set up R12 as a temporary second base pointer for the caller saved things
126		mov r12, sp
127		@ Stack pointer unchanged, no stack allocated arguments
128		subs r8, r0, #2
129		blvs _errOverflow
130		push {r8}
131		pop {r8}
132		mov r8, r8
133		mov r0, r8
134		bl wacc_fibonacci
135		mov r12, r0
136		@ Stack pointer unchanged, no stack allocated arguments
137		pop {r0}
138		mov r8, r12
139		mov r5, r8
140		adds r8, r4, r5
141		blvs _errOverflow
142		push {r8}
143		pop {r0}
144		mov r0, r0
145		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
146		mov sp, fp
147		pop {r4, r5}
148		pop {fp, pc}
149		@ Stack pointer unchanged, no stack allocated variables
150		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
151		@ the .ltorg here is used to generate the constant pool after every function
152		@ this is important for ensuring string literals and large integers are accessible!
153		.ltorg
154	
155	@ length of .L._prints_str0
156		.word 4
157	.L._prints_str0:
158		.asciz "%.*s"
159	.align 4
160	_prints:
161		push {fp, lr}
162		mov fp, sp
163		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
164		bic sp, sp, #0x7
165		mov r2, r0
166		ldr r1, [r0, #-4]
167		adr r0, .L._prints_str0
168		bl printf
169		mov r0, #0
170		bl fflush
171		mov sp, fp
172		pop {fp, pc}
173	
174	@ length of .L._readi_str0
175		.word 2
176	.L._readi_str0:
177		.asciz "%d"
178	.align 4
179	_readi:
180		push {fp, lr}
181		mov fp, sp
182		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
183		bic sp, sp, #0x7
184		@ R0 contains the "original" value of the destination of the read
185		@ allocate space on the stack to store the read: preserve alignment!
186		@ the passed default argument should be stored in case of EOF
187		sub sp, sp, #8
188		str r0, [sp, #0]
189		mov r1, sp
190		adr r0, .L._readi_str0
191		bl scanf
192		ldr r0, [sp, #0]
193		add sp, sp, #8
194		mov sp, fp
195		pop {fp, pc}
196	
197	@ length of .L._printi_str0
198		.word 2
199	.L._printi_str0:
200		.asciz "%d"
201	.align 4
202	_printi:
203		push {fp, lr}
204		mov fp, sp
205		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
206		bic sp, sp, #0x7
207		mov r1, r0
208		adr r0, .L._printi_str0
209		bl printf
210		mov r0, #0
211		bl fflush
212		mov sp, fp
213		pop {fp, pc}
214	
215	@ length of .L._println_str0
216		.word 0
217	.L._println_str0:
218		.asciz ""
219	.align 4
220	_println:
221		push {fp, lr}
222		mov fp, sp
223		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
224		bic sp, sp, #0x7
225		adr r0, .L._println_str0
226		bl puts
227		mov r0, #0
228		bl fflush
229		mov sp, fp
230		pop {fp, pc}
231	
232	_exit:
233		push {fp, lr}
234		mov fp, sp
235		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
236		bic sp, sp, #0x7
237		bl exit
238		mov sp, fp
239		pop {fp, pc}
240	
241	@ length of .L._errOverflow_str0
242		.word 52
243	.L._errOverflow_str0:
244		.asciz "fatal error: integer overflow or underflow occurred\n"
245	.align 4
246	_errOverflow:
247		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
248		bic sp, sp, #0x7
249		adr r0, .L._errOverflow_str0
250		bl _prints
251		mov r0, #255
252		bl _exit
===========================================================
-- Finished

