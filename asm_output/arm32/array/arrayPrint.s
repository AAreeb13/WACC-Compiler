./valid/array/arrayPrint.wacc
calling the reference compiler on ./valid/array/arrayPrint.wacc
-- Test: arrayPrint.wacc

-- Uploaded file: 
---------------------------------------------------------------
# print the contents of a simple array

# Output:
# #addrs# = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9}
#

# Program:

begin
  int[] a = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9] ;
  int i = 0 ;
  print a ;
  print " = {" ;
  i = 0 ;
  while i < 10
  do
    print a[i] ;
    if i < 9
    then
      print ", "
    else
      skip
    fi ;
    i = i + 1
  done ;
  println "}"
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
arrayPrint.s contents are:
===========================================================
0	.data
1	@ length of .L.str0
2		.word 4
3	.L.str0:
4		.asciz " = {"
5	@ length of .L.str1
6		.word 2
7	.L.str1:
8		.asciz ", "
9	@ length of .L.str2
10		.word 1
11	.L.str2:
12		.asciz "}"
13	.align 4
14	.text
15	.global main
16	main:
17		push {fp, lr}
18		push {r4, r5, r8, r10, r12}
19		mov fp, sp
20		@ Stack pointer unchanged, no stack allocated variables
21		@ 10 element array
22		mov r0, #44
23		bl _malloc
24		mov r12, r0
25		@ array pointers are shifted forwards by 4 bytes (to account for size)
26		add r12, r12, #4
27		mov r8, #10
28		str r8, [r12, #-4]
29		mov r8, #0
30		str r8, [r12, #0]
31		mov r8, #1
32		str r8, [r12, #4]
33		mov r8, #2
34		str r8, [r12, #8]
35		mov r8, #3
36		str r8, [r12, #12]
37		mov r8, #4
38		str r8, [r12, #16]
39		mov r8, #5
40		str r8, [r12, #20]
41		mov r8, #6
42		str r8, [r12, #24]
43		mov r8, #7
44		str r8, [r12, #28]
45		mov r8, #8
46		str r8, [r12, #32]
47		mov r8, #9
48		str r8, [r12, #36]
49		mov r8, r12
50		mov r4, r8
51		mov r8, #0
52		mov r5, r8
53		@ Stack pointer unchanged, no stack allocated arguments
54		mov r8, r4
55		mov r0, r8
56		@ statement primitives do not return results (but will clobber r0/rax)
57		bl _printp
58		@ Stack pointer unchanged, no stack allocated arguments
59		ldr r8, =.L.str0
60		push {r8}
61		pop {r8}
62		mov r8, r8
63		mov r0, r8
64		@ statement primitives do not return results (but will clobber r0/rax)
65		bl _prints
66		mov r8, #0
67		mov r5, r8
68		b .L0
69	.L1:
70		@ Stack pointer unchanged, no stack allocated arguments
71		mov r10, r5
72		mov r3, r4
73		bl _arrLoad4
74		mov r8, r3
75		mov r8, r8
76		mov r0, r8
77		@ statement primitives do not return results (but will clobber r0/rax)
78		bl _printi
79		cmp r5, #9
80		blt .L2
81		b .L3
82	.L2:
83		@ Stack pointer unchanged, no stack allocated arguments
84		ldr r8, =.L.str1
85		push {r8}
86		pop {r8}
87		mov r8, r8
88		mov r0, r8
89		@ statement primitives do not return results (but will clobber r0/rax)
90		bl _prints
91	.L3:
92		adds r8, r5, #1
93		blvs _errOverflow
94		push {r8}
95		pop {r8}
96		mov r8, r8
97		mov r5, r8
98	.L0:
99		cmp r5, #10
100		blt .L1
101		@ Stack pointer unchanged, no stack allocated arguments
102		ldr r8, =.L.str2
103		push {r8}
104		pop {r8}
105		mov r8, r8
106		mov r0, r8
107		@ statement primitives do not return results (but will clobber r0/rax)
108		bl _prints
109		bl _println
110		@ Stack pointer unchanged, no stack allocated variables
111		mov r0, #0
112		pop {r4, r5, r8, r10, r12}
113		pop {fp, pc}
114	
115	@ length of .L._prints_str0
116		.word 4
117	.L._prints_str0:
118		.asciz "%.*s"
119	.align 4
120	_prints:
121		push {fp, lr}
122		mov fp, sp
123		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
124		bic sp, sp, #0x7
125		mov r2, r0
126		ldr r1, [r0, #-4]
127		adr r0, .L._prints_str0
128		bl printf
129		mov r0, #0
130		bl fflush
131		mov sp, fp
132		pop {fp, pc}
133	
134	@ length of .L._printp_str0
135		.word 2
136	.L._printp_str0:
137		.asciz "%p"
138	.align 4
139	_printp:
140		push {fp, lr}
141		mov fp, sp
142		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
143		bic sp, sp, #0x7
144		mov r1, r0
145		adr r0, .L._printp_str0
146		bl printf
147		mov r0, #0
148		bl fflush
149		mov sp, fp
150		pop {fp, pc}
151	
152	_malloc:
153		push {fp, lr}
154		mov fp, sp
155		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
156		bic sp, sp, #0x7
157		bl malloc
158		cmp r0, #0
159		bleq _errOutOfMemory
160		mov sp, fp
161		pop {fp, pc}
162	
163	@ length of .L._printi_str0
164		.word 2
165	.L._printi_str0:
166		.asciz "%d"
167	.align 4
168	_printi:
169		push {fp, lr}
170		mov fp, sp
171		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
172		bic sp, sp, #0x7
173		mov r1, r0
174		adr r0, .L._printi_str0
175		bl printf
176		mov r0, #0
177		bl fflush
178		mov sp, fp
179		pop {fp, pc}
180	
181	@ length of .L._println_str0
182		.word 0
183	.L._println_str0:
184		.asciz ""
185	.align 4
186	_println:
187		push {fp, lr}
188		mov fp, sp
189		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
190		bic sp, sp, #0x7
191		adr r0, .L._println_str0
192		bl puts
193		mov r0, #0
194		bl fflush
195		mov sp, fp
196		pop {fp, pc}
197	
198	_exit:
199		push {fp, lr}
200		mov fp, sp
201		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
202		bic sp, sp, #0x7
203		bl exit
204		mov sp, fp
205		pop {fp, pc}
206	
207	_arrLoad4:
208		@ Special calling convention: array ptr passed in R3, index in R10, LR (R14) is used as general register, and return into R3
209		push {lr}
210		cmp r10, #0
211		movlt r1, r10
212		bllt _errOutOfBounds
213		ldr lr, [r3, #-4]
214		cmp r10, lr
215		movge r1, r10
216		blge _errOutOfBounds
217		ldr r3, [r3, r10, lsl #2]
218		pop {pc}
219	
220	@ length of .L._errOutOfMemory_str0
221		.word 27
222	.L._errOutOfMemory_str0:
223		.asciz "fatal error: out of memory\n"
224	.align 4
225	_errOutOfMemory:
226		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
227		bic sp, sp, #0x7
228		adr r0, .L._errOutOfMemory_str0
229		bl _prints
230		mov r0, #255
231		bl _exit
232	
233	@ length of .L._errOutOfBounds_str0
234		.word 42
235	.L._errOutOfBounds_str0:
236		.asciz "fatal error: array index %d out of bounds\n"
237	.align 4
238	_errOutOfBounds:
239		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
240		bic sp, sp, #0x7
241		adr r0, .L._errOutOfBounds_str0
242		bl printf
243		mov r0, #0
244		bl fflush
245		mov r0, #255
246		bl _exit
247	
248	@ length of .L._errOverflow_str0
249		.word 52
250	.L._errOverflow_str0:
251		.asciz "fatal error: integer overflow or underflow occurred\n"
252	.align 4
253	_errOverflow:
254		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
255		bic sp, sp, #0x7
256		adr r0, .L._errOverflow_str0
257		bl _prints
258		mov r0, #255
259		bl _exit
===========================================================
-- Finished

