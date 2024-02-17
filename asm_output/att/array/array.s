./valid/array/array.wacc
calling the reference compiler on ./valid/array/array.wacc
-- Test: array.wacc

-- Uploaded file: 
---------------------------------------------------------------
# moderate complexity array manipulations

# Output:
# #addrs# = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9}
#

# Program:

begin
  int[] a = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ;
  int i = 0 ;
  while i < 10
  do
    a[i] = i ;
    i = i + 1
  done ;
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
array.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 4
4	.L.str0:
5		.asciz " = {"
6	# length of .L.str1
7		.int 2
8	.L.str1:
9		.asciz ", "
10	# length of .L.str2
11		.int 1
12	.L.str2:
13		.asciz "}"
14	.text
15	main:
16		pushq %rbp
17		# pushq {%rbx, %r12, %r13}
18		subq $24, %rsp
19		movq %rbx, (%rsp)
20		movq %r12, 8(%rsp)
21		movq %r13, 16(%rsp)
22		movq %rsp, %rbp
23		# Stack pointer unchanged, no stack allocated variables
24		# 10 element array
25		movl $44, %edi
26		call _malloc
27		movq %rax, %r11
28		# array pointers are shifted forwards by 4 bytes (to account for size)
29		movq %r11, %r11
30		addq $4, %r11
31		movq $10, %rax
32		movl %eax, -4(%r11)
33		movq $0, %rax
34		movl %eax, (%r11)
35		movq $0, %rax
36		movl %eax, 4(%r11)
37		movq $0, %rax
38		movl %eax, 8(%r11)
39		movq $0, %rax
40		movl %eax, 12(%r11)
41		movq $0, %rax
42		movl %eax, 16(%r11)
43		movq $0, %rax
44		movl %eax, 20(%r11)
45		movq $0, %rax
46		movl %eax, 24(%r11)
47		movq $0, %rax
48		movl %eax, 28(%r11)
49		movq $0, %rax
50		movl %eax, 32(%r11)
51		movq $0, %rax
52		movl %eax, 36(%r11)
53		movq %r11, %rax
54		movq %rax, %r12
55		movq $0, %rax
56		movq %rax, %r13
57		jmp .L0
58	.L1:
59		movl %r13d, %r10d
60		movq %r13, %rax
61		movq %r12, %r9
62		call _arrStore4
63		movl %r13d, %eax
64		addl $1, %eax
65		jo _errOverflow
66		movslq %eax, %rax
67		pushq %rax
68		popq %rax
69		movq %rax, %rax
70		movq %rax, %r13
71	.L0:
72		cmpq $10, %r13
73		jl .L1
74		# Stack pointer unchanged, no stack allocated arguments
75		movq %r12, %rax
76		movq %rax, %rdi
77		# statement primitives do not return results (but will clobber r0/rax)
78		call _printp
79		# Stack pointer unchanged, no stack allocated arguments
80		leaq .L.str0(%rip), %rax
81		pushq %rax
82		popq %rax
83		movq %rax, %rax
84		movq %rax, %rdi
85		# statement primitives do not return results (but will clobber r0/rax)
86		call _prints
87		movq $0, %rax
88		movq %rax, %r13
89		jmp .L2
90	.L3:
91		# Stack pointer unchanged, no stack allocated arguments
92		movl %r13d, %r10d
93		movq %r12, %r9
94		call _arrLoad4
95		movl %r9d, %eax
96		movq %rax, %rax
97		movq %rax, %rdi
98		# statement primitives do not return results (but will clobber r0/rax)
99		call _printi
100		cmpq $9, %r13
101		jl .L4
102		jmp .L5
103	.L4:
104		# Stack pointer unchanged, no stack allocated arguments
105		leaq .L.str1(%rip), %rax
106		pushq %rax
107		popq %rax
108		movq %rax, %rax
109		movq %rax, %rdi
110		# statement primitives do not return results (but will clobber r0/rax)
111		call _prints
112	.L5:
113		movl %r13d, %eax
114		addl $1, %eax
115		jo _errOverflow
116		movslq %eax, %rax
117		pushq %rax
118		popq %rax
119		movq %rax, %rax
120		movq %rax, %r13
121	.L2:
122		cmpq $10, %r13
123		jl .L3
124		# Stack pointer unchanged, no stack allocated arguments
125		leaq .L.str2(%rip), %rax
126		pushq %rax
127		popq %rax
128		movq %rax, %rax
129		movq %rax, %rdi
130		# statement primitives do not return results (but will clobber r0/rax)
131		call _prints
132		call _println
133		# Stack pointer unchanged, no stack allocated variables
134		movq $0, %rax
135		# popq {%rbx, %r12, %r13}
136		movq (%rsp), %rbx
137		movq 8(%rsp), %r12
138		movq 16(%rsp), %r13
139		addq $24, %rsp
140		popq %rbp
141		ret
142	
143	.section .rodata
144	# length of .L._prints_str0
145		.int 4
146	.L._prints_str0:
147		.asciz "%.*s"
148	.text
149	_prints:
150		pushq %rbp
151		movq %rsp, %rbp
152		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
153		andq $-16, %rsp
154		movq %rdi, %rdx
155		movl -4(%rdi), %esi
156		leaq .L._prints_str0(%rip), %rdi
157		# on x86, al represents the number of SIMD registers used as variadic arguments
158		movb $0, %al
159		call printf@plt
160		movq $0, %rdi
161		call fflush@plt
162		movq %rbp, %rsp
163		popq %rbp
164		ret
165	
166	.section .rodata
167	# length of .L._printp_str0
168		.int 2
169	.L._printp_str0:
170		.asciz "%p"
171	.text
172	_printp:
173		pushq %rbp
174		movq %rsp, %rbp
175		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
176		andq $-16, %rsp
177		movq %rdi, %rsi
178		leaq .L._printp_str0(%rip), %rdi
179		# on x86, al represents the number of SIMD registers used as variadic arguments
180		movb $0, %al
181		call printf@plt
182		movq $0, %rdi
183		call fflush@plt
184		movq %rbp, %rsp
185		popq %rbp
186		ret
187	
188	_malloc:
189		pushq %rbp
190		movq %rsp, %rbp
191		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
192		andq $-16, %rsp
193		call malloc@plt
194		cmpq $0, %rax
195		je _errOutOfMemory
196		movq %rbp, %rsp
197		popq %rbp
198		ret
199	
200	.section .rodata
201	# length of .L._printi_str0
202		.int 2
203	.L._printi_str0:
204		.asciz "%d"
205	.text
206	_printi:
207		pushq %rbp
208		movq %rsp, %rbp
209		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
210		andq $-16, %rsp
211		movl %edi, %esi
212		leaq .L._printi_str0(%rip), %rdi
213		# on x86, al represents the number of SIMD registers used as variadic arguments
214		movb $0, %al
215		call printf@plt
216		movq $0, %rdi
217		call fflush@plt
218		movq %rbp, %rsp
219		popq %rbp
220		ret
221	
222	.section .rodata
223	# length of .L._println_str0
224		.int 0
225	.L._println_str0:
226		.asciz ""
227	.text
228	_println:
229		pushq %rbp
230		movq %rsp, %rbp
231		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
232		andq $-16, %rsp
233		leaq .L._println_str0(%rip), %rdi
234		call puts@plt
235		movq $0, %rdi
236		call fflush@plt
237		movq %rbp, %rsp
238		popq %rbp
239		ret
240	
241	_exit:
242		pushq %rbp
243		movq %rsp, %rbp
244		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
245		andq $-16, %rsp
246		call exit@plt
247		movq %rbp, %rsp
248		popq %rbp
249		ret
250	
251	_arrStore4:
252		# Special calling convention: array ptr passed in R9, index in R10, value to store in RAX
253		pushq %rbx
254		cmpl $0, %r10d
255		cmovl %r10d, %esi
256		jl _errOutOfBounds
257		movl -4(%r9), %ebx
258		cmpl %ebx, %r10d
259		cmovge %r10d, %esi
260		jge _errOutOfBounds
261		movl %eax, (%r9,%r10,4)
262		popq %rbx
263		ret
264	
265	_arrLoad4:
266		# Special calling convention: array ptr passed in R9, index in R10, and return into R9
267		pushq %rbx
268		cmpl $0, %r10d
269		cmovl %r10d, %esi
270		jl _errOutOfBounds
271		movl -4(%r9), %ebx
272		cmpl %ebx, %r10d
273		cmovge %r10d, %esi
274		jge _errOutOfBounds
275		movslq (%r9,%r10,4), %r9
276		popq %rbx
277		ret
278	
279	.section .rodata
280	# length of .L._errOutOfMemory_str0
281		.int 27
282	.L._errOutOfMemory_str0:
283		.asciz "fatal error: out of memory\n"
284	.text
285	_errOutOfMemory:
286		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
287		andq $-16, %rsp
288		leaq .L._errOutOfMemory_str0(%rip), %rdi
289		call _prints
290		movb $-1, %dil
291		call exit@plt
292	
293	.section .rodata
294	# length of .L._errOutOfBounds_str0
295		.int 42
296	.L._errOutOfBounds_str0:
297		.asciz "fatal error: array index %d out of bounds\n"
298	.text
299	_errOutOfBounds:
300		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
301		andq $-16, %rsp
302		leaq .L._errOutOfBounds_str0(%rip), %rdi
303		# on x86, al represents the number of SIMD registers used as variadic arguments
304		movb $0, %al
305		call printf@plt
306		movq $0, %rdi
307		call fflush@plt
308		movb $-1, %dil
309		call exit@plt
310	
311	.section .rodata
312	# length of .L._errOverflow_str0
313		.int 52
314	.L._errOverflow_str0:
315		.asciz "fatal error: integer overflow or underflow occurred\n"
316	.text
317	_errOverflow:
318		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
319		andq $-16, %rsp
320		leaq .L._errOverflow_str0(%rip), %rdi
321		call _prints
322		movb $-1, %dil
323		call exit@plt
===========================================================
-- Finished

