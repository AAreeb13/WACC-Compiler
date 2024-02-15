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
35		movq $1, %rax
36		movl %eax, 4(%r11)
37		movq $2, %rax
38		movl %eax, 8(%r11)
39		movq $3, %rax
40		movl %eax, 12(%r11)
41		movq $4, %rax
42		movl %eax, 16(%r11)
43		movq $5, %rax
44		movl %eax, 20(%r11)
45		movq $6, %rax
46		movl %eax, 24(%r11)
47		movq $7, %rax
48		movl %eax, 28(%r11)
49		movq $8, %rax
50		movl %eax, 32(%r11)
51		movq $9, %rax
52		movl %eax, 36(%r11)
53		movq %r11, %rax
54		movq %rax, %r12
55		movq $0, %rax
56		movq %rax, %r13
57		# Stack pointer unchanged, no stack allocated arguments
58		movq %r12, %rax
59		movq %rax, %rdi
60		# statement primitives do not return results (but will clobber r0/rax)
61		call _printp
62		# Stack pointer unchanged, no stack allocated arguments
63		leaq .L.str0(%rip), %rax
64		pushq %rax
65		popq %rax
66		movq %rax, %rax
67		movq %rax, %rdi
68		# statement primitives do not return results (but will clobber r0/rax)
69		call _prints
70		movq $0, %rax
71		movq %rax, %r13
72		jmp .L0
73	.L1:
74		# Stack pointer unchanged, no stack allocated arguments
75		movl %r13d, %r10d
76		movq %r12, %r9
77		call _arrLoad4
78		movl %r9d, %eax
79		movq %rax, %rax
80		movq %rax, %rdi
81		# statement primitives do not return results (but will clobber r0/rax)
82		call _printi
83		cmpq $9, %r13
84		jl .L2
85		jmp .L3
86	.L2:
87		# Stack pointer unchanged, no stack allocated arguments
88		leaq .L.str1(%rip), %rax
89		pushq %rax
90		popq %rax
91		movq %rax, %rax
92		movq %rax, %rdi
93		# statement primitives do not return results (but will clobber r0/rax)
94		call _prints
95	.L3:
96		movl %r13d, %eax
97		addl $1, %eax
98		jo _errOverflow
99		movslq %eax, %rax
100		pushq %rax
101		popq %rax
102		movq %rax, %rax
103		movq %rax, %r13
104	.L0:
105		cmpq $10, %r13
106		jl .L1
107		# Stack pointer unchanged, no stack allocated arguments
108		leaq .L.str2(%rip), %rax
109		pushq %rax
110		popq %rax
111		movq %rax, %rax
112		movq %rax, %rdi
113		# statement primitives do not return results (but will clobber r0/rax)
114		call _prints
115		call _println
116		# Stack pointer unchanged, no stack allocated variables
117		movq $0, %rax
118		# popq {%rbx, %r12, %r13}
119		movq (%rsp), %rbx
120		movq 8(%rsp), %r12
121		movq 16(%rsp), %r13
122		addq $24, %rsp
123		popq %rbp
124		ret
125	
126	.section .rodata
127	# length of .L._prints_str0
128		.int 4
129	.L._prints_str0:
130		.asciz "%.*s"
131	.text
132	_prints:
133		pushq %rbp
134		movq %rsp, %rbp
135		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
136		andq $-16, %rsp
137		movq %rdi, %rdx
138		movl -4(%rdi), %esi
139		leaq .L._prints_str0(%rip), %rdi
140		# on x86, al represents the number of SIMD registers used as variadic arguments
141		movb $0, %al
142		call printf@plt
143		movq $0, %rdi
144		call fflush@plt
145		movq %rbp, %rsp
146		popq %rbp
147		ret
148	
149	.section .rodata
150	# length of .L._printp_str0
151		.int 2
152	.L._printp_str0:
153		.asciz "%p"
154	.text
155	_printp:
156		pushq %rbp
157		movq %rsp, %rbp
158		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
159		andq $-16, %rsp
160		movq %rdi, %rsi
161		leaq .L._printp_str0(%rip), %rdi
162		# on x86, al represents the number of SIMD registers used as variadic arguments
163		movb $0, %al
164		call printf@plt
165		movq $0, %rdi
166		call fflush@plt
167		movq %rbp, %rsp
168		popq %rbp
169		ret
170	
171	_malloc:
172		pushq %rbp
173		movq %rsp, %rbp
174		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
175		andq $-16, %rsp
176		call malloc@plt
177		cmpq $0, %rax
178		je _errOutOfMemory
179		movq %rbp, %rsp
180		popq %rbp
181		ret
182	
183	.section .rodata
184	# length of .L._printi_str0
185		.int 2
186	.L._printi_str0:
187		.asciz "%d"
188	.text
189	_printi:
190		pushq %rbp
191		movq %rsp, %rbp
192		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
193		andq $-16, %rsp
194		movl %edi, %esi
195		leaq .L._printi_str0(%rip), %rdi
196		# on x86, al represents the number of SIMD registers used as variadic arguments
197		movb $0, %al
198		call printf@plt
199		movq $0, %rdi
200		call fflush@plt
201		movq %rbp, %rsp
202		popq %rbp
203		ret
204	
205	.section .rodata
206	# length of .L._println_str0
207		.int 0
208	.L._println_str0:
209		.asciz ""
210	.text
211	_println:
212		pushq %rbp
213		movq %rsp, %rbp
214		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
215		andq $-16, %rsp
216		leaq .L._println_str0(%rip), %rdi
217		call puts@plt
218		movq $0, %rdi
219		call fflush@plt
220		movq %rbp, %rsp
221		popq %rbp
222		ret
223	
224	_exit:
225		pushq %rbp
226		movq %rsp, %rbp
227		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
228		andq $-16, %rsp
229		call exit@plt
230		movq %rbp, %rsp
231		popq %rbp
232		ret
233	
234	_arrLoad4:
235		# Special calling convention: array ptr passed in R9, index in R10, and return into R9
236		pushq %rbx
237		cmpl $0, %r10d
238		cmovl %r10d, %esi
239		jl _errOutOfBounds
240		movl -4(%r9), %ebx
241		cmpl %ebx, %r10d
242		cmovge %r10d, %esi
243		jge _errOutOfBounds
244		movslq (%r9,%r10,4), %r9
245		popq %rbx
246		ret
247	
248	.section .rodata
249	# length of .L._errOutOfMemory_str0
250		.int 27
251	.L._errOutOfMemory_str0:
252		.asciz "fatal error: out of memory\n"
253	.text
254	_errOutOfMemory:
255		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
256		andq $-16, %rsp
257		leaq .L._errOutOfMemory_str0(%rip), %rdi
258		call _prints
259		movb $-1, %dil
260		call exit@plt
261	
262	.section .rodata
263	# length of .L._errOutOfBounds_str0
264		.int 42
265	.L._errOutOfBounds_str0:
266		.asciz "fatal error: array index %d out of bounds\n"
267	.text
268	_errOutOfBounds:
269		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
270		andq $-16, %rsp
271		leaq .L._errOutOfBounds_str0(%rip), %rdi
272		# on x86, al represents the number of SIMD registers used as variadic arguments
273		movb $0, %al
274		call printf@plt
275		movq $0, %rdi
276		call fflush@plt
277		movb $-1, %dil
278		call exit@plt
279	
280	.section .rodata
281	# length of .L._errOverflow_str0
282		.int 52
283	.L._errOverflow_str0:
284		.asciz "fatal error: integer overflow or underflow occurred\n"
285	.text
286	_errOverflow:
287		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
288		andq $-16, %rsp
289		leaq .L._errOverflow_str0(%rip), %rdi
290		call _prints
291		movb $-1, %dil
292		call exit@plt
===========================================================
-- Finished

