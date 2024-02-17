./valid/array/arrayOnHeap.wacc
calling the reference compiler on ./valid/array/arrayOnHeap.wacc
-- Test: arrayOnHeap.wacc

-- Uploaded file: 
---------------------------------------------------------------
# ensures that arrays are heap allocated

# Output:
# false
# 0
# 1
#

# Program:

begin
  int[] dummy_arr = [] ;
  int[][] arrs = [dummy_arr, dummy_arr] ;
  int i = 0;
  while i < 2 do
    int[] arr = [i] ;
    arrs[i] = arr ;
    i = i + 1
  done;

  # should have different addresses
  println arrs[0] == arrs[1];
  # should have different values
  println arrs[0][0];
  println arrs[1][0]
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
arrayOnHeap.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	.text
3	main:
4		pushq %rbp
5		# pushq {%rbx, %r12, %r13, %r14, %r15}
6		subq $40, %rsp
7		movq %rbx, (%rsp)
8		movq %r12, 8(%rsp)
9		movq %r13, 16(%rsp)
10		movq %r14, 24(%rsp)
11		movq %r15, 32(%rsp)
12		movq %rsp, %rbp
13		# Stack pointer unchanged, no stack allocated variables
14		# 0 element array
15		movl $4, %edi
16		call _malloc
17		movq %rax, %r11
18		# array pointers are shifted forwards by 4 bytes (to account for size)
19		movq %r11, %r11
20		addq $4, %r11
21		movq $0, %rax
22		movl %eax, -4(%r11)
23		movq %r11, %rax
24		movq %rax, %r12
25		# 2 element array
26		movl $20, %edi
27		call _malloc
28		movq %rax, %r11
29		# array pointers are shifted forwards by 4 bytes (to account for size)
30		movq %r11, %r11
31		addq $4, %r11
32		movq $2, %rax
33		movl %eax, -4(%r11)
34		movq %r12, %rax
35		movq %rax, (%r11)
36		movq %r12, %rax
37		movq %rax, 8(%r11)
38		movq %r11, %rax
39		movq %rax, %r13
40		movq $0, %rax
41		movq %rax, %r14
42		jmp .L0
43	.L1:
44		# Stack pointer unchanged, no stack allocated variables
45		# 1 element array
46		movl $8, %edi
47		call _malloc
48		movq %rax, %r11
49		# array pointers are shifted forwards by 4 bytes (to account for size)
50		movq %r11, %r11
51		addq $4, %r11
52		movq $1, %rax
53		movl %eax, -4(%r11)
54		movq %r14, %rax
55		movl %eax, (%r11)
56		movq %r11, %rax
57		movq %rax, %r15
58		movl %r14d, %r10d
59		movq %r15, %rax
60		movq %r13, %r9
61		call _arrStore8
62		movl %r14d, %eax
63		addl $1, %eax
64		jo _errOverflow
65		movslq %eax, %rax
66		pushq %rax
67		popq %rax
68		movq %rax, %rax
69		movq %rax, %r14
70		# Stack pointer unchanged, no stack allocated variables
71	.L0:
72		cmpq $2, %r14
73		jl .L1
74		# Stack pointer unchanged, no stack allocated arguments
75		movl $1, %r10d
76		movq %r13, %r9
77		call _arrLoad8
78		movq %r9, %rbx
79		movl $0, %r10d
80		movq %r13, %r9
81		call _arrLoad8
82		movq %r9, %rax
83		cmpq %rbx, %rax
84		sete %al
85		movsbq %al, %rax
86		pushq %rax
87		popq %rax
88		movq %rax, %rax
89		movq %rax, %rdi
90		# statement primitives do not return results (but will clobber r0/rax)
91		call _printb
92		call _println
93		# Stack pointer unchanged, no stack allocated arguments
94		movl $0, %r10d
95		movq %r13, %r9
96		call _arrLoad8
97		movq %r9, %rax
98		movq %rax, %rax
99		pushq %rax
100		movl $0, %r10d
101		popq %rax
102		movq %rax, %r9
103		call _arrLoad4
104		movl %r9d, %eax
105		movq %rax, %rax
106		movq %rax, %rdi
107		# statement primitives do not return results (but will clobber r0/rax)
108		call _printi
109		call _println
110		# Stack pointer unchanged, no stack allocated arguments
111		movl $1, %r10d
112		movq %r13, %r9
113		call _arrLoad8
114		movq %r9, %rax
115		movq %rax, %rax
116		pushq %rax
117		movl $0, %r10d
118		popq %rax
119		movq %rax, %r9
120		call _arrLoad4
121		movl %r9d, %eax
122		movq %rax, %rax
123		movq %rax, %rdi
124		# statement primitives do not return results (but will clobber r0/rax)
125		call _printi
126		call _println
127		# Stack pointer unchanged, no stack allocated variables
128		movq $0, %rax
129		# popq {%rbx, %r12, %r13, %r14, %r15}
130		movq (%rsp), %rbx
131		movq 8(%rsp), %r12
132		movq 16(%rsp), %r13
133		movq 24(%rsp), %r14
134		movq 32(%rsp), %r15
135		addq $40, %rsp
136		popq %rbp
137		ret
138	
139	.section .rodata
140	# length of .L._prints_str0
141		.int 4
142	.L._prints_str0:
143		.asciz "%.*s"
144	.text
145	_prints:
146		pushq %rbp
147		movq %rsp, %rbp
148		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
149		andq $-16, %rsp
150		movq %rdi, %rdx
151		movl -4(%rdi), %esi
152		leaq .L._prints_str0(%rip), %rdi
153		# on x86, al represents the number of SIMD registers used as variadic arguments
154		movb $0, %al
155		call printf@plt
156		movq $0, %rdi
157		call fflush@plt
158		movq %rbp, %rsp
159		popq %rbp
160		ret
161	
162	_malloc:
163		pushq %rbp
164		movq %rsp, %rbp
165		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
166		andq $-16, %rsp
167		call malloc@plt
168		cmpq $0, %rax
169		je _errOutOfMemory
170		movq %rbp, %rsp
171		popq %rbp
172		ret
173	
174	.section .rodata
175	# length of .L._printi_str0
176		.int 2
177	.L._printi_str0:
178		.asciz "%d"
179	.text
180	_printi:
181		pushq %rbp
182		movq %rsp, %rbp
183		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
184		andq $-16, %rsp
185		movl %edi, %esi
186		leaq .L._printi_str0(%rip), %rdi
187		# on x86, al represents the number of SIMD registers used as variadic arguments
188		movb $0, %al
189		call printf@plt
190		movq $0, %rdi
191		call fflush@plt
192		movq %rbp, %rsp
193		popq %rbp
194		ret
195	
196	.section .rodata
197	# length of .L._printb_str0
198		.int 5
199	.L._printb_str0:
200		.asciz "false"
201	# length of .L._printb_str1
202		.int 4
203	.L._printb_str1:
204		.asciz "true"
205	# length of .L._printb_str2
206		.int 4
207	.L._printb_str2:
208		.asciz "%.*s"
209	.text
210	_printb:
211		pushq %rbp
212		movq %rsp, %rbp
213		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
214		andq $-16, %rsp
215		cmpb $0, %dil
216		jne .L_printb0
217		leaq .L._printb_str0(%rip), %rdx
218		jmp .L_printb1
219	.L_printb0:
220		leaq .L._printb_str1(%rip), %rdx
221	.L_printb1:
222		movl -4(%rdx), %esi
223		leaq .L._printb_str2(%rip), %rdi
224		# on x86, al represents the number of SIMD registers used as variadic arguments
225		movb $0, %al
226		call printf@plt
227		movq $0, %rdi
228		call fflush@plt
229		movq %rbp, %rsp
230		popq %rbp
231		ret
232	
233	.section .rodata
234	# length of .L._println_str0
235		.int 0
236	.L._println_str0:
237		.asciz ""
238	.text
239	_println:
240		pushq %rbp
241		movq %rsp, %rbp
242		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
243		andq $-16, %rsp
244		leaq .L._println_str0(%rip), %rdi
245		call puts@plt
246		movq $0, %rdi
247		call fflush@plt
248		movq %rbp, %rsp
249		popq %rbp
250		ret
251	
252	_exit:
253		pushq %rbp
254		movq %rsp, %rbp
255		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
256		andq $-16, %rsp
257		call exit@plt
258		movq %rbp, %rsp
259		popq %rbp
260		ret
261	
262	_arrLoad8:
263		# Special calling convention: array ptr passed in R9, index in R10, and return into R9
264		pushq %rbx
265		cmpl $0, %r10d
266		cmovl %r10d, %esi
267		jl _errOutOfBounds
268		movl -4(%r9), %ebx
269		cmpl %ebx, %r10d
270		cmovge %r10d, %esi
271		jge _errOutOfBounds
272		movq (%r9,%r10,8), %r9
273		popq %rbx
274		ret
275	
276	_arrLoad4:
277		# Special calling convention: array ptr passed in R9, index in R10, and return into R9
278		pushq %rbx
279		cmpl $0, %r10d
280		cmovl %r10d, %esi
281		jl _errOutOfBounds
282		movl -4(%r9), %ebx
283		cmpl %ebx, %r10d
284		cmovge %r10d, %esi
285		jge _errOutOfBounds
286		movslq (%r9,%r10,4), %r9
287		popq %rbx
288		ret
289	
290	_arrStore8:
291		# Special calling convention: array ptr passed in R9, index in R10, value to store in RAX
292		pushq %rbx
293		cmpl $0, %r10d
294		cmovl %r10d, %esi
295		jl _errOutOfBounds
296		movl -4(%r9), %ebx
297		cmpl %ebx, %r10d
298		cmovge %r10d, %esi
299		jge _errOutOfBounds
300		movq %rax, (%r9,%r10,8)
301		popq %rbx
302		ret
303	
304	.section .rodata
305	# length of .L._errOutOfMemory_str0
306		.int 27
307	.L._errOutOfMemory_str0:
308		.asciz "fatal error: out of memory\n"
309	.text
310	_errOutOfMemory:
311		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
312		andq $-16, %rsp
313		leaq .L._errOutOfMemory_str0(%rip), %rdi
314		call _prints
315		movb $-1, %dil
316		call exit@plt
317	
318	.section .rodata
319	# length of .L._errOutOfBounds_str0
320		.int 42
321	.L._errOutOfBounds_str0:
322		.asciz "fatal error: array index %d out of bounds\n"
323	.text
324	_errOutOfBounds:
325		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
326		andq $-16, %rsp
327		leaq .L._errOutOfBounds_str0(%rip), %rdi
328		# on x86, al represents the number of SIMD registers used as variadic arguments
329		movb $0, %al
330		call printf@plt
331		movq $0, %rdi
332		call fflush@plt
333		movb $-1, %dil
334		call exit@plt
335	
336	.section .rodata
337	# length of .L._errOverflow_str0
338		.int 52
339	.L._errOverflow_str0:
340		.asciz "fatal error: integer overflow or underflow occurred\n"
341	.text
342	_errOverflow:
343		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
344		andq $-16, %rsp
345		leaq .L._errOverflow_str0(%rip), %rdi
346		call _prints
347		movb $-1, %dil
348		call exit@plt
===========================================================
-- Finished

