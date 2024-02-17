./valid/array/arrayIndexMayBeArrayIndex.wacc
calling the reference compiler on ./valid/array/arrayIndexMayBeArrayIndex.wacc
-- Test: arrayIndexMayBeArrayIndex.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Testing recursive array indexing

# Output:
# 6
# 7
# 8
#

# Program:

begin
    int[] idxs1 = [2, 0, 1] ;
    int[] idxs2 = [1, 2, 0] ;
    # idxs1[idxs2[0]] = 0
    # idxs1[idxs2[1]] = 1
    # idxs1[idxs2[2]] = 2
    int[] xs = [5, 6, 7] ;
    int i = 0 ;
    while i != 3 do
        xs[idxs1[idxs2[i]]] = xs[idxs1[idxs2[i]]] + 1 ;
        println (xs[idxs1[idxs2[i]]]) ;
        i = i + 1
    done
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
arrayIndexMayBeArrayIndex.s contents are:
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
14		# 3 element array
15		movl $16, %edi
16		call _malloc
17		movq %rax, %r11
18		# array pointers are shifted forwards by 4 bytes (to account for size)
19		movq %r11, %r11
20		addq $4, %r11
21		movq $3, %rax
22		movl %eax, -4(%r11)
23		movq $2, %rax
24		movl %eax, (%r11)
25		movq $0, %rax
26		movl %eax, 4(%r11)
27		movq $1, %rax
28		movl %eax, 8(%r11)
29		movq %r11, %rax
30		movq %rax, %r12
31		# 3 element array
32		movl $16, %edi
33		call _malloc
34		movq %rax, %r11
35		# array pointers are shifted forwards by 4 bytes (to account for size)
36		movq %r11, %r11
37		addq $4, %r11
38		movq $3, %rax
39		movl %eax, -4(%r11)
40		movq $1, %rax
41		movl %eax, (%r11)
42		movq $2, %rax
43		movl %eax, 4(%r11)
44		movq $0, %rax
45		movl %eax, 8(%r11)
46		movq %r11, %rax
47		movq %rax, %r13
48		# 3 element array
49		movl $16, %edi
50		call _malloc
51		movq %rax, %r11
52		# array pointers are shifted forwards by 4 bytes (to account for size)
53		movq %r11, %r11
54		addq $4, %r11
55		movq $3, %rax
56		movl %eax, -4(%r11)
57		movq $5, %rax
58		movl %eax, (%r11)
59		movq $6, %rax
60		movl %eax, 4(%r11)
61		movq $7, %rax
62		movl %eax, 8(%r11)
63		movq %r11, %rax
64		movq %rax, %r14
65		movq $0, %rax
66		movq %rax, %r15
67		jmp .L0
68	.L1:
69		movl %r15d, %r10d
70		movq %r13, %r9
71		call _arrLoad4
72		movl %r9d, %eax
73		movq %rax, %rax
74		pushq %rax
75		popq %r10
76		movl %r10d, %r10d
77		movq %r12, %r9
78		call _arrLoad4
79		movl %r9d, %eax
80		movq %rax, %rax
81		pushq %rax
82		popq %r10
83		movl %r10d, %r10d
84		movq %r14, %r9
85		call _arrLoad4
86		movl %r9d, %eax
87		movl %eax, %eax
88		addl $1, %eax
89		jo _errOverflow
90		movslq %eax, %rax
91		pushq %rax
92		movl %r15d, %r10d
93		movq %r13, %r9
94		call _arrLoad4
95		movl %r9d, %eax
96		movq %rax, %rax
97		pushq %rax
98		popq %r10
99		movl %r10d, %r10d
100		movq %r12, %r9
101		call _arrLoad4
102		movl %r9d, %eax
103		movq %rax, %rax
104		pushq %rax
105		popq %r10
106		movl %r10d, %r10d
107		popq %rax
108		movq %rax, %rax
109		movq %r14, %r9
110		call _arrStore4
111		# Stack pointer unchanged, no stack allocated arguments
112		movl %r15d, %r10d
113		movq %r13, %r9
114		call _arrLoad4
115		movl %r9d, %eax
116		movq %rax, %rax
117		pushq %rax
118		popq %r10
119		movl %r10d, %r10d
120		movq %r12, %r9
121		call _arrLoad4
122		movl %r9d, %eax
123		movq %rax, %rax
124		pushq %rax
125		popq %r10
126		movl %r10d, %r10d
127		movq %r14, %r9
128		call _arrLoad4
129		movl %r9d, %eax
130		movq %rax, %rax
131		movq %rax, %rdi
132		# statement primitives do not return results (but will clobber r0/rax)
133		call _printi
134		call _println
135		movl %r15d, %eax
136		addl $1, %eax
137		jo _errOverflow
138		movslq %eax, %rax
139		pushq %rax
140		popq %rax
141		movq %rax, %rax
142		movq %rax, %r15
143	.L0:
144		cmpq $3, %r15
145		jne .L1
146		# Stack pointer unchanged, no stack allocated variables
147		movq $0, %rax
148		# popq {%rbx, %r12, %r13, %r14, %r15}
149		movq (%rsp), %rbx
150		movq 8(%rsp), %r12
151		movq 16(%rsp), %r13
152		movq 24(%rsp), %r14
153		movq 32(%rsp), %r15
154		addq $40, %rsp
155		popq %rbp
156		ret
157	
158	.section .rodata
159	# length of .L._prints_str0
160		.int 4
161	.L._prints_str0:
162		.asciz "%.*s"
163	.text
164	_prints:
165		pushq %rbp
166		movq %rsp, %rbp
167		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
168		andq $-16, %rsp
169		movq %rdi, %rdx
170		movl -4(%rdi), %esi
171		leaq .L._prints_str0(%rip), %rdi
172		# on x86, al represents the number of SIMD registers used as variadic arguments
173		movb $0, %al
174		call printf@plt
175		movq $0, %rdi
176		call fflush@plt
177		movq %rbp, %rsp
178		popq %rbp
179		ret
180	
181	_malloc:
182		pushq %rbp
183		movq %rsp, %rbp
184		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
185		andq $-16, %rsp
186		call malloc@plt
187		cmpq $0, %rax
188		je _errOutOfMemory
189		movq %rbp, %rsp
190		popq %rbp
191		ret
192	
193	.section .rodata
194	# length of .L._printi_str0
195		.int 2
196	.L._printi_str0:
197		.asciz "%d"
198	.text
199	_printi:
200		pushq %rbp
201		movq %rsp, %rbp
202		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
203		andq $-16, %rsp
204		movl %edi, %esi
205		leaq .L._printi_str0(%rip), %rdi
206		# on x86, al represents the number of SIMD registers used as variadic arguments
207		movb $0, %al
208		call printf@plt
209		movq $0, %rdi
210		call fflush@plt
211		movq %rbp, %rsp
212		popq %rbp
213		ret
214	
215	.section .rodata
216	# length of .L._println_str0
217		.int 0
218	.L._println_str0:
219		.asciz ""
220	.text
221	_println:
222		pushq %rbp
223		movq %rsp, %rbp
224		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
225		andq $-16, %rsp
226		leaq .L._println_str0(%rip), %rdi
227		call puts@plt
228		movq $0, %rdi
229		call fflush@plt
230		movq %rbp, %rsp
231		popq %rbp
232		ret
233	
234	_exit:
235		pushq %rbp
236		movq %rsp, %rbp
237		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
238		andq $-16, %rsp
239		call exit@plt
240		movq %rbp, %rsp
241		popq %rbp
242		ret
243	
244	_arrStore4:
245		# Special calling convention: array ptr passed in R9, index in R10, value to store in RAX
246		pushq %rbx
247		cmpl $0, %r10d
248		cmovl %r10d, %esi
249		jl _errOutOfBounds
250		movl -4(%r9), %ebx
251		cmpl %ebx, %r10d
252		cmovge %r10d, %esi
253		jge _errOutOfBounds
254		movl %eax, (%r9,%r10,4)
255		popq %rbx
256		ret
257	
258	_arrLoad4:
259		# Special calling convention: array ptr passed in R9, index in R10, and return into R9
260		pushq %rbx
261		cmpl $0, %r10d
262		cmovl %r10d, %esi
263		jl _errOutOfBounds
264		movl -4(%r9), %ebx
265		cmpl %ebx, %r10d
266		cmovge %r10d, %esi
267		jge _errOutOfBounds
268		movslq (%r9,%r10,4), %r9
269		popq %rbx
270		ret
271	
272	.section .rodata
273	# length of .L._errOutOfMemory_str0
274		.int 27
275	.L._errOutOfMemory_str0:
276		.asciz "fatal error: out of memory\n"
277	.text
278	_errOutOfMemory:
279		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
280		andq $-16, %rsp
281		leaq .L._errOutOfMemory_str0(%rip), %rdi
282		call _prints
283		movb $-1, %dil
284		call exit@plt
285	
286	.section .rodata
287	# length of .L._errOutOfBounds_str0
288		.int 42
289	.L._errOutOfBounds_str0:
290		.asciz "fatal error: array index %d out of bounds\n"
291	.text
292	_errOutOfBounds:
293		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
294		andq $-16, %rsp
295		leaq .L._errOutOfBounds_str0(%rip), %rdi
296		# on x86, al represents the number of SIMD registers used as variadic arguments
297		movb $0, %al
298		call printf@plt
299		movq $0, %rdi
300		call fflush@plt
301		movb $-1, %dil
302		call exit@plt
303	
304	.section .rodata
305	# length of .L._errOverflow_str0
306		.int 52
307	.L._errOverflow_str0:
308		.asciz "fatal error: integer overflow or underflow occurred\n"
309	.text
310	_errOverflow:
311		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
312		andq $-16, %rsp
313		leaq .L._errOverflow_str0(%rip), %rdi
314		call _prints
315		movb $-1, %dil
316		call exit@plt
===========================================================
-- Finished

