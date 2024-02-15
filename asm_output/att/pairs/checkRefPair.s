./valid/pairs/checkRefPair.wacc
calling the reference compiler on ./valid/pairs/checkRefPair.wacc
-- Test: checkRefPair.wacc

-- Uploaded file: 
---------------------------------------------------------------
# create a pair(int, char) with a second reference to it and check it works

# Output:
# #addrs#
# #addrs#
# true
# 10
# 10
# true
# a
# a
# true
#

# Program:

begin
  pair(int, char) p = newpair(10, 'a') ;
  pair(int, char) q = p;
  println p ;
  println q ;
  println p == q ;
  int x = fst p ;
  int y = fst q ;
  println x ;
  println y ;
  println x == y ;
  char c1 = snd p ;
  char c2 = snd q ;
  println c1 ;
  println c2 ;
  println c1 == c2
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
checkRefPair.s contents are:
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
14		# pushq {%rcx, %rdx}
15		subq $16, %rsp
16		movq %rcx, (%rsp)
17		movq %rdx, 8(%rsp)
18		movl $16, %edi
19		call _malloc
20		movq %rax, %r11
21		# popq {%rcx, %rdx}
22		movq (%rsp), %rcx
23		movq 8(%rsp), %rdx
24		addq $16, %rsp
25		movq $10, %rax
26		movq %rax, (%r11)
27		movq $97, %rax
28		movq %rax, 8(%r11)
29		movq %r11, %rax
30		movq %rax, %r12
31		movq %r12, %rax
32		movq %rax, %r13
33		# pushq {%rcx, %rdx}
34		subq $16, %rsp
35		movq %rcx, (%rsp)
36		movq %rdx, 8(%rsp)
37		# Set up R11 as a temporary second base pointer for the caller saved things
38		movq %rsp, %r11
39		# Stack pointer unchanged, no stack allocated arguments
40		movq %r12, %rax
41		movq %rax, %rdi
42		# statement primitives do not return results (but will clobber r0/rax)
43		call _printp
44		call _println
45		# popq {%rcx, %rdx}
46		movq (%rsp), %rcx
47		movq 8(%rsp), %rdx
48		addq $16, %rsp
49		# pushq {%rcx, %rdx}
50		subq $16, %rsp
51		movq %rcx, (%rsp)
52		movq %rdx, 8(%rsp)
53		# Set up R11 as a temporary second base pointer for the caller saved things
54		movq %rsp, %r11
55		# Stack pointer unchanged, no stack allocated arguments
56		movq %r13, %rax
57		movq %rax, %rdi
58		# statement primitives do not return results (but will clobber r0/rax)
59		call _printp
60		call _println
61		# popq {%rcx, %rdx}
62		movq (%rsp), %rcx
63		movq 8(%rsp), %rdx
64		addq $16, %rsp
65		# pushq {%rcx, %rdx}
66		subq $16, %rsp
67		movq %rcx, (%rsp)
68		movq %rdx, 8(%rsp)
69		# Set up R11 as a temporary second base pointer for the caller saved things
70		movq %rsp, %r11
71		# Stack pointer unchanged, no stack allocated arguments
72		cmpq %r13, %r12
73		sete %al
74		movsbq %al, %rax
75		pushq %rax
76		popq %rax
77		movq %rax, %rax
78		movq %rax, %rdi
79		# statement primitives do not return results (but will clobber r0/rax)
80		call _printb
81		call _println
82		# popq {%rcx, %rdx}
83		movq (%rsp), %rcx
84		movq 8(%rsp), %rdx
85		addq $16, %rsp
86		cmpq $0, %r12
87		je _errNull
88		movq (%r12), %rax
89		movq %rax, %rax
90		movq %rax, %r14
91		cmpq $0, %r13
92		je _errNull
93		movq (%r13), %rax
94		movq %rax, %rax
95		movq %rax, %r15
96		# pushq {%rcx, %rdx}
97		subq $16, %rsp
98		movq %rcx, (%rsp)
99		movq %rdx, 8(%rsp)
100		# Set up R11 as a temporary second base pointer for the caller saved things
101		movq %rsp, %r11
102		# Stack pointer unchanged, no stack allocated arguments
103		movq %r14, %rax
104		movq %rax, %rdi
105		# statement primitives do not return results (but will clobber r0/rax)
106		call _printi
107		call _println
108		# popq {%rcx, %rdx}
109		movq (%rsp), %rcx
110		movq 8(%rsp), %rdx
111		addq $16, %rsp
112		# pushq {%rcx, %rdx}
113		subq $16, %rsp
114		movq %rcx, (%rsp)
115		movq %rdx, 8(%rsp)
116		# Set up R11 as a temporary second base pointer for the caller saved things
117		movq %rsp, %r11
118		# Stack pointer unchanged, no stack allocated arguments
119		movq %r15, %rax
120		movq %rax, %rdi
121		# statement primitives do not return results (but will clobber r0/rax)
122		call _printi
123		call _println
124		# popq {%rcx, %rdx}
125		movq (%rsp), %rcx
126		movq 8(%rsp), %rdx
127		addq $16, %rsp
128		# pushq {%rcx, %rdx}
129		subq $16, %rsp
130		movq %rcx, (%rsp)
131		movq %rdx, 8(%rsp)
132		# Set up R11 as a temporary second base pointer for the caller saved things
133		movq %rsp, %r11
134		# Stack pointer unchanged, no stack allocated arguments
135		cmpq %r15, %r14
136		sete %al
137		movsbq %al, %rax
138		pushq %rax
139		popq %rax
140		movq %rax, %rax
141		movq %rax, %rdi
142		# statement primitives do not return results (but will clobber r0/rax)
143		call _printb
144		call _println
145		# popq {%rcx, %rdx}
146		movq (%rsp), %rcx
147		movq 8(%rsp), %rdx
148		addq $16, %rsp
149		cmpq $0, %r12
150		je _errNull
151		movq 8(%r12), %rax
152		movq %rax, %rax
153		movq %rax, %rcx
154		cmpq $0, %r13
155		je _errNull
156		movq 8(%r13), %rax
157		movq %rax, %rax
158		movq %rax, %rdx
159		# pushq {%rcx, %rdx}
160		subq $16, %rsp
161		movq %rcx, (%rsp)
162		movq %rdx, 8(%rsp)
163		# Set up R11 as a temporary second base pointer for the caller saved things
164		movq %rsp, %r11
165		# Stack pointer unchanged, no stack allocated arguments
166		movq %rcx, %rax
167		movq %rax, %rdi
168		# statement primitives do not return results (but will clobber r0/rax)
169		call _printc
170		call _println
171		# popq {%rcx, %rdx}
172		movq (%rsp), %rcx
173		movq 8(%rsp), %rdx
174		addq $16, %rsp
175		# pushq {%rcx, %rdx}
176		subq $16, %rsp
177		movq %rcx, (%rsp)
178		movq %rdx, 8(%rsp)
179		# Set up R11 as a temporary second base pointer for the caller saved things
180		movq %rsp, %r11
181		# Stack pointer unchanged, no stack allocated arguments
182		movq %rdx, %rax
183		movq %rax, %rdi
184		# statement primitives do not return results (but will clobber r0/rax)
185		call _printc
186		call _println
187		# popq {%rcx, %rdx}
188		movq (%rsp), %rcx
189		movq 8(%rsp), %rdx
190		addq $16, %rsp
191		# pushq {%rcx, %rdx}
192		subq $16, %rsp
193		movq %rcx, (%rsp)
194		movq %rdx, 8(%rsp)
195		# Set up R11 as a temporary second base pointer for the caller saved things
196		movq %rsp, %r11
197		# Stack pointer unchanged, no stack allocated arguments
198		cmpq %rdx, %rcx
199		sete %al
200		movsbq %al, %rax
201		pushq %rax
202		popq %rax
203		movq %rax, %rax
204		movq %rax, %rdi
205		# statement primitives do not return results (but will clobber r0/rax)
206		call _printb
207		call _println
208		# popq {%rcx, %rdx}
209		movq (%rsp), %rcx
210		movq 8(%rsp), %rdx
211		addq $16, %rsp
212		# Stack pointer unchanged, no stack allocated variables
213		movq $0, %rax
214		# popq {%rbx, %r12, %r13, %r14, %r15}
215		movq (%rsp), %rbx
216		movq 8(%rsp), %r12
217		movq 16(%rsp), %r13
218		movq 24(%rsp), %r14
219		movq 32(%rsp), %r15
220		addq $40, %rsp
221		popq %rbp
222		ret
223	
224	.section .rodata
225	# length of .L._prints_str0
226		.int 4
227	.L._prints_str0:
228		.asciz "%.*s"
229	.text
230	_prints:
231		pushq %rbp
232		movq %rsp, %rbp
233		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
234		andq $-16, %rsp
235		movq %rdi, %rdx
236		movl -4(%rdi), %esi
237		leaq .L._prints_str0(%rip), %rdi
238		# on x86, al represents the number of SIMD registers used as variadic arguments
239		movb $0, %al
240		call printf@plt
241		movq $0, %rdi
242		call fflush@plt
243		movq %rbp, %rsp
244		popq %rbp
245		ret
246	
247	.section .rodata
248	# length of .L._printp_str0
249		.int 2
250	.L._printp_str0:
251		.asciz "%p"
252	.text
253	_printp:
254		pushq %rbp
255		movq %rsp, %rbp
256		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
257		andq $-16, %rsp
258		movq %rdi, %rsi
259		leaq .L._printp_str0(%rip), %rdi
260		# on x86, al represents the number of SIMD registers used as variadic arguments
261		movb $0, %al
262		call printf@plt
263		movq $0, %rdi
264		call fflush@plt
265		movq %rbp, %rsp
266		popq %rbp
267		ret
268	
269	_malloc:
270		pushq %rbp
271		movq %rsp, %rbp
272		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
273		andq $-16, %rsp
274		call malloc@plt
275		cmpq $0, %rax
276		je _errOutOfMemory
277		movq %rbp, %rsp
278		popq %rbp
279		ret
280	
281	.section .rodata
282	# length of .L._printi_str0
283		.int 2
284	.L._printi_str0:
285		.asciz "%d"
286	.text
287	_printi:
288		pushq %rbp
289		movq %rsp, %rbp
290		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
291		andq $-16, %rsp
292		movl %edi, %esi
293		leaq .L._printi_str0(%rip), %rdi
294		# on x86, al represents the number of SIMD registers used as variadic arguments
295		movb $0, %al
296		call printf@plt
297		movq $0, %rdi
298		call fflush@plt
299		movq %rbp, %rsp
300		popq %rbp
301		ret
302	
303	.section .rodata
304	# length of .L._printb_str0
305		.int 5
306	.L._printb_str0:
307		.asciz "false"
308	# length of .L._printb_str1
309		.int 4
310	.L._printb_str1:
311		.asciz "true"
312	# length of .L._printb_str2
313		.int 4
314	.L._printb_str2:
315		.asciz "%.*s"
316	.text
317	_printb:
318		pushq %rbp
319		movq %rsp, %rbp
320		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
321		andq $-16, %rsp
322		cmpb $0, %dil
323		jne .L_printb0
324		leaq .L._printb_str0(%rip), %rdx
325		jmp .L_printb1
326	.L_printb0:
327		leaq .L._printb_str1(%rip), %rdx
328	.L_printb1:
329		movl -4(%rdx), %esi
330		leaq .L._printb_str2(%rip), %rdi
331		# on x86, al represents the number of SIMD registers used as variadic arguments
332		movb $0, %al
333		call printf@plt
334		movq $0, %rdi
335		call fflush@plt
336		movq %rbp, %rsp
337		popq %rbp
338		ret
339	
340	.section .rodata
341	# length of .L._printc_str0
342		.int 2
343	.L._printc_str0:
344		.asciz "%c"
345	.text
346	_printc:
347		pushq %rbp
348		movq %rsp, %rbp
349		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
350		andq $-16, %rsp
351		movb %dil, %sil
352		leaq .L._printc_str0(%rip), %rdi
353		# on x86, al represents the number of SIMD registers used as variadic arguments
354		movb $0, %al
355		call printf@plt
356		movq $0, %rdi
357		call fflush@plt
358		movq %rbp, %rsp
359		popq %rbp
360		ret
361	
362	.section .rodata
363	# length of .L._println_str0
364		.int 0
365	.L._println_str0:
366		.asciz ""
367	.text
368	_println:
369		pushq %rbp
370		movq %rsp, %rbp
371		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
372		andq $-16, %rsp
373		leaq .L._println_str0(%rip), %rdi
374		call puts@plt
375		movq $0, %rdi
376		call fflush@plt
377		movq %rbp, %rsp
378		popq %rbp
379		ret
380	
381	_exit:
382		pushq %rbp
383		movq %rsp, %rbp
384		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
385		andq $-16, %rsp
386		call exit@plt
387		movq %rbp, %rsp
388		popq %rbp
389		ret
390	
391	.section .rodata
392	# length of .L._errOutOfMemory_str0
393		.int 27
394	.L._errOutOfMemory_str0:
395		.asciz "fatal error: out of memory\n"
396	.text
397	_errOutOfMemory:
398		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
399		andq $-16, %rsp
400		leaq .L._errOutOfMemory_str0(%rip), %rdi
401		call _prints
402		movb $-1, %dil
403		call exit@plt
404	
405	.section .rodata
406	# length of .L._errNull_str0
407		.int 45
408	.L._errNull_str0:
409		.asciz "fatal error: null pair dereferenced or freed\n"
410	.text
411	_errNull:
412		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
413		andq $-16, %rsp
414		leaq .L._errNull_str0(%rip), %rdi
415		call _prints
416		movb $-1, %dil
417		call exit@plt
===========================================================
-- Finished

