./valid/pairs/linkedList.wacc
calling the reference compiler on ./valid/pairs/linkedList.wacc
-- Test: linkedList.wacc

-- Uploaded file: 
---------------------------------------------------------------
# create and print a linked list using pairs

# Output:
# list = {1, 2, 4, 11}
#

# Program:

begin
  pair(int, pair) p = newpair(11, null) ;
    pair(int, pair) q = newpair(4, p) ;
    pair(int, pair) r = newpair(2, q) ;
    pair(int, pair) s = newpair(1, r) ;
    print "list = {" ;
    pair(int, pair) x = s ;
    pair(int, pair) y = snd x ;
    int f = 0;
    while y != null do
      f = fst x ;
      print f ;
      print ", " ;
      x = y ;
      y = snd x
    done ;
    f = fst x ;
    print f ;
    println "}"
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
linkedList.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 8
4	.L.str0:
5		.asciz "list = {"
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
17		# pushq {%rbx, %r12, %r13, %r14, %r15}
18		subq $40, %rsp
19		movq %rbx, (%rsp)
20		movq %r12, 8(%rsp)
21		movq %r13, 16(%rsp)
22		movq %r14, 24(%rsp)
23		movq %r15, 32(%rsp)
24		movq %rsp, %rbp
25		# Stack pointer unchanged, no stack allocated variables
26		# pushq {%rcx, %rdx, %rsi}
27		subq $24, %rsp
28		movq %rcx, (%rsp)
29		movq %rdx, 8(%rsp)
30		movq %rsi, 16(%rsp)
31		movl $16, %edi
32		call _malloc
33		movq %rax, %r11
34		# popq {%rcx, %rdx, %rsi}
35		movq (%rsp), %rcx
36		movq 8(%rsp), %rdx
37		movq 16(%rsp), %rsi
38		addq $24, %rsp
39		movq $11, %rax
40		movq %rax, (%r11)
41		movq $0, %rax
42		movq %rax, 8(%r11)
43		movq %r11, %rax
44		movq %rax, %r12
45		# pushq {%rcx, %rdx, %rsi}
46		subq $24, %rsp
47		movq %rcx, (%rsp)
48		movq %rdx, 8(%rsp)
49		movq %rsi, 16(%rsp)
50		movl $16, %edi
51		call _malloc
52		movq %rax, %r11
53		# popq {%rcx, %rdx, %rsi}
54		movq (%rsp), %rcx
55		movq 8(%rsp), %rdx
56		movq 16(%rsp), %rsi
57		addq $24, %rsp
58		movq $4, %rax
59		movq %rax, (%r11)
60		movq %r12, %rax
61		movq %rax, 8(%r11)
62		movq %r11, %rax
63		movq %rax, %r13
64		# pushq {%rcx, %rdx, %rsi}
65		subq $24, %rsp
66		movq %rcx, (%rsp)
67		movq %rdx, 8(%rsp)
68		movq %rsi, 16(%rsp)
69		movl $16, %edi
70		call _malloc
71		movq %rax, %r11
72		# popq {%rcx, %rdx, %rsi}
73		movq (%rsp), %rcx
74		movq 8(%rsp), %rdx
75		movq 16(%rsp), %rsi
76		addq $24, %rsp
77		movq $2, %rax
78		movq %rax, (%r11)
79		movq %r13, %rax
80		movq %rax, 8(%r11)
81		movq %r11, %rax
82		movq %rax, %r14
83		# pushq {%rcx, %rdx, %rsi}
84		subq $24, %rsp
85		movq %rcx, (%rsp)
86		movq %rdx, 8(%rsp)
87		movq %rsi, 16(%rsp)
88		movl $16, %edi
89		call _malloc
90		movq %rax, %r11
91		# popq {%rcx, %rdx, %rsi}
92		movq (%rsp), %rcx
93		movq 8(%rsp), %rdx
94		movq 16(%rsp), %rsi
95		addq $24, %rsp
96		movq $1, %rax
97		movq %rax, (%r11)
98		movq %r14, %rax
99		movq %rax, 8(%r11)
100		movq %r11, %rax
101		movq %rax, %r15
102		# pushq {%rcx, %rdx, %rsi}
103		subq $24, %rsp
104		movq %rcx, (%rsp)
105		movq %rdx, 8(%rsp)
106		movq %rsi, 16(%rsp)
107		# Set up R11 as a temporary second base pointer for the caller saved things
108		movq %rsp, %r11
109		# Stack pointer unchanged, no stack allocated arguments
110		leaq .L.str0(%rip), %rax
111		pushq %rax
112		popq %rax
113		movq %rax, %rax
114		movq %rax, %rdi
115		# statement primitives do not return results (but will clobber r0/rax)
116		call _prints
117		# popq {%rcx, %rdx, %rsi}
118		movq (%rsp), %rcx
119		movq 8(%rsp), %rdx
120		movq 16(%rsp), %rsi
121		addq $24, %rsp
122		movq %r15, %rax
123		movq %rax, %rcx
124		cmpq $0, %rcx
125		je _errNull
126		movq 8(%rcx), %rax
127		movq %rax, %rax
128		movq %rax, %rdx
129		movq $0, %rax
130		movq %rax, %rsi
131		jmp .L0
132	.L1:
133		cmpq $0, %rcx
134		je _errNull
135		movq (%rcx), %rax
136		movq %rax, %rax
137		pushq %rax
138		popq %rax
139		movq %rax, %rax
140		movq %rax, %rsi
141		# pushq {%rcx, %rdx, %rsi}
142		subq $24, %rsp
143		movq %rcx, (%rsp)
144		movq %rdx, 8(%rsp)
145		movq %rsi, 16(%rsp)
146		# Set up R11 as a temporary second base pointer for the caller saved things
147		movq %rsp, %r11
148		# Stack pointer unchanged, no stack allocated arguments
149		movq %rsi, %rax
150		movq %rax, %rdi
151		# statement primitives do not return results (but will clobber r0/rax)
152		call _printi
153		# popq {%rcx, %rdx, %rsi}
154		movq (%rsp), %rcx
155		movq 8(%rsp), %rdx
156		movq 16(%rsp), %rsi
157		addq $24, %rsp
158		# pushq {%rcx, %rdx, %rsi}
159		subq $24, %rsp
160		movq %rcx, (%rsp)
161		movq %rdx, 8(%rsp)
162		movq %rsi, 16(%rsp)
163		# Set up R11 as a temporary second base pointer for the caller saved things
164		movq %rsp, %r11
165		# Stack pointer unchanged, no stack allocated arguments
166		leaq .L.str1(%rip), %rax
167		pushq %rax
168		popq %rax
169		movq %rax, %rax
170		movq %rax, %rdi
171		# statement primitives do not return results (but will clobber r0/rax)
172		call _prints
173		# popq {%rcx, %rdx, %rsi}
174		movq (%rsp), %rcx
175		movq 8(%rsp), %rdx
176		movq 16(%rsp), %rsi
177		addq $24, %rsp
178		movq %rdx, %rax
179		movq %rax, %rcx
180		cmpq $0, %rcx
181		je _errNull
182		movq 8(%rcx), %rax
183		movq %rax, %rax
184		pushq %rax
185		popq %rax
186		movq %rax, %rax
187		movq %rax, %rdx
188	.L0:
189		cmpq $0, %rdx
190		jne .L1
191		cmpq $0, %rcx
192		je _errNull
193		movq (%rcx), %rax
194		movq %rax, %rax
195		pushq %rax
196		popq %rax
197		movq %rax, %rax
198		movq %rax, %rsi
199		# pushq {%rcx, %rdx, %rsi}
200		subq $24, %rsp
201		movq %rcx, (%rsp)
202		movq %rdx, 8(%rsp)
203		movq %rsi, 16(%rsp)
204		# Set up R11 as a temporary second base pointer for the caller saved things
205		movq %rsp, %r11
206		# Stack pointer unchanged, no stack allocated arguments
207		movq %rsi, %rax
208		movq %rax, %rdi
209		# statement primitives do not return results (but will clobber r0/rax)
210		call _printi
211		# popq {%rcx, %rdx, %rsi}
212		movq (%rsp), %rcx
213		movq 8(%rsp), %rdx
214		movq 16(%rsp), %rsi
215		addq $24, %rsp
216		# pushq {%rcx, %rdx, %rsi}
217		subq $24, %rsp
218		movq %rcx, (%rsp)
219		movq %rdx, 8(%rsp)
220		movq %rsi, 16(%rsp)
221		# Set up R11 as a temporary second base pointer for the caller saved things
222		movq %rsp, %r11
223		# Stack pointer unchanged, no stack allocated arguments
224		leaq .L.str2(%rip), %rax
225		pushq %rax
226		popq %rax
227		movq %rax, %rax
228		movq %rax, %rdi
229		# statement primitives do not return results (but will clobber r0/rax)
230		call _prints
231		call _println
232		# popq {%rcx, %rdx, %rsi}
233		movq (%rsp), %rcx
234		movq 8(%rsp), %rdx
235		movq 16(%rsp), %rsi
236		addq $24, %rsp
237		# Stack pointer unchanged, no stack allocated variables
238		movq $0, %rax
239		# popq {%rbx, %r12, %r13, %r14, %r15}
240		movq (%rsp), %rbx
241		movq 8(%rsp), %r12
242		movq 16(%rsp), %r13
243		movq 24(%rsp), %r14
244		movq 32(%rsp), %r15
245		addq $40, %rsp
246		popq %rbp
247		ret
248	
249	.section .rodata
250	# length of .L._prints_str0
251		.int 4
252	.L._prints_str0:
253		.asciz "%.*s"
254	.text
255	_prints:
256		pushq %rbp
257		movq %rsp, %rbp
258		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
259		andq $-16, %rsp
260		movq %rdi, %rdx
261		movl -4(%rdi), %esi
262		leaq .L._prints_str0(%rip), %rdi
263		# on x86, al represents the number of SIMD registers used as variadic arguments
264		movb $0, %al
265		call printf@plt
266		movq $0, %rdi
267		call fflush@plt
268		movq %rbp, %rsp
269		popq %rbp
270		ret
271	
272	_malloc:
273		pushq %rbp
274		movq %rsp, %rbp
275		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
276		andq $-16, %rsp
277		call malloc@plt
278		cmpq $0, %rax
279		je _errOutOfMemory
280		movq %rbp, %rsp
281		popq %rbp
282		ret
283	
284	.section .rodata
285	# length of .L._printi_str0
286		.int 2
287	.L._printi_str0:
288		.asciz "%d"
289	.text
290	_printi:
291		pushq %rbp
292		movq %rsp, %rbp
293		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
294		andq $-16, %rsp
295		movl %edi, %esi
296		leaq .L._printi_str0(%rip), %rdi
297		# on x86, al represents the number of SIMD registers used as variadic arguments
298		movb $0, %al
299		call printf@plt
300		movq $0, %rdi
301		call fflush@plt
302		movq %rbp, %rsp
303		popq %rbp
304		ret
305	
306	.section .rodata
307	# length of .L._println_str0
308		.int 0
309	.L._println_str0:
310		.asciz ""
311	.text
312	_println:
313		pushq %rbp
314		movq %rsp, %rbp
315		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
316		andq $-16, %rsp
317		leaq .L._println_str0(%rip), %rdi
318		call puts@plt
319		movq $0, %rdi
320		call fflush@plt
321		movq %rbp, %rsp
322		popq %rbp
323		ret
324	
325	_exit:
326		pushq %rbp
327		movq %rsp, %rbp
328		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
329		andq $-16, %rsp
330		call exit@plt
331		movq %rbp, %rsp
332		popq %rbp
333		ret
334	
335	.section .rodata
336	# length of .L._errOutOfMemory_str0
337		.int 27
338	.L._errOutOfMemory_str0:
339		.asciz "fatal error: out of memory\n"
340	.text
341	_errOutOfMemory:
342		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
343		andq $-16, %rsp
344		leaq .L._errOutOfMemory_str0(%rip), %rdi
345		call _prints
346		movb $-1, %dil
347		call exit@plt
348	
349	.section .rodata
350	# length of .L._errNull_str0
351		.int 45
352	.L._errNull_str0:
353		.asciz "fatal error: null pair dereferenced or freed\n"
354	.text
355	_errNull:
356		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
357		andq $-16, %rsp
358		leaq .L._errNull_str0(%rip), %rdi
359		call _prints
360		movb $-1, %dil
361		call exit@plt
===========================================================
-- Finished

