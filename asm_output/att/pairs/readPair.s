./valid/pairs/readPair.wacc
calling the reference compiler on ./valid/pairs/readPair.wacc
-- Test: readPair.wacc

-- Uploaded file: 
---------------------------------------------------------------
# construct a pair from supplied user input

# Input: f 16

# Output:
# Please enter the first element (char): Please enter the second element (int): The first element was f
# The second element was 16
#

# Program:

begin
	pair(char, int) p = newpair('\0', 0) ;
	print "Please enter the first element (char): " ;
  	char c = '0';
	read c ;
  	fst p = c ;
	print "Please enter the second element (int): " ;
	int i = 0 ;
	read i ;
	snd p = i ;
	# Clear the value for c and i
	c = '\0' ;
	i = -1 ;
	print "The first element was " ;
	c = fst p ;
	println c ;
	print "The second element was " ;
	i = snd p ;
	println i
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
readPair.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 39
4	.L.str0:
5		.asciz "Please enter the first element (char): "
6	# length of .L.str1
7		.int 39
8	.L.str1:
9		.asciz "Please enter the second element (int): "
10	# length of .L.str2
11		.int 22
12	.L.str2:
13		.asciz "The first element was "
14	# length of .L.str3
15		.int 23
16	.L.str3:
17		.asciz "The second element was "
18	.text
19	main:
20		pushq %rbp
21		# pushq {%rbx, %r12, %r13, %r14}
22		subq $32, %rsp
23		movq %rbx, (%rsp)
24		movq %r12, 8(%rsp)
25		movq %r13, 16(%rsp)
26		movq %r14, 24(%rsp)
27		movq %rsp, %rbp
28		# Stack pointer unchanged, no stack allocated variables
29		movl $16, %edi
30		call _malloc
31		movq %rax, %r11
32		movq $0, %rax
33		movq %rax, (%r11)
34		movq $0, %rax
35		movq %rax, 8(%r11)
36		movq %r11, %rax
37		movq %rax, %r12
38		# Stack pointer unchanged, no stack allocated arguments
39		leaq .L.str0(%rip), %rax
40		pushq %rax
41		popq %rax
42		movq %rax, %rax
43		movq %rax, %rdi
44		# statement primitives do not return results (but will clobber r0/rax)
45		call _prints
46		movq $48, %rax
47		movq %rax, %r13
48		# Stack pointer unchanged, no stack allocated arguments
49		# load the current value in the destination of the read so it supports defaults
50		movq %r13, %rax
51		movq %rax, %rdi
52		call _readc
53		movq %rax, %r11
54		movq %r11, %rax
55		movq %rax, %r13
56		cmpq $0, %r12
57		je _errNull
58		movq %r13, %rax
59		movq %rax, (%r12)
60		# Stack pointer unchanged, no stack allocated arguments
61		leaq .L.str1(%rip), %rax
62		pushq %rax
63		popq %rax
64		movq %rax, %rax
65		movq %rax, %rdi
66		# statement primitives do not return results (but will clobber r0/rax)
67		call _prints
68		movq $0, %rax
69		movq %rax, %r14
70		# Stack pointer unchanged, no stack allocated arguments
71		# load the current value in the destination of the read so it supports defaults
72		movq %r14, %rax
73		movq %rax, %rdi
74		call _readi
75		movq %rax, %r11
76		movq %r11, %rax
77		movq %rax, %r14
78		cmpq $0, %r12
79		je _errNull
80		movq %r14, %rax
81		movq %rax, 8(%r12)
82		movq $0, %rax
83		movq %rax, %r13
84		movq $-1, %rax
85		movq %rax, %r14
86		# Stack pointer unchanged, no stack allocated arguments
87		leaq .L.str2(%rip), %rax
88		pushq %rax
89		popq %rax
90		movq %rax, %rax
91		movq %rax, %rdi
92		# statement primitives do not return results (but will clobber r0/rax)
93		call _prints
94		cmpq $0, %r12
95		je _errNull
96		movq (%r12), %rax
97		movq %rax, %rax
98		pushq %rax
99		popq %rax
100		movq %rax, %rax
101		movq %rax, %r13
102		# Stack pointer unchanged, no stack allocated arguments
103		movq %r13, %rax
104		movq %rax, %rdi
105		# statement primitives do not return results (but will clobber r0/rax)
106		call _printc
107		call _println
108		# Stack pointer unchanged, no stack allocated arguments
109		leaq .L.str3(%rip), %rax
110		pushq %rax
111		popq %rax
112		movq %rax, %rax
113		movq %rax, %rdi
114		# statement primitives do not return results (but will clobber r0/rax)
115		call _prints
116		cmpq $0, %r12
117		je _errNull
118		movq 8(%r12), %rax
119		movq %rax, %rax
120		pushq %rax
121		popq %rax
122		movq %rax, %rax
123		movq %rax, %r14
124		# Stack pointer unchanged, no stack allocated arguments
125		movq %r14, %rax
126		movq %rax, %rdi
127		# statement primitives do not return results (but will clobber r0/rax)
128		call _printi
129		call _println
130		# Stack pointer unchanged, no stack allocated variables
131		movq $0, %rax
132		# popq {%rbx, %r12, %r13, %r14}
133		movq (%rsp), %rbx
134		movq 8(%rsp), %r12
135		movq 16(%rsp), %r13
136		movq 24(%rsp), %r14
137		addq $32, %rsp
138		popq %rbp
139		ret
140	
141	.section .rodata
142	# length of .L._readc_str0
143		.int 3
144	.L._readc_str0:
145		.asciz " %c"
146	.text
147	_readc:
148		pushq %rbp
149		movq %rsp, %rbp
150		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
151		andq $-16, %rsp
152		# RDI contains the "original" value of the destination of the read
153		# allocate space on the stack to store the read: preserve alignment!
154		# the passed default argument should be stored in case of EOF
155		subq $16, %rsp
156		movb %dil, (%rsp)
157		leaq (%rsp), %rsi
158		leaq .L._readc_str0(%rip), %rdi
159		# on x86, al represents the number of SIMD registers used as variadic arguments
160		movb $0, %al
161		call scanf@plt
162		movsbq (%rsp), %rax
163		addq $16, %rsp
164		movq %rbp, %rsp
165		popq %rbp
166		ret
167	
168	.section .rodata
169	# length of .L._prints_str0
170		.int 4
171	.L._prints_str0:
172		.asciz "%.*s"
173	.text
174	_prints:
175		pushq %rbp
176		movq %rsp, %rbp
177		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
178		andq $-16, %rsp
179		movq %rdi, %rdx
180		movl -4(%rdi), %esi
181		leaq .L._prints_str0(%rip), %rdi
182		# on x86, al represents the number of SIMD registers used as variadic arguments
183		movb $0, %al
184		call printf@plt
185		movq $0, %rdi
186		call fflush@plt
187		movq %rbp, %rsp
188		popq %rbp
189		ret
190	
191	.section .rodata
192	# length of .L._readi_str0
193		.int 2
194	.L._readi_str0:
195		.asciz "%d"
196	.text
197	_readi:
198		pushq %rbp
199		movq %rsp, %rbp
200		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
201		andq $-16, %rsp
202		# RDI contains the "original" value of the destination of the read
203		# allocate space on the stack to store the read: preserve alignment!
204		# the passed default argument should be stored in case of EOF
205		subq $16, %rsp
206		movl %edi, (%rsp)
207		leaq (%rsp), %rsi
208		leaq .L._readi_str0(%rip), %rdi
209		# on x86, al represents the number of SIMD registers used as variadic arguments
210		movb $0, %al
211		call scanf@plt
212		movslq (%rsp), %rax
213		addq $16, %rsp
214		movq %rbp, %rsp
215		popq %rbp
216		ret
217	
218	_malloc:
219		pushq %rbp
220		movq %rsp, %rbp
221		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
222		andq $-16, %rsp
223		call malloc@plt
224		cmpq $0, %rax
225		je _errOutOfMemory
226		movq %rbp, %rsp
227		popq %rbp
228		ret
229	
230	.section .rodata
231	# length of .L._printi_str0
232		.int 2
233	.L._printi_str0:
234		.asciz "%d"
235	.text
236	_printi:
237		pushq %rbp
238		movq %rsp, %rbp
239		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
240		andq $-16, %rsp
241		movl %edi, %esi
242		leaq .L._printi_str0(%rip), %rdi
243		# on x86, al represents the number of SIMD registers used as variadic arguments
244		movb $0, %al
245		call printf@plt
246		movq $0, %rdi
247		call fflush@plt
248		movq %rbp, %rsp
249		popq %rbp
250		ret
251	
252	.section .rodata
253	# length of .L._printc_str0
254		.int 2
255	.L._printc_str0:
256		.asciz "%c"
257	.text
258	_printc:
259		pushq %rbp
260		movq %rsp, %rbp
261		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
262		andq $-16, %rsp
263		movb %dil, %sil
264		leaq .L._printc_str0(%rip), %rdi
265		# on x86, al represents the number of SIMD registers used as variadic arguments
266		movb $0, %al
267		call printf@plt
268		movq $0, %rdi
269		call fflush@plt
270		movq %rbp, %rsp
271		popq %rbp
272		ret
273	
274	.section .rodata
275	# length of .L._println_str0
276		.int 0
277	.L._println_str0:
278		.asciz ""
279	.text
280	_println:
281		pushq %rbp
282		movq %rsp, %rbp
283		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
284		andq $-16, %rsp
285		leaq .L._println_str0(%rip), %rdi
286		call puts@plt
287		movq $0, %rdi
288		call fflush@plt
289		movq %rbp, %rsp
290		popq %rbp
291		ret
292	
293	_exit:
294		pushq %rbp
295		movq %rsp, %rbp
296		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
297		andq $-16, %rsp
298		call exit@plt
299		movq %rbp, %rsp
300		popq %rbp
301		ret
302	
303	.section .rodata
304	# length of .L._errOutOfMemory_str0
305		.int 27
306	.L._errOutOfMemory_str0:
307		.asciz "fatal error: out of memory\n"
308	.text
309	_errOutOfMemory:
310		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
311		andq $-16, %rsp
312		leaq .L._errOutOfMemory_str0(%rip), %rdi
313		call _prints
314		movb $-1, %dil
315		call exit@plt
316	
317	.section .rodata
318	# length of .L._errNull_str0
319		.int 45
320	.L._errNull_str0:
321		.asciz "fatal error: null pair dereferenced or freed\n"
322	.text
323	_errNull:
324		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
325		andq $-16, %rsp
326		leaq .L._errNull_str0(%rip), %rdi
327		call _prints
328		movb $-1, %dil
329		call exit@plt
===========================================================
-- Finished

