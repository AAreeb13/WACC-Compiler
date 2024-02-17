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
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	.text
4	main:
5		push rbp
6		# push {rbx, r12, r13, r14, r15}
7		sub rsp, 40
8		mov qword ptr [rsp], rbx
9		mov qword ptr [rsp + 8], r12
10		mov qword ptr [rsp + 16], r13
11		mov qword ptr [rsp + 24], r14
12		mov qword ptr [rsp + 32], r15
13		mov rbp, rsp
14		# Stack pointer unchanged, no stack allocated variables
15		# push {rcx, rdx}
16		sub rsp, 16
17		mov qword ptr [rsp], rcx
18		mov qword ptr [rsp + 8], rdx
19		mov edi, 16
20		call _malloc
21		mov r11, rax
22		# pop {rcx, rdx}
23		mov rcx, qword ptr [rsp]
24		mov rdx, qword ptr [rsp + 8]
25		add rsp, 16
26		mov rax, 10
27		mov qword ptr [r11], rax
28		mov rax, 97
29		mov qword ptr [r11 + 8], rax
30		mov rax, r11
31		mov r12, rax
32		mov rax, r12
33		mov r13, rax
34		# push {rcx, rdx}
35		sub rsp, 16
36		mov qword ptr [rsp], rcx
37		mov qword ptr [rsp + 8], rdx
38		# Set up R11 as a temporary second base pointer for the caller saved things
39		mov r11, rsp
40		# Stack pointer unchanged, no stack allocated arguments
41		mov rax, r12
42		mov rdi, rax
43		# statement primitives do not return results (but will clobber r0/rax)
44		call _printp
45		call _println
46		# pop {rcx, rdx}
47		mov rcx, qword ptr [rsp]
48		mov rdx, qword ptr [rsp + 8]
49		add rsp, 16
50		# push {rcx, rdx}
51		sub rsp, 16
52		mov qword ptr [rsp], rcx
53		mov qword ptr [rsp + 8], rdx
54		# Set up R11 as a temporary second base pointer for the caller saved things
55		mov r11, rsp
56		# Stack pointer unchanged, no stack allocated arguments
57		mov rax, r13
58		mov rdi, rax
59		# statement primitives do not return results (but will clobber r0/rax)
60		call _printp
61		call _println
62		# pop {rcx, rdx}
63		mov rcx, qword ptr [rsp]
64		mov rdx, qword ptr [rsp + 8]
65		add rsp, 16
66		# push {rcx, rdx}
67		sub rsp, 16
68		mov qword ptr [rsp], rcx
69		mov qword ptr [rsp + 8], rdx
70		# Set up R11 as a temporary second base pointer for the caller saved things
71		mov r11, rsp
72		# Stack pointer unchanged, no stack allocated arguments
73		cmp r12, r13
74		sete al
75		movsx rax, al
76		push rax
77		pop rax
78		mov rax, rax
79		mov rdi, rax
80		# statement primitives do not return results (but will clobber r0/rax)
81		call _printb
82		call _println
83		# pop {rcx, rdx}
84		mov rcx, qword ptr [rsp]
85		mov rdx, qword ptr [rsp + 8]
86		add rsp, 16
87		cmp r12, 0
88		je _errNull
89		mov rax, qword ptr [r12]
90		mov rax, rax
91		mov r14, rax
92		cmp r13, 0
93		je _errNull
94		mov rax, qword ptr [r13]
95		mov rax, rax
96		mov r15, rax
97		# push {rcx, rdx}
98		sub rsp, 16
99		mov qword ptr [rsp], rcx
100		mov qword ptr [rsp + 8], rdx
101		# Set up R11 as a temporary second base pointer for the caller saved things
102		mov r11, rsp
103		# Stack pointer unchanged, no stack allocated arguments
104		mov rax, r14
105		mov rdi, rax
106		# statement primitives do not return results (but will clobber r0/rax)
107		call _printi
108		call _println
109		# pop {rcx, rdx}
110		mov rcx, qword ptr [rsp]
111		mov rdx, qword ptr [rsp + 8]
112		add rsp, 16
113		# push {rcx, rdx}
114		sub rsp, 16
115		mov qword ptr [rsp], rcx
116		mov qword ptr [rsp + 8], rdx
117		# Set up R11 as a temporary second base pointer for the caller saved things
118		mov r11, rsp
119		# Stack pointer unchanged, no stack allocated arguments
120		mov rax, r15
121		mov rdi, rax
122		# statement primitives do not return results (but will clobber r0/rax)
123		call _printi
124		call _println
125		# pop {rcx, rdx}
126		mov rcx, qword ptr [rsp]
127		mov rdx, qword ptr [rsp + 8]
128		add rsp, 16
129		# push {rcx, rdx}
130		sub rsp, 16
131		mov qword ptr [rsp], rcx
132		mov qword ptr [rsp + 8], rdx
133		# Set up R11 as a temporary second base pointer for the caller saved things
134		mov r11, rsp
135		# Stack pointer unchanged, no stack allocated arguments
136		cmp r14, r15
137		sete al
138		movsx rax, al
139		push rax
140		pop rax
141		mov rax, rax
142		mov rdi, rax
143		# statement primitives do not return results (but will clobber r0/rax)
144		call _printb
145		call _println
146		# pop {rcx, rdx}
147		mov rcx, qword ptr [rsp]
148		mov rdx, qword ptr [rsp + 8]
149		add rsp, 16
150		cmp r12, 0
151		je _errNull
152		mov rax, qword ptr [r12 + 8]
153		mov rax, rax
154		mov rcx, rax
155		cmp r13, 0
156		je _errNull
157		mov rax, qword ptr [r13 + 8]
158		mov rax, rax
159		mov rdx, rax
160		# push {rcx, rdx}
161		sub rsp, 16
162		mov qword ptr [rsp], rcx
163		mov qword ptr [rsp + 8], rdx
164		# Set up R11 as a temporary second base pointer for the caller saved things
165		mov r11, rsp
166		# Stack pointer unchanged, no stack allocated arguments
167		mov rax, rcx
168		mov rdi, rax
169		# statement primitives do not return results (but will clobber r0/rax)
170		call _printc
171		call _println
172		# pop {rcx, rdx}
173		mov rcx, qword ptr [rsp]
174		mov rdx, qword ptr [rsp + 8]
175		add rsp, 16
176		# push {rcx, rdx}
177		sub rsp, 16
178		mov qword ptr [rsp], rcx
179		mov qword ptr [rsp + 8], rdx
180		# Set up R11 as a temporary second base pointer for the caller saved things
181		mov r11, rsp
182		# Stack pointer unchanged, no stack allocated arguments
183		mov rax, rdx
184		mov rdi, rax
185		# statement primitives do not return results (but will clobber r0/rax)
186		call _printc
187		call _println
188		# pop {rcx, rdx}
189		mov rcx, qword ptr [rsp]
190		mov rdx, qword ptr [rsp + 8]
191		add rsp, 16
192		# push {rcx, rdx}
193		sub rsp, 16
194		mov qword ptr [rsp], rcx
195		mov qword ptr [rsp + 8], rdx
196		# Set up R11 as a temporary second base pointer for the caller saved things
197		mov r11, rsp
198		# Stack pointer unchanged, no stack allocated arguments
199		cmp rcx, rdx
200		sete al
201		movsx rax, al
202		push rax
203		pop rax
204		mov rax, rax
205		mov rdi, rax
206		# statement primitives do not return results (but will clobber r0/rax)
207		call _printb
208		call _println
209		# pop {rcx, rdx}
210		mov rcx, qword ptr [rsp]
211		mov rdx, qword ptr [rsp + 8]
212		add rsp, 16
213		# Stack pointer unchanged, no stack allocated variables
214		mov rax, 0
215		# pop {rbx, r12, r13, r14, r15}
216		mov rbx, qword ptr [rsp]
217		mov r12, qword ptr [rsp + 8]
218		mov r13, qword ptr [rsp + 16]
219		mov r14, qword ptr [rsp + 24]
220		mov r15, qword ptr [rsp + 32]
221		add rsp, 40
222		pop rbp
223		ret
224	
225	.section .rodata
226	# length of .L._prints_str0
227		.int 4
228	.L._prints_str0:
229		.asciz "%.*s"
230	.text
231	_prints:
232		push rbp
233		mov rbp, rsp
234		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
235		and rsp, -16
236		mov rdx, rdi
237		mov esi, dword ptr [rdi - 4]
238		lea rdi, [rip + .L._prints_str0]
239		# on x86, al represents the number of SIMD registers used as variadic arguments
240		mov al, 0
241		call printf@plt
242		mov rdi, 0
243		call fflush@plt
244		mov rsp, rbp
245		pop rbp
246		ret
247	
248	.section .rodata
249	# length of .L._printp_str0
250		.int 2
251	.L._printp_str0:
252		.asciz "%p"
253	.text
254	_printp:
255		push rbp
256		mov rbp, rsp
257		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
258		and rsp, -16
259		mov rsi, rdi
260		lea rdi, [rip + .L._printp_str0]
261		# on x86, al represents the number of SIMD registers used as variadic arguments
262		mov al, 0
263		call printf@plt
264		mov rdi, 0
265		call fflush@plt
266		mov rsp, rbp
267		pop rbp
268		ret
269	
270	_malloc:
271		push rbp
272		mov rbp, rsp
273		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
274		and rsp, -16
275		call malloc@plt
276		cmp rax, 0
277		je _errOutOfMemory
278		mov rsp, rbp
279		pop rbp
280		ret
281	
282	.section .rodata
283	# length of .L._printi_str0
284		.int 2
285	.L._printi_str0:
286		.asciz "%d"
287	.text
288	_printi:
289		push rbp
290		mov rbp, rsp
291		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
292		and rsp, -16
293		mov esi, edi
294		lea rdi, [rip + .L._printi_str0]
295		# on x86, al represents the number of SIMD registers used as variadic arguments
296		mov al, 0
297		call printf@plt
298		mov rdi, 0
299		call fflush@plt
300		mov rsp, rbp
301		pop rbp
302		ret
303	
304	.section .rodata
305	# length of .L._printb_str0
306		.int 5
307	.L._printb_str0:
308		.asciz "false"
309	# length of .L._printb_str1
310		.int 4
311	.L._printb_str1:
312		.asciz "true"
313	# length of .L._printb_str2
314		.int 4
315	.L._printb_str2:
316		.asciz "%.*s"
317	.text
318	_printb:
319		push rbp
320		mov rbp, rsp
321		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
322		and rsp, -16
323		cmp dil, 0
324		jne .L_printb0
325		lea rdx, [rip + .L._printb_str0]
326		jmp .L_printb1
327	.L_printb0:
328		lea rdx, [rip + .L._printb_str1]
329	.L_printb1:
330		mov esi, dword ptr [rdx - 4]
331		lea rdi, [rip + .L._printb_str2]
332		# on x86, al represents the number of SIMD registers used as variadic arguments
333		mov al, 0
334		call printf@plt
335		mov rdi, 0
336		call fflush@plt
337		mov rsp, rbp
338		pop rbp
339		ret
340	
341	.section .rodata
342	# length of .L._printc_str0
343		.int 2
344	.L._printc_str0:
345		.asciz "%c"
346	.text
347	_printc:
348		push rbp
349		mov rbp, rsp
350		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
351		and rsp, -16
352		mov sil, dil
353		lea rdi, [rip + .L._printc_str0]
354		# on x86, al represents the number of SIMD registers used as variadic arguments
355		mov al, 0
356		call printf@plt
357		mov rdi, 0
358		call fflush@plt
359		mov rsp, rbp
360		pop rbp
361		ret
362	
363	.section .rodata
364	# length of .L._println_str0
365		.int 0
366	.L._println_str0:
367		.asciz ""
368	.text
369	_println:
370		push rbp
371		mov rbp, rsp
372		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
373		and rsp, -16
374		lea rdi, [rip + .L._println_str0]
375		call puts@plt
376		mov rdi, 0
377		call fflush@plt
378		mov rsp, rbp
379		pop rbp
380		ret
381	
382	_exit:
383		push rbp
384		mov rbp, rsp
385		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
386		and rsp, -16
387		call exit@plt
388		mov rsp, rbp
389		pop rbp
390		ret
391	
392	.section .rodata
393	# length of .L._errOutOfMemory_str0
394		.int 27
395	.L._errOutOfMemory_str0:
396		.asciz "fatal error: out of memory\n"
397	.text
398	_errOutOfMemory:
399		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
400		and rsp, -16
401		lea rdi, [rip + .L._errOutOfMemory_str0]
402		call _prints
403		mov dil, -1
404		call exit@plt
405	
406	.section .rodata
407	# length of .L._errNull_str0
408		.int 45
409	.L._errNull_str0:
410		.asciz "fatal error: null pair dereferenced or freed\n"
411	.text
412	_errNull:
413		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
414		and rsp, -16
415		lea rdi, [rip + .L._errNull_str0]
416		call _prints
417		mov dil, -1
418		call exit@plt
===========================================================
-- Finished

