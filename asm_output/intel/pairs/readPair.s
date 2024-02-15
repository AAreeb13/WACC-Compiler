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
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 39
5	.L.str0:
6		.asciz "Please enter the first element (char): "
7	# length of .L.str1
8		.int 39
9	.L.str1:
10		.asciz "Please enter the second element (int): "
11	# length of .L.str2
12		.int 22
13	.L.str2:
14		.asciz "The first element was "
15	# length of .L.str3
16		.int 23
17	.L.str3:
18		.asciz "The second element was "
19	.text
20	main:
21		push rbp
22		# push {rbx, r12, r13, r14}
23		sub rsp, 32
24		mov qword ptr [rsp], rbx
25		mov qword ptr [rsp + 8], r12
26		mov qword ptr [rsp + 16], r13
27		mov qword ptr [rsp + 24], r14
28		mov rbp, rsp
29		# Stack pointer unchanged, no stack allocated variables
30		mov edi, 16
31		call _malloc
32		mov r11, rax
33		mov rax, 0
34		mov qword ptr [r11], rax
35		mov rax, 0
36		mov qword ptr [r11 + 8], rax
37		mov rax, r11
38		mov r12, rax
39		# Stack pointer unchanged, no stack allocated arguments
40		lea rax, [rip + .L.str0]
41		push rax
42		pop rax
43		mov rax, rax
44		mov rdi, rax
45		# statement primitives do not return results (but will clobber r0/rax)
46		call _prints
47		mov rax, 48
48		mov r13, rax
49		# Stack pointer unchanged, no stack allocated arguments
50		# load the current value in the destination of the read so it supports defaults
51		mov rax, r13
52		mov rdi, rax
53		call _readc
54		mov r11, rax
55		mov rax, r11
56		mov r13, rax
57		cmp r12, 0
58		je _errNull
59		mov rax, r13
60		mov qword ptr [r12], rax
61		# Stack pointer unchanged, no stack allocated arguments
62		lea rax, [rip + .L.str1]
63		push rax
64		pop rax
65		mov rax, rax
66		mov rdi, rax
67		# statement primitives do not return results (but will clobber r0/rax)
68		call _prints
69		mov rax, 0
70		mov r14, rax
71		# Stack pointer unchanged, no stack allocated arguments
72		# load the current value in the destination of the read so it supports defaults
73		mov rax, r14
74		mov rdi, rax
75		call _readi
76		mov r11, rax
77		mov rax, r11
78		mov r14, rax
79		cmp r12, 0
80		je _errNull
81		mov rax, r14
82		mov qword ptr [r12 + 8], rax
83		mov rax, 0
84		mov r13, rax
85		mov rax, -1
86		mov r14, rax
87		# Stack pointer unchanged, no stack allocated arguments
88		lea rax, [rip + .L.str2]
89		push rax
90		pop rax
91		mov rax, rax
92		mov rdi, rax
93		# statement primitives do not return results (but will clobber r0/rax)
94		call _prints
95		cmp r12, 0
96		je _errNull
97		mov rax, qword ptr [r12]
98		mov rax, rax
99		push rax
100		pop rax
101		mov rax, rax
102		mov r13, rax
103		# Stack pointer unchanged, no stack allocated arguments
104		mov rax, r13
105		mov rdi, rax
106		# statement primitives do not return results (but will clobber r0/rax)
107		call _printc
108		call _println
109		# Stack pointer unchanged, no stack allocated arguments
110		lea rax, [rip + .L.str3]
111		push rax
112		pop rax
113		mov rax, rax
114		mov rdi, rax
115		# statement primitives do not return results (but will clobber r0/rax)
116		call _prints
117		cmp r12, 0
118		je _errNull
119		mov rax, qword ptr [r12 + 8]
120		mov rax, rax
121		push rax
122		pop rax
123		mov rax, rax
124		mov r14, rax
125		# Stack pointer unchanged, no stack allocated arguments
126		mov rax, r14
127		mov rdi, rax
128		# statement primitives do not return results (but will clobber r0/rax)
129		call _printi
130		call _println
131		# Stack pointer unchanged, no stack allocated variables
132		mov rax, 0
133		# pop {rbx, r12, r13, r14}
134		mov rbx, qword ptr [rsp]
135		mov r12, qword ptr [rsp + 8]
136		mov r13, qword ptr [rsp + 16]
137		mov r14, qword ptr [rsp + 24]
138		add rsp, 32
139		pop rbp
140		ret
141	
142	.section .rodata
143	# length of .L._readc_str0
144		.int 3
145	.L._readc_str0:
146		.asciz " %c"
147	.text
148	_readc:
149		push rbp
150		mov rbp, rsp
151		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
152		and rsp, -16
153		# RDI contains the "original" value of the destination of the read
154		# allocate space on the stack to store the read: preserve alignment!
155		# the passed default argument should be stored in case of EOF
156		sub rsp, 16
157		mov byte ptr [rsp], dil
158		lea rsi, qword ptr [rsp]
159		lea rdi, [rip + .L._readc_str0]
160		# on x86, al represents the number of SIMD registers used as variadic arguments
161		mov al, 0
162		call scanf@plt
163		movsx rax, byte ptr [rsp]
164		add rsp, 16
165		mov rsp, rbp
166		pop rbp
167		ret
168	
169	.section .rodata
170	# length of .L._prints_str0
171		.int 4
172	.L._prints_str0:
173		.asciz "%.*s"
174	.text
175	_prints:
176		push rbp
177		mov rbp, rsp
178		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
179		and rsp, -16
180		mov rdx, rdi
181		mov esi, dword ptr [rdi - 4]
182		lea rdi, [rip + .L._prints_str0]
183		# on x86, al represents the number of SIMD registers used as variadic arguments
184		mov al, 0
185		call printf@plt
186		mov rdi, 0
187		call fflush@plt
188		mov rsp, rbp
189		pop rbp
190		ret
191	
192	.section .rodata
193	# length of .L._readi_str0
194		.int 2
195	.L._readi_str0:
196		.asciz "%d"
197	.text
198	_readi:
199		push rbp
200		mov rbp, rsp
201		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
202		and rsp, -16
203		# RDI contains the "original" value of the destination of the read
204		# allocate space on the stack to store the read: preserve alignment!
205		# the passed default argument should be stored in case of EOF
206		sub rsp, 16
207		mov dword ptr [rsp], edi
208		lea rsi, qword ptr [rsp]
209		lea rdi, [rip + .L._readi_str0]
210		# on x86, al represents the number of SIMD registers used as variadic arguments
211		mov al, 0
212		call scanf@plt
213		movsx rax, dword ptr [rsp]
214		add rsp, 16
215		mov rsp, rbp
216		pop rbp
217		ret
218	
219	_malloc:
220		push rbp
221		mov rbp, rsp
222		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
223		and rsp, -16
224		call malloc@plt
225		cmp rax, 0
226		je _errOutOfMemory
227		mov rsp, rbp
228		pop rbp
229		ret
230	
231	.section .rodata
232	# length of .L._printi_str0
233		.int 2
234	.L._printi_str0:
235		.asciz "%d"
236	.text
237	_printi:
238		push rbp
239		mov rbp, rsp
240		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
241		and rsp, -16
242		mov esi, edi
243		lea rdi, [rip + .L._printi_str0]
244		# on x86, al represents the number of SIMD registers used as variadic arguments
245		mov al, 0
246		call printf@plt
247		mov rdi, 0
248		call fflush@plt
249		mov rsp, rbp
250		pop rbp
251		ret
252	
253	.section .rodata
254	# length of .L._printc_str0
255		.int 2
256	.L._printc_str0:
257		.asciz "%c"
258	.text
259	_printc:
260		push rbp
261		mov rbp, rsp
262		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
263		and rsp, -16
264		mov sil, dil
265		lea rdi, [rip + .L._printc_str0]
266		# on x86, al represents the number of SIMD registers used as variadic arguments
267		mov al, 0
268		call printf@plt
269		mov rdi, 0
270		call fflush@plt
271		mov rsp, rbp
272		pop rbp
273		ret
274	
275	.section .rodata
276	# length of .L._println_str0
277		.int 0
278	.L._println_str0:
279		.asciz ""
280	.text
281	_println:
282		push rbp
283		mov rbp, rsp
284		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
285		and rsp, -16
286		lea rdi, [rip + .L._println_str0]
287		call puts@plt
288		mov rdi, 0
289		call fflush@plt
290		mov rsp, rbp
291		pop rbp
292		ret
293	
294	_exit:
295		push rbp
296		mov rbp, rsp
297		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
298		and rsp, -16
299		call exit@plt
300		mov rsp, rbp
301		pop rbp
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
312		and rsp, -16
313		lea rdi, [rip + .L._errOutOfMemory_str0]
314		call _prints
315		mov dil, -1
316		call exit@plt
317	
318	.section .rodata
319	# length of .L._errNull_str0
320		.int 45
321	.L._errNull_str0:
322		.asciz "fatal error: null pair dereferenced or freed\n"
323	.text
324	_errNull:
325		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
326		and rsp, -16
327		lea rdi, [rip + .L._errNull_str0]
328		call _prints
329		mov dil, -1
330		call exit@plt
===========================================================
-- Finished

