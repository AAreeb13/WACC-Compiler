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
15		# 0 element array
16		mov edi, 4
17		call _malloc
18		mov r11, rax
19		# array pointers are shifted forwards by 4 bytes (to account for size)
20		mov r11, r11
21		add r11, 4
22		mov rax, 0
23		mov dword ptr [r11 - 4], eax
24		mov rax, r11
25		mov r12, rax
26		# 2 element array
27		mov edi, 20
28		call _malloc
29		mov r11, rax
30		# array pointers are shifted forwards by 4 bytes (to account for size)
31		mov r11, r11
32		add r11, 4
33		mov rax, 2
34		mov dword ptr [r11 - 4], eax
35		mov rax, r12
36		mov qword ptr [r11], rax
37		mov rax, r12
38		mov qword ptr [r11 + 8], rax
39		mov rax, r11
40		mov r13, rax
41		mov rax, 0
42		mov r14, rax
43		jmp .L0
44	.L1:
45		# Stack pointer unchanged, no stack allocated variables
46		# 1 element array
47		mov edi, 8
48		call _malloc
49		mov r11, rax
50		# array pointers are shifted forwards by 4 bytes (to account for size)
51		mov r11, r11
52		add r11, 4
53		mov rax, 1
54		mov dword ptr [r11 - 4], eax
55		mov rax, r14
56		mov dword ptr [r11], eax
57		mov rax, r11
58		mov r15, rax
59		mov r10d, r14d
60		mov rax, r15
61		mov r9, r13
62		call _arrStore8
63		mov eax, r14d
64		add eax, 1
65		jo _errOverflow
66		movsx rax, eax
67		push rax
68		pop rax
69		mov rax, rax
70		mov r14, rax
71		# Stack pointer unchanged, no stack allocated variables
72	.L0:
73		cmp r14, 2
74		jl .L1
75		# Stack pointer unchanged, no stack allocated arguments
76		mov r10d, 1
77		mov r9, r13
78		call _arrLoad8
79		mov rbx, r9
80		mov r10d, 0
81		mov r9, r13
82		call _arrLoad8
83		mov rax, r9
84		cmp rax, rbx
85		sete al
86		movsx rax, al
87		push rax
88		pop rax
89		mov rax, rax
90		mov rdi, rax
91		# statement primitives do not return results (but will clobber r0/rax)
92		call _printb
93		call _println
94		# Stack pointer unchanged, no stack allocated arguments
95		mov r10d, 0
96		mov r9, r13
97		call _arrLoad8
98		mov rax, r9
99		mov rax, rax
100		push rax
101		mov r10d, 0
102		pop rax
103		mov r9, rax
104		call _arrLoad4
105		mov eax, r9d
106		mov rax, rax
107		mov rdi, rax
108		# statement primitives do not return results (but will clobber r0/rax)
109		call _printi
110		call _println
111		# Stack pointer unchanged, no stack allocated arguments
112		mov r10d, 1
113		mov r9, r13
114		call _arrLoad8
115		mov rax, r9
116		mov rax, rax
117		push rax
118		mov r10d, 0
119		pop rax
120		mov r9, rax
121		call _arrLoad4
122		mov eax, r9d
123		mov rax, rax
124		mov rdi, rax
125		# statement primitives do not return results (but will clobber r0/rax)
126		call _printi
127		call _println
128		# Stack pointer unchanged, no stack allocated variables
129		mov rax, 0
130		# pop {rbx, r12, r13, r14, r15}
131		mov rbx, qword ptr [rsp]
132		mov r12, qword ptr [rsp + 8]
133		mov r13, qword ptr [rsp + 16]
134		mov r14, qword ptr [rsp + 24]
135		mov r15, qword ptr [rsp + 32]
136		add rsp, 40
137		pop rbp
138		ret
139	
140	.section .rodata
141	# length of .L._prints_str0
142		.int 4
143	.L._prints_str0:
144		.asciz "%.*s"
145	.text
146	_prints:
147		push rbp
148		mov rbp, rsp
149		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
150		and rsp, -16
151		mov rdx, rdi
152		mov esi, dword ptr [rdi - 4]
153		lea rdi, [rip + .L._prints_str0]
154		# on x86, al represents the number of SIMD registers used as variadic arguments
155		mov al, 0
156		call printf@plt
157		mov rdi, 0
158		call fflush@plt
159		mov rsp, rbp
160		pop rbp
161		ret
162	
163	_malloc:
164		push rbp
165		mov rbp, rsp
166		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
167		and rsp, -16
168		call malloc@plt
169		cmp rax, 0
170		je _errOutOfMemory
171		mov rsp, rbp
172		pop rbp
173		ret
174	
175	.section .rodata
176	# length of .L._printi_str0
177		.int 2
178	.L._printi_str0:
179		.asciz "%d"
180	.text
181	_printi:
182		push rbp
183		mov rbp, rsp
184		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
185		and rsp, -16
186		mov esi, edi
187		lea rdi, [rip + .L._printi_str0]
188		# on x86, al represents the number of SIMD registers used as variadic arguments
189		mov al, 0
190		call printf@plt
191		mov rdi, 0
192		call fflush@plt
193		mov rsp, rbp
194		pop rbp
195		ret
196	
197	.section .rodata
198	# length of .L._printb_str0
199		.int 5
200	.L._printb_str0:
201		.asciz "false"
202	# length of .L._printb_str1
203		.int 4
204	.L._printb_str1:
205		.asciz "true"
206	# length of .L._printb_str2
207		.int 4
208	.L._printb_str2:
209		.asciz "%.*s"
210	.text
211	_printb:
212		push rbp
213		mov rbp, rsp
214		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
215		and rsp, -16
216		cmp dil, 0
217		jne .L_printb0
218		lea rdx, [rip + .L._printb_str0]
219		jmp .L_printb1
220	.L_printb0:
221		lea rdx, [rip + .L._printb_str1]
222	.L_printb1:
223		mov esi, dword ptr [rdx - 4]
224		lea rdi, [rip + .L._printb_str2]
225		# on x86, al represents the number of SIMD registers used as variadic arguments
226		mov al, 0
227		call printf@plt
228		mov rdi, 0
229		call fflush@plt
230		mov rsp, rbp
231		pop rbp
232		ret
233	
234	.section .rodata
235	# length of .L._println_str0
236		.int 0
237	.L._println_str0:
238		.asciz ""
239	.text
240	_println:
241		push rbp
242		mov rbp, rsp
243		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
244		and rsp, -16
245		lea rdi, [rip + .L._println_str0]
246		call puts@plt
247		mov rdi, 0
248		call fflush@plt
249		mov rsp, rbp
250		pop rbp
251		ret
252	
253	_exit:
254		push rbp
255		mov rbp, rsp
256		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
257		and rsp, -16
258		call exit@plt
259		mov rsp, rbp
260		pop rbp
261		ret
262	
263	_arrLoad8:
264		# Special calling convention: array ptr passed in R9, index in R10, and return into R9
265		push rbx
266		cmp r10d, 0
267		cmovl esi, r10d
268		jl _errOutOfBounds
269		mov ebx, dword ptr [r9 - 4]
270		cmp r10d, ebx
271		cmovge esi, r10d
272		jge _errOutOfBounds
273		mov r9, qword ptr [r9 + 8*r10]
274		pop rbx
275		ret
276	
277	_arrLoad4:
278		# Special calling convention: array ptr passed in R9, index in R10, and return into R9
279		push rbx
280		cmp r10d, 0
281		cmovl esi, r10d
282		jl _errOutOfBounds
283		mov ebx, dword ptr [r9 - 4]
284		cmp r10d, ebx
285		cmovge esi, r10d
286		jge _errOutOfBounds
287		movsx r9, dword ptr [r9 + 4*r10]
288		pop rbx
289		ret
290	
291	_arrStore8:
292		# Special calling convention: array ptr passed in R9, index in R10, value to store in RAX
293		push rbx
294		cmp r10d, 0
295		cmovl esi, r10d
296		jl _errOutOfBounds
297		mov ebx, dword ptr [r9 - 4]
298		cmp r10d, ebx
299		cmovge esi, r10d
300		jge _errOutOfBounds
301		mov qword ptr [r9 + 8*r10], rax
302		pop rbx
303		ret
304	
305	.section .rodata
306	# length of .L._errOutOfMemory_str0
307		.int 27
308	.L._errOutOfMemory_str0:
309		.asciz "fatal error: out of memory\n"
310	.text
311	_errOutOfMemory:
312		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
313		and rsp, -16
314		lea rdi, [rip + .L._errOutOfMemory_str0]
315		call _prints
316		mov dil, -1
317		call exit@plt
318	
319	.section .rodata
320	# length of .L._errOutOfBounds_str0
321		.int 42
322	.L._errOutOfBounds_str0:
323		.asciz "fatal error: array index %d out of bounds\n"
324	.text
325	_errOutOfBounds:
326		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
327		and rsp, -16
328		lea rdi, [rip + .L._errOutOfBounds_str0]
329		# on x86, al represents the number of SIMD registers used as variadic arguments
330		mov al, 0
331		call printf@plt
332		mov rdi, 0
333		call fflush@plt
334		mov dil, -1
335		call exit@plt
336	
337	.section .rodata
338	# length of .L._errOverflow_str0
339		.int 52
340	.L._errOverflow_str0:
341		.asciz "fatal error: integer overflow or underflow occurred\n"
342	.text
343	_errOverflow:
344		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
345		and rsp, -16
346		lea rdi, [rip + .L._errOverflow_str0]
347		call _prints
348		mov dil, -1
349		call exit@plt
===========================================================
-- Finished

