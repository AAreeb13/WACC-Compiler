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
15		# 3 element array
16		mov edi, 16
17		call _malloc
18		mov r11, rax
19		# array pointers are shifted forwards by 4 bytes (to account for size)
20		mov r11, r11
21		add r11, 4
22		mov rax, 3
23		mov dword ptr [r11 - 4], eax
24		mov rax, 2
25		mov dword ptr [r11], eax
26		mov rax, 0
27		mov dword ptr [r11 + 4], eax
28		mov rax, 1
29		mov dword ptr [r11 + 8], eax
30		mov rax, r11
31		mov r12, rax
32		# 3 element array
33		mov edi, 16
34		call _malloc
35		mov r11, rax
36		# array pointers are shifted forwards by 4 bytes (to account for size)
37		mov r11, r11
38		add r11, 4
39		mov rax, 3
40		mov dword ptr [r11 - 4], eax
41		mov rax, 1
42		mov dword ptr [r11], eax
43		mov rax, 2
44		mov dword ptr [r11 + 4], eax
45		mov rax, 0
46		mov dword ptr [r11 + 8], eax
47		mov rax, r11
48		mov r13, rax
49		# 3 element array
50		mov edi, 16
51		call _malloc
52		mov r11, rax
53		# array pointers are shifted forwards by 4 bytes (to account for size)
54		mov r11, r11
55		add r11, 4
56		mov rax, 3
57		mov dword ptr [r11 - 4], eax
58		mov rax, 5
59		mov dword ptr [r11], eax
60		mov rax, 6
61		mov dword ptr [r11 + 4], eax
62		mov rax, 7
63		mov dword ptr [r11 + 8], eax
64		mov rax, r11
65		mov r14, rax
66		mov rax, 0
67		mov r15, rax
68		jmp .L0
69	.L1:
70		mov r10d, r15d
71		mov r9, r13
72		call _arrLoad4
73		mov eax, r9d
74		mov rax, rax
75		push rax
76		pop r10
77		mov r10d, r10d
78		mov r9, r12
79		call _arrLoad4
80		mov eax, r9d
81		mov rax, rax
82		push rax
83		pop r10
84		mov r10d, r10d
85		mov r9, r14
86		call _arrLoad4
87		mov eax, r9d
88		mov eax, eax
89		add eax, 1
90		jo _errOverflow
91		movsx rax, eax
92		push rax
93		mov r10d, r15d
94		mov r9, r13
95		call _arrLoad4
96		mov eax, r9d
97		mov rax, rax
98		push rax
99		pop r10
100		mov r10d, r10d
101		mov r9, r12
102		call _arrLoad4
103		mov eax, r9d
104		mov rax, rax
105		push rax
106		pop r10
107		mov r10d, r10d
108		pop rax
109		mov rax, rax
110		mov r9, r14
111		call _arrStore4
112		# Stack pointer unchanged, no stack allocated arguments
113		mov r10d, r15d
114		mov r9, r13
115		call _arrLoad4
116		mov eax, r9d
117		mov rax, rax
118		push rax
119		pop r10
120		mov r10d, r10d
121		mov r9, r12
122		call _arrLoad4
123		mov eax, r9d
124		mov rax, rax
125		push rax
126		pop r10
127		mov r10d, r10d
128		mov r9, r14
129		call _arrLoad4
130		mov eax, r9d
131		mov rax, rax
132		mov rdi, rax
133		# statement primitives do not return results (but will clobber r0/rax)
134		call _printi
135		call _println
136		mov eax, r15d
137		add eax, 1
138		jo _errOverflow
139		movsx rax, eax
140		push rax
141		pop rax
142		mov rax, rax
143		mov r15, rax
144	.L0:
145		cmp r15, 3
146		jne .L1
147		# Stack pointer unchanged, no stack allocated variables
148		mov rax, 0
149		# pop {rbx, r12, r13, r14, r15}
150		mov rbx, qword ptr [rsp]
151		mov r12, qword ptr [rsp + 8]
152		mov r13, qword ptr [rsp + 16]
153		mov r14, qword ptr [rsp + 24]
154		mov r15, qword ptr [rsp + 32]
155		add rsp, 40
156		pop rbp
157		ret
158	
159	.section .rodata
160	# length of .L._prints_str0
161		.int 4
162	.L._prints_str0:
163		.asciz "%.*s"
164	.text
165	_prints:
166		push rbp
167		mov rbp, rsp
168		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
169		and rsp, -16
170		mov rdx, rdi
171		mov esi, dword ptr [rdi - 4]
172		lea rdi, [rip + .L._prints_str0]
173		# on x86, al represents the number of SIMD registers used as variadic arguments
174		mov al, 0
175		call printf@plt
176		mov rdi, 0
177		call fflush@plt
178		mov rsp, rbp
179		pop rbp
180		ret
181	
182	_malloc:
183		push rbp
184		mov rbp, rsp
185		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
186		and rsp, -16
187		call malloc@plt
188		cmp rax, 0
189		je _errOutOfMemory
190		mov rsp, rbp
191		pop rbp
192		ret
193	
194	.section .rodata
195	# length of .L._printi_str0
196		.int 2
197	.L._printi_str0:
198		.asciz "%d"
199	.text
200	_printi:
201		push rbp
202		mov rbp, rsp
203		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
204		and rsp, -16
205		mov esi, edi
206		lea rdi, [rip + .L._printi_str0]
207		# on x86, al represents the number of SIMD registers used as variadic arguments
208		mov al, 0
209		call printf@plt
210		mov rdi, 0
211		call fflush@plt
212		mov rsp, rbp
213		pop rbp
214		ret
215	
216	.section .rodata
217	# length of .L._println_str0
218		.int 0
219	.L._println_str0:
220		.asciz ""
221	.text
222	_println:
223		push rbp
224		mov rbp, rsp
225		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
226		and rsp, -16
227		lea rdi, [rip + .L._println_str0]
228		call puts@plt
229		mov rdi, 0
230		call fflush@plt
231		mov rsp, rbp
232		pop rbp
233		ret
234	
235	_exit:
236		push rbp
237		mov rbp, rsp
238		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
239		and rsp, -16
240		call exit@plt
241		mov rsp, rbp
242		pop rbp
243		ret
244	
245	_arrStore4:
246		# Special calling convention: array ptr passed in R9, index in R10, value to store in RAX
247		push rbx
248		cmp r10d, 0
249		cmovl esi, r10d
250		jl _errOutOfBounds
251		mov ebx, dword ptr [r9 - 4]
252		cmp r10d, ebx
253		cmovge esi, r10d
254		jge _errOutOfBounds
255		mov dword ptr [r9 + 4*r10], eax
256		pop rbx
257		ret
258	
259	_arrLoad4:
260		# Special calling convention: array ptr passed in R9, index in R10, and return into R9
261		push rbx
262		cmp r10d, 0
263		cmovl esi, r10d
264		jl _errOutOfBounds
265		mov ebx, dword ptr [r9 - 4]
266		cmp r10d, ebx
267		cmovge esi, r10d
268		jge _errOutOfBounds
269		movsx r9, dword ptr [r9 + 4*r10]
270		pop rbx
271		ret
272	
273	.section .rodata
274	# length of .L._errOutOfMemory_str0
275		.int 27
276	.L._errOutOfMemory_str0:
277		.asciz "fatal error: out of memory\n"
278	.text
279	_errOutOfMemory:
280		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
281		and rsp, -16
282		lea rdi, [rip + .L._errOutOfMemory_str0]
283		call _prints
284		mov dil, -1
285		call exit@plt
286	
287	.section .rodata
288	# length of .L._errOutOfBounds_str0
289		.int 42
290	.L._errOutOfBounds_str0:
291		.asciz "fatal error: array index %d out of bounds\n"
292	.text
293	_errOutOfBounds:
294		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
295		and rsp, -16
296		lea rdi, [rip + .L._errOutOfBounds_str0]
297		# on x86, al represents the number of SIMD registers used as variadic arguments
298		mov al, 0
299		call printf@plt
300		mov rdi, 0
301		call fflush@plt
302		mov dil, -1
303		call exit@plt
304	
305	.section .rodata
306	# length of .L._errOverflow_str0
307		.int 52
308	.L._errOverflow_str0:
309		.asciz "fatal error: integer overflow or underflow occurred\n"
310	.text
311	_errOverflow:
312		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
313		and rsp, -16
314		lea rdi, [rip + .L._errOverflow_str0]
315		call _prints
316		mov dil, -1
317		call exit@plt
===========================================================
-- Finished

