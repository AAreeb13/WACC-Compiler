./valid/array/arrayNested.wacc
calling the reference compiler on ./valid/array/arrayNested.wacc
-- Test: arrayNested.wacc

-- Uploaded file: 
---------------------------------------------------------------
# basic array declaration and assignment

# Output:
# 3
# 3
#

# Program:

begin
  int[] a = [1,2,3];
  int[] b = [3,4];
  int[][] c = [a,b] ;
  println c[0][2] ;
  println c[1][0]
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
arrayNested.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	.text
4	main:
5		push rbp
6		# push {rbx, r12, r13, r14}
7		sub rsp, 32
8		mov qword ptr [rsp], rbx
9		mov qword ptr [rsp + 8], r12
10		mov qword ptr [rsp + 16], r13
11		mov qword ptr [rsp + 24], r14
12		mov rbp, rsp
13		# Stack pointer unchanged, no stack allocated variables
14		# 3 element array
15		mov edi, 16
16		call _malloc
17		mov r11, rax
18		# array pointers are shifted forwards by 4 bytes (to account for size)
19		mov r11, r11
20		add r11, 4
21		mov rax, 3
22		mov dword ptr [r11 - 4], eax
23		mov rax, 1
24		mov dword ptr [r11], eax
25		mov rax, 2
26		mov dword ptr [r11 + 4], eax
27		mov rax, 3
28		mov dword ptr [r11 + 8], eax
29		mov rax, r11
30		mov r12, rax
31		# 2 element array
32		mov edi, 12
33		call _malloc
34		mov r11, rax
35		# array pointers are shifted forwards by 4 bytes (to account for size)
36		mov r11, r11
37		add r11, 4
38		mov rax, 2
39		mov dword ptr [r11 - 4], eax
40		mov rax, 3
41		mov dword ptr [r11], eax
42		mov rax, 4
43		mov dword ptr [r11 + 4], eax
44		mov rax, r11
45		mov r13, rax
46		# 2 element array
47		mov edi, 20
48		call _malloc
49		mov r11, rax
50		# array pointers are shifted forwards by 4 bytes (to account for size)
51		mov r11, r11
52		add r11, 4
53		mov rax, 2
54		mov dword ptr [r11 - 4], eax
55		mov rax, r12
56		mov qword ptr [r11], rax
57		mov rax, r13
58		mov qword ptr [r11 + 8], rax
59		mov rax, r11
60		mov r14, rax
61		# Stack pointer unchanged, no stack allocated arguments
62		mov r10d, 0
63		mov r9, r14
64		call _arrLoad8
65		mov rax, r9
66		mov rax, rax
67		push rax
68		mov r10d, 2
69		pop rax
70		mov r9, rax
71		call _arrLoad4
72		mov eax, r9d
73		mov rax, rax
74		mov rdi, rax
75		# statement primitives do not return results (but will clobber r0/rax)
76		call _printi
77		call _println
78		# Stack pointer unchanged, no stack allocated arguments
79		mov r10d, 1
80		mov r9, r14
81		call _arrLoad8
82		mov rax, r9
83		mov rax, rax
84		push rax
85		mov r10d, 0
86		pop rax
87		mov r9, rax
88		call _arrLoad4
89		mov eax, r9d
90		mov rax, rax
91		mov rdi, rax
92		# statement primitives do not return results (but will clobber r0/rax)
93		call _printi
94		call _println
95		# Stack pointer unchanged, no stack allocated variables
96		mov rax, 0
97		# pop {rbx, r12, r13, r14}
98		mov rbx, qword ptr [rsp]
99		mov r12, qword ptr [rsp + 8]
100		mov r13, qword ptr [rsp + 16]
101		mov r14, qword ptr [rsp + 24]
102		add rsp, 32
103		pop rbp
104		ret
105	
106	.section .rodata
107	# length of .L._prints_str0
108		.int 4
109	.L._prints_str0:
110		.asciz "%.*s"
111	.text
112	_prints:
113		push rbp
114		mov rbp, rsp
115		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
116		and rsp, -16
117		mov rdx, rdi
118		mov esi, dword ptr [rdi - 4]
119		lea rdi, [rip + .L._prints_str0]
120		# on x86, al represents the number of SIMD registers used as variadic arguments
121		mov al, 0
122		call printf@plt
123		mov rdi, 0
124		call fflush@plt
125		mov rsp, rbp
126		pop rbp
127		ret
128	
129	_malloc:
130		push rbp
131		mov rbp, rsp
132		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
133		and rsp, -16
134		call malloc@plt
135		cmp rax, 0
136		je _errOutOfMemory
137		mov rsp, rbp
138		pop rbp
139		ret
140	
141	.section .rodata
142	# length of .L._printi_str0
143		.int 2
144	.L._printi_str0:
145		.asciz "%d"
146	.text
147	_printi:
148		push rbp
149		mov rbp, rsp
150		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
151		and rsp, -16
152		mov esi, edi
153		lea rdi, [rip + .L._printi_str0]
154		# on x86, al represents the number of SIMD registers used as variadic arguments
155		mov al, 0
156		call printf@plt
157		mov rdi, 0
158		call fflush@plt
159		mov rsp, rbp
160		pop rbp
161		ret
162	
163	.section .rodata
164	# length of .L._println_str0
165		.int 0
166	.L._println_str0:
167		.asciz ""
168	.text
169	_println:
170		push rbp
171		mov rbp, rsp
172		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
173		and rsp, -16
174		lea rdi, [rip + .L._println_str0]
175		call puts@plt
176		mov rdi, 0
177		call fflush@plt
178		mov rsp, rbp
179		pop rbp
180		ret
181	
182	_exit:
183		push rbp
184		mov rbp, rsp
185		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
186		and rsp, -16
187		call exit@plt
188		mov rsp, rbp
189		pop rbp
190		ret
191	
192	_arrLoad8:
193		# Special calling convention: array ptr passed in R9, index in R10, and return into R9
194		push rbx
195		cmp r10d, 0
196		cmovl esi, r10d
197		jl _errOutOfBounds
198		mov ebx, dword ptr [r9 - 4]
199		cmp r10d, ebx
200		cmovge esi, r10d
201		jge _errOutOfBounds
202		mov r9, qword ptr [r9 + 8*r10]
203		pop rbx
204		ret
205	
206	_arrLoad4:
207		# Special calling convention: array ptr passed in R9, index in R10, and return into R9
208		push rbx
209		cmp r10d, 0
210		cmovl esi, r10d
211		jl _errOutOfBounds
212		mov ebx, dword ptr [r9 - 4]
213		cmp r10d, ebx
214		cmovge esi, r10d
215		jge _errOutOfBounds
216		movsx r9, dword ptr [r9 + 4*r10]
217		pop rbx
218		ret
219	
220	.section .rodata
221	# length of .L._errOutOfMemory_str0
222		.int 27
223	.L._errOutOfMemory_str0:
224		.asciz "fatal error: out of memory\n"
225	.text
226	_errOutOfMemory:
227		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
228		and rsp, -16
229		lea rdi, [rip + .L._errOutOfMemory_str0]
230		call _prints
231		mov dil, -1
232		call exit@plt
233	
234	.section .rodata
235	# length of .L._errOutOfBounds_str0
236		.int 42
237	.L._errOutOfBounds_str0:
238		.asciz "fatal error: array index %d out of bounds\n"
239	.text
240	_errOutOfBounds:
241		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
242		and rsp, -16
243		lea rdi, [rip + .L._errOutOfBounds_str0]
244		# on x86, al represents the number of SIMD registers used as variadic arguments
245		mov al, 0
246		call printf@plt
247		mov rdi, 0
248		call fflush@plt
249		mov dil, -1
250		call exit@plt
===========================================================
-- Finished

