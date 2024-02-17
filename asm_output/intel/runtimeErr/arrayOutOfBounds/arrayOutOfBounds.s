./valid/runtimeErr/arrayOutOfBounds/arrayOutOfBounds.wacc
calling the reference compiler on ./valid/runtimeErr/arrayOutOfBounds/arrayOutOfBounds.wacc
-- Test: arrayOutOfBounds.wacc

-- Uploaded file: 
---------------------------------------------------------------
# attempt out of bounds array access (this ought to seg fault or similar)

# Output:
# #runtime_error#

# Exit:
# 255

# Program:

begin

  int[] b = [1, 2, 3] ;
	int[] a = [43, 2, 18, 1] ;
	println a[5]
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
arrayOutOfBounds.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	.text
4	main:
5		push rbp
6		# push {rbx, r12, r13}
7		sub rsp, 24
8		mov qword ptr [rsp], rbx
9		mov qword ptr [rsp + 8], r12
10		mov qword ptr [rsp + 16], r13
11		mov rbp, rsp
12		# Stack pointer unchanged, no stack allocated variables
13		# 3 element array
14		mov edi, 16
15		call _malloc
16		mov r11, rax
17		# array pointers are shifted forwards by 4 bytes (to account for size)
18		mov r11, r11
19		add r11, 4
20		mov rax, 3
21		mov dword ptr [r11 - 4], eax
22		mov rax, 1
23		mov dword ptr [r11], eax
24		mov rax, 2
25		mov dword ptr [r11 + 4], eax
26		mov rax, 3
27		mov dword ptr [r11 + 8], eax
28		mov rax, r11
29		mov r12, rax
30		# 4 element array
31		mov edi, 20
32		call _malloc
33		mov r11, rax
34		# array pointers are shifted forwards by 4 bytes (to account for size)
35		mov r11, r11
36		add r11, 4
37		mov rax, 4
38		mov dword ptr [r11 - 4], eax
39		mov rax, 43
40		mov dword ptr [r11], eax
41		mov rax, 2
42		mov dword ptr [r11 + 4], eax
43		mov rax, 18
44		mov dword ptr [r11 + 8], eax
45		mov rax, 1
46		mov dword ptr [r11 + 12], eax
47		mov rax, r11
48		mov r13, rax
49		# Stack pointer unchanged, no stack allocated arguments
50		mov r10d, 5
51		mov r9, r13
52		call _arrLoad4
53		mov eax, r9d
54		mov rax, rax
55		mov rdi, rax
56		# statement primitives do not return results (but will clobber r0/rax)
57		call _printi
58		call _println
59		# Stack pointer unchanged, no stack allocated variables
60		mov rax, 0
61		# pop {rbx, r12, r13}
62		mov rbx, qword ptr [rsp]
63		mov r12, qword ptr [rsp + 8]
64		mov r13, qword ptr [rsp + 16]
65		add rsp, 24
66		pop rbp
67		ret
68	
69	.section .rodata
70	# length of .L._prints_str0
71		.int 4
72	.L._prints_str0:
73		.asciz "%.*s"
74	.text
75	_prints:
76		push rbp
77		mov rbp, rsp
78		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
79		and rsp, -16
80		mov rdx, rdi
81		mov esi, dword ptr [rdi - 4]
82		lea rdi, [rip + .L._prints_str0]
83		# on x86, al represents the number of SIMD registers used as variadic arguments
84		mov al, 0
85		call printf@plt
86		mov rdi, 0
87		call fflush@plt
88		mov rsp, rbp
89		pop rbp
90		ret
91	
92	_malloc:
93		push rbp
94		mov rbp, rsp
95		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
96		and rsp, -16
97		call malloc@plt
98		cmp rax, 0
99		je _errOutOfMemory
100		mov rsp, rbp
101		pop rbp
102		ret
103	
104	.section .rodata
105	# length of .L._printi_str0
106		.int 2
107	.L._printi_str0:
108		.asciz "%d"
109	.text
110	_printi:
111		push rbp
112		mov rbp, rsp
113		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
114		and rsp, -16
115		mov esi, edi
116		lea rdi, [rip + .L._printi_str0]
117		# on x86, al represents the number of SIMD registers used as variadic arguments
118		mov al, 0
119		call printf@plt
120		mov rdi, 0
121		call fflush@plt
122		mov rsp, rbp
123		pop rbp
124		ret
125	
126	.section .rodata
127	# length of .L._println_str0
128		.int 0
129	.L._println_str0:
130		.asciz ""
131	.text
132	_println:
133		push rbp
134		mov rbp, rsp
135		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
136		and rsp, -16
137		lea rdi, [rip + .L._println_str0]
138		call puts@plt
139		mov rdi, 0
140		call fflush@plt
141		mov rsp, rbp
142		pop rbp
143		ret
144	
145	_exit:
146		push rbp
147		mov rbp, rsp
148		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
149		and rsp, -16
150		call exit@plt
151		mov rsp, rbp
152		pop rbp
153		ret
154	
155	_arrLoad4:
156		# Special calling convention: array ptr passed in R9, index in R10, and return into R9
157		push rbx
158		cmp r10d, 0
159		cmovl esi, r10d
160		jl _errOutOfBounds
161		mov ebx, dword ptr [r9 - 4]
162		cmp r10d, ebx
163		cmovge esi, r10d
164		jge _errOutOfBounds
165		movsx r9, dword ptr [r9 + 4*r10]
166		pop rbx
167		ret
168	
169	.section .rodata
170	# length of .L._errOutOfMemory_str0
171		.int 27
172	.L._errOutOfMemory_str0:
173		.asciz "fatal error: out of memory\n"
174	.text
175	_errOutOfMemory:
176		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
177		and rsp, -16
178		lea rdi, [rip + .L._errOutOfMemory_str0]
179		call _prints
180		mov dil, -1
181		call exit@plt
182	
183	.section .rodata
184	# length of .L._errOutOfBounds_str0
185		.int 42
186	.L._errOutOfBounds_str0:
187		.asciz "fatal error: array index %d out of bounds\n"
188	.text
189	_errOutOfBounds:
190		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
191		and rsp, -16
192		lea rdi, [rip + .L._errOutOfBounds_str0]
193		# on x86, al represents the number of SIMD registers used as variadic arguments
194		mov al, 0
195		call printf@plt
196		mov rdi, 0
197		call fflush@plt
198		mov dil, -1
199		call exit@plt
===========================================================
-- Finished

