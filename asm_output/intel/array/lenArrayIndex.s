./valid/array/lenArrayIndex.wacc
calling the reference compiler on ./valid/array/lenArrayIndex.wacc
-- Test: lenArrayIndex.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Tests that array length works on array indexes
# Thanks to Nixon Enraght-Moony for his help in this test-case

# Output:
# 0

# Program:

begin
  int[] arr = [];
  int[][] arrs = [arr];
  print len arrs[0]
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
lenArrayIndex.s contents are:
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
13		# 0 element array
14		mov edi, 4
15		call _malloc
16		mov r11, rax
17		# array pointers are shifted forwards by 4 bytes (to account for size)
18		mov r11, r11
19		add r11, 4
20		mov rax, 0
21		mov dword ptr [r11 - 4], eax
22		mov rax, r11
23		mov r12, rax
24		# 1 element array
25		mov edi, 12
26		call _malloc
27		mov r11, rax
28		# array pointers are shifted forwards by 4 bytes (to account for size)
29		mov r11, r11
30		add r11, 4
31		mov rax, 1
32		mov dword ptr [r11 - 4], eax
33		mov rax, r12
34		mov qword ptr [r11], rax
35		mov rax, r11
36		mov r13, rax
37		# Stack pointer unchanged, no stack allocated arguments
38		mov r10d, 0
39		mov r9, r13
40		call _arrLoad8
41		mov rax, r9
42		mov rax, rax
43		push rax
44		pop rax
45		movsx rax, dword ptr [rax - 4]
46		mov rax, rax
47		mov rdi, rax
48		# statement primitives do not return results (but will clobber r0/rax)
49		call _printi
50		# Stack pointer unchanged, no stack allocated variables
51		mov rax, 0
52		# pop {rbx, r12, r13}
53		mov rbx, qword ptr [rsp]
54		mov r12, qword ptr [rsp + 8]
55		mov r13, qword ptr [rsp + 16]
56		add rsp, 24
57		pop rbp
58		ret
59	
60	.section .rodata
61	# length of .L._prints_str0
62		.int 4
63	.L._prints_str0:
64		.asciz "%.*s"
65	.text
66	_prints:
67		push rbp
68		mov rbp, rsp
69		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
70		and rsp, -16
71		mov rdx, rdi
72		mov esi, dword ptr [rdi - 4]
73		lea rdi, [rip + .L._prints_str0]
74		# on x86, al represents the number of SIMD registers used as variadic arguments
75		mov al, 0
76		call printf@plt
77		mov rdi, 0
78		call fflush@plt
79		mov rsp, rbp
80		pop rbp
81		ret
82	
83	_malloc:
84		push rbp
85		mov rbp, rsp
86		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
87		and rsp, -16
88		call malloc@plt
89		cmp rax, 0
90		je _errOutOfMemory
91		mov rsp, rbp
92		pop rbp
93		ret
94	
95	.section .rodata
96	# length of .L._printi_str0
97		.int 2
98	.L._printi_str0:
99		.asciz "%d"
100	.text
101	_printi:
102		push rbp
103		mov rbp, rsp
104		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
105		and rsp, -16
106		mov esi, edi
107		lea rdi, [rip + .L._printi_str0]
108		# on x86, al represents the number of SIMD registers used as variadic arguments
109		mov al, 0
110		call printf@plt
111		mov rdi, 0
112		call fflush@plt
113		mov rsp, rbp
114		pop rbp
115		ret
116	
117	_exit:
118		push rbp
119		mov rbp, rsp
120		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
121		and rsp, -16
122		call exit@plt
123		mov rsp, rbp
124		pop rbp
125		ret
126	
127	_arrLoad8:
128		# Special calling convention: array ptr passed in R9, index in R10, and return into R9
129		push rbx
130		cmp r10d, 0
131		cmovl esi, r10d
132		jl _errOutOfBounds
133		mov ebx, dword ptr [r9 - 4]
134		cmp r10d, ebx
135		cmovge esi, r10d
136		jge _errOutOfBounds
137		mov r9, qword ptr [r9 + 8*r10]
138		pop rbx
139		ret
140	
141	.section .rodata
142	# length of .L._errOutOfMemory_str0
143		.int 27
144	.L._errOutOfMemory_str0:
145		.asciz "fatal error: out of memory\n"
146	.text
147	_errOutOfMemory:
148		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
149		and rsp, -16
150		lea rdi, [rip + .L._errOutOfMemory_str0]
151		call _prints
152		mov dil, -1
153		call exit@plt
154	
155	.section .rodata
156	# length of .L._errOutOfBounds_str0
157		.int 42
158	.L._errOutOfBounds_str0:
159		.asciz "fatal error: array index %d out of bounds\n"
160	.text
161	_errOutOfBounds:
162		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
163		and rsp, -16
164		lea rdi, [rip + .L._errOutOfBounds_str0]
165		# on x86, al represents the number of SIMD registers used as variadic arguments
166		mov al, 0
167		call printf@plt
168		mov rdi, 0
169		call fflush@plt
170		mov dil, -1
171		call exit@plt
===========================================================
-- Finished

