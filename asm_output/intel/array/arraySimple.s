./valid/array/arraySimple.wacc
calling the reference compiler on ./valid/array/arraySimple.wacc
-- Test: arraySimple.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple array assignment and lookup

# Output:
# 42
#

# Program:

begin
  int[] a = [0] ;
  a[0] = 42 ;
  println a[0]
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
arraySimple.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	.text
4	main:
5		push rbp
6		# push {rbx, r12}
7		sub rsp, 16
8		mov qword ptr [rsp], rbx
9		mov qword ptr [rsp + 8], r12
10		mov rbp, rsp
11		# Stack pointer unchanged, no stack allocated variables
12		# 1 element array
13		mov edi, 8
14		call _malloc
15		mov r11, rax
16		# array pointers are shifted forwards by 4 bytes (to account for size)
17		mov r11, r11
18		add r11, 4
19		mov rax, 1
20		mov dword ptr [r11 - 4], eax
21		mov rax, 0
22		mov dword ptr [r11], eax
23		mov rax, r11
24		mov r12, rax
25		mov r10d, 0
26		mov rax, 42
27		mov r9, r12
28		call _arrStore4
29		# Stack pointer unchanged, no stack allocated arguments
30		mov r10d, 0
31		mov r9, r12
32		call _arrLoad4
33		mov eax, r9d
34		mov rax, rax
35		mov rdi, rax
36		# statement primitives do not return results (but will clobber r0/rax)
37		call _printi
38		call _println
39		# Stack pointer unchanged, no stack allocated variables
40		mov rax, 0
41		# pop {rbx, r12}
42		mov rbx, qword ptr [rsp]
43		mov r12, qword ptr [rsp + 8]
44		add rsp, 16
45		pop rbp
46		ret
47	
48	.section .rodata
49	# length of .L._prints_str0
50		.int 4
51	.L._prints_str0:
52		.asciz "%.*s"
53	.text
54	_prints:
55		push rbp
56		mov rbp, rsp
57		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
58		and rsp, -16
59		mov rdx, rdi
60		mov esi, dword ptr [rdi - 4]
61		lea rdi, [rip + .L._prints_str0]
62		# on x86, al represents the number of SIMD registers used as variadic arguments
63		mov al, 0
64		call printf@plt
65		mov rdi, 0
66		call fflush@plt
67		mov rsp, rbp
68		pop rbp
69		ret
70	
71	_malloc:
72		push rbp
73		mov rbp, rsp
74		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
75		and rsp, -16
76		call malloc@plt
77		cmp rax, 0
78		je _errOutOfMemory
79		mov rsp, rbp
80		pop rbp
81		ret
82	
83	.section .rodata
84	# length of .L._printi_str0
85		.int 2
86	.L._printi_str0:
87		.asciz "%d"
88	.text
89	_printi:
90		push rbp
91		mov rbp, rsp
92		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
93		and rsp, -16
94		mov esi, edi
95		lea rdi, [rip + .L._printi_str0]
96		# on x86, al represents the number of SIMD registers used as variadic arguments
97		mov al, 0
98		call printf@plt
99		mov rdi, 0
100		call fflush@plt
101		mov rsp, rbp
102		pop rbp
103		ret
104	
105	.section .rodata
106	# length of .L._println_str0
107		.int 0
108	.L._println_str0:
109		.asciz ""
110	.text
111	_println:
112		push rbp
113		mov rbp, rsp
114		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
115		and rsp, -16
116		lea rdi, [rip + .L._println_str0]
117		call puts@plt
118		mov rdi, 0
119		call fflush@plt
120		mov rsp, rbp
121		pop rbp
122		ret
123	
124	_exit:
125		push rbp
126		mov rbp, rsp
127		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
128		and rsp, -16
129		call exit@plt
130		mov rsp, rbp
131		pop rbp
132		ret
133	
134	_arrStore4:
135		# Special calling convention: array ptr passed in R9, index in R10, value to store in RAX
136		push rbx
137		cmp r10d, 0
138		cmovl esi, r10d
139		jl _errOutOfBounds
140		mov ebx, dword ptr [r9 - 4]
141		cmp r10d, ebx
142		cmovge esi, r10d
143		jge _errOutOfBounds
144		mov dword ptr [r9 + 4*r10], eax
145		pop rbx
146		ret
147	
148	_arrLoad4:
149		# Special calling convention: array ptr passed in R9, index in R10, and return into R9
150		push rbx
151		cmp r10d, 0
152		cmovl esi, r10d
153		jl _errOutOfBounds
154		mov ebx, dword ptr [r9 - 4]
155		cmp r10d, ebx
156		cmovge esi, r10d
157		jge _errOutOfBounds
158		movsx r9, dword ptr [r9 + 4*r10]
159		pop rbx
160		ret
161	
162	.section .rodata
163	# length of .L._errOutOfMemory_str0
164		.int 27
165	.L._errOutOfMemory_str0:
166		.asciz "fatal error: out of memory\n"
167	.text
168	_errOutOfMemory:
169		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
170		and rsp, -16
171		lea rdi, [rip + .L._errOutOfMemory_str0]
172		call _prints
173		mov dil, -1
174		call exit@plt
175	
176	.section .rodata
177	# length of .L._errOutOfBounds_str0
178		.int 42
179	.L._errOutOfBounds_str0:
180		.asciz "fatal error: array index %d out of bounds\n"
181	.text
182	_errOutOfBounds:
183		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
184		and rsp, -16
185		lea rdi, [rip + .L._errOutOfBounds_str0]
186		# on x86, al represents the number of SIMD registers used as variadic arguments
187		mov al, 0
188		call printf@plt
189		mov rdi, 0
190		call fflush@plt
191		mov dil, -1
192		call exit@plt
===========================================================
-- Finished

