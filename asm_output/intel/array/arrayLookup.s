./valid/array/arrayLookup.wacc
calling the reference compiler on ./valid/array/arrayLookup.wacc
-- Test: arrayLookup.wacc

-- Uploaded file: 
---------------------------------------------------------------
# check first element of array

# Output:
# 43
#

# Program:

begin
  int[] a = [43, 2, 18, 1] ;
  println a[0]
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
arrayLookup.s contents are:
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
12		# 4 element array
13		mov edi, 20
14		call _malloc
15		mov r11, rax
16		# array pointers are shifted forwards by 4 bytes (to account for size)
17		mov r11, r11
18		add r11, 4
19		mov rax, 4
20		mov dword ptr [r11 - 4], eax
21		mov rax, 43
22		mov dword ptr [r11], eax
23		mov rax, 2
24		mov dword ptr [r11 + 4], eax
25		mov rax, 18
26		mov dword ptr [r11 + 8], eax
27		mov rax, 1
28		mov dword ptr [r11 + 12], eax
29		mov rax, r11
30		mov r12, rax
31		# Stack pointer unchanged, no stack allocated arguments
32		mov r10d, 0
33		mov r9, r12
34		call _arrLoad4
35		mov eax, r9d
36		mov rax, rax
37		mov rdi, rax
38		# statement primitives do not return results (but will clobber r0/rax)
39		call _printi
40		call _println
41		# Stack pointer unchanged, no stack allocated variables
42		mov rax, 0
43		# pop {rbx, r12}
44		mov rbx, qword ptr [rsp]
45		mov r12, qword ptr [rsp + 8]
46		add rsp, 16
47		pop rbp
48		ret
49	
50	.section .rodata
51	# length of .L._prints_str0
52		.int 4
53	.L._prints_str0:
54		.asciz "%.*s"
55	.text
56	_prints:
57		push rbp
58		mov rbp, rsp
59		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
60		and rsp, -16
61		mov rdx, rdi
62		mov esi, dword ptr [rdi - 4]
63		lea rdi, [rip + .L._prints_str0]
64		# on x86, al represents the number of SIMD registers used as variadic arguments
65		mov al, 0
66		call printf@plt
67		mov rdi, 0
68		call fflush@plt
69		mov rsp, rbp
70		pop rbp
71		ret
72	
73	_malloc:
74		push rbp
75		mov rbp, rsp
76		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
77		and rsp, -16
78		call malloc@plt
79		cmp rax, 0
80		je _errOutOfMemory
81		mov rsp, rbp
82		pop rbp
83		ret
84	
85	.section .rodata
86	# length of .L._printi_str0
87		.int 2
88	.L._printi_str0:
89		.asciz "%d"
90	.text
91	_printi:
92		push rbp
93		mov rbp, rsp
94		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
95		and rsp, -16
96		mov esi, edi
97		lea rdi, [rip + .L._printi_str0]
98		# on x86, al represents the number of SIMD registers used as variadic arguments
99		mov al, 0
100		call printf@plt
101		mov rdi, 0
102		call fflush@plt
103		mov rsp, rbp
104		pop rbp
105		ret
106	
107	.section .rodata
108	# length of .L._println_str0
109		.int 0
110	.L._println_str0:
111		.asciz ""
112	.text
113	_println:
114		push rbp
115		mov rbp, rsp
116		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
117		and rsp, -16
118		lea rdi, [rip + .L._println_str0]
119		call puts@plt
120		mov rdi, 0
121		call fflush@plt
122		mov rsp, rbp
123		pop rbp
124		ret
125	
126	_exit:
127		push rbp
128		mov rbp, rsp
129		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
130		and rsp, -16
131		call exit@plt
132		mov rsp, rbp
133		pop rbp
134		ret
135	
136	_arrLoad4:
137		# Special calling convention: array ptr passed in R9, index in R10, and return into R9
138		push rbx
139		cmp r10d, 0
140		cmovl esi, r10d
141		jl _errOutOfBounds
142		mov ebx, dword ptr [r9 - 4]
143		cmp r10d, ebx
144		cmovge esi, r10d
145		jge _errOutOfBounds
146		movsx r9, dword ptr [r9 + 4*r10]
147		pop rbx
148		ret
149	
150	.section .rodata
151	# length of .L._errOutOfMemory_str0
152		.int 27
153	.L._errOutOfMemory_str0:
154		.asciz "fatal error: out of memory\n"
155	.text
156	_errOutOfMemory:
157		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
158		and rsp, -16
159		lea rdi, [rip + .L._errOutOfMemory_str0]
160		call _prints
161		mov dil, -1
162		call exit@plt
163	
164	.section .rodata
165	# length of .L._errOutOfBounds_str0
166		.int 42
167	.L._errOutOfBounds_str0:
168		.asciz "fatal error: array index %d out of bounds\n"
169	.text
170	_errOutOfBounds:
171		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
172		and rsp, -16
173		lea rdi, [rip + .L._errOutOfBounds_str0]
174		# on x86, al represents the number of SIMD registers used as variadic arguments
175		mov al, 0
176		call printf@plt
177		mov rdi, 0
178		call fflush@plt
179		mov dil, -1
180		call exit@plt
===========================================================
-- Finished

