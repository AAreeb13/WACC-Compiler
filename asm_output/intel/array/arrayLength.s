./valid/array/arrayLength.wacc
calling the reference compiler on ./valid/array/arrayLength.wacc
-- Test: arrayLength.wacc

-- Uploaded file: 
---------------------------------------------------------------
# check length of array

# Output:
# 4
#

# Program:

begin
  int[] a = [43, 2, 18, 1] ;
  println len a
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
arrayLength.s contents are:
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
32		movsx rax, dword ptr [r12 - 4]
33		mov rax, rax
34		mov rdi, rax
35		# statement primitives do not return results (but will clobber r0/rax)
36		call _printi
37		call _println
38		# Stack pointer unchanged, no stack allocated variables
39		mov rax, 0
40		# pop {rbx, r12}
41		mov rbx, qword ptr [rsp]
42		mov r12, qword ptr [rsp + 8]
43		add rsp, 16
44		pop rbp
45		ret
46	
47	.section .rodata
48	# length of .L._prints_str0
49		.int 4
50	.L._prints_str0:
51		.asciz "%.*s"
52	.text
53	_prints:
54		push rbp
55		mov rbp, rsp
56		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
57		and rsp, -16
58		mov rdx, rdi
59		mov esi, dword ptr [rdi - 4]
60		lea rdi, [rip + .L._prints_str0]
61		# on x86, al represents the number of SIMD registers used as variadic arguments
62		mov al, 0
63		call printf@plt
64		mov rdi, 0
65		call fflush@plt
66		mov rsp, rbp
67		pop rbp
68		ret
69	
70	_malloc:
71		push rbp
72		mov rbp, rsp
73		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
74		and rsp, -16
75		call malloc@plt
76		cmp rax, 0
77		je _errOutOfMemory
78		mov rsp, rbp
79		pop rbp
80		ret
81	
82	.section .rodata
83	# length of .L._printi_str0
84		.int 2
85	.L._printi_str0:
86		.asciz "%d"
87	.text
88	_printi:
89		push rbp
90		mov rbp, rsp
91		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
92		and rsp, -16
93		mov esi, edi
94		lea rdi, [rip + .L._printi_str0]
95		# on x86, al represents the number of SIMD registers used as variadic arguments
96		mov al, 0
97		call printf@plt
98		mov rdi, 0
99		call fflush@plt
100		mov rsp, rbp
101		pop rbp
102		ret
103	
104	.section .rodata
105	# length of .L._println_str0
106		.int 0
107	.L._println_str0:
108		.asciz ""
109	.text
110	_println:
111		push rbp
112		mov rbp, rsp
113		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
114		and rsp, -16
115		lea rdi, [rip + .L._println_str0]
116		call puts@plt
117		mov rdi, 0
118		call fflush@plt
119		mov rsp, rbp
120		pop rbp
121		ret
122	
123	_exit:
124		push rbp
125		mov rbp, rsp
126		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
127		and rsp, -16
128		call exit@plt
129		mov rsp, rbp
130		pop rbp
131		ret
132	
133	.section .rodata
134	# length of .L._errOutOfMemory_str0
135		.int 27
136	.L._errOutOfMemory_str0:
137		.asciz "fatal error: out of memory\n"
138	.text
139	_errOutOfMemory:
140		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
141		and rsp, -16
142		lea rdi, [rip + .L._errOutOfMemory_str0]
143		call _prints
144		mov dil, -1
145		call exit@plt
===========================================================
-- Finished

