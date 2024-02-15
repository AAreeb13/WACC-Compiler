./valid/array/free.wacc
calling the reference compiler on ./valid/array/free.wacc
-- Test: free.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Create and free an array

# Output:

# Program:

begin
  int[] arr = [1, 2, 3] ;
  free arr
end


---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
free.s contents are:
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
12		# 3 element array
13		mov edi, 16
14		call _malloc
15		mov r11, rax
16		# array pointers are shifted forwards by 4 bytes (to account for size)
17		mov r11, r11
18		add r11, 4
19		mov rax, 3
20		mov dword ptr [r11 - 4], eax
21		mov rax, 1
22		mov dword ptr [r11], eax
23		mov rax, 2
24		mov dword ptr [r11 + 4], eax
25		mov rax, 3
26		mov dword ptr [r11 + 8], eax
27		mov rax, r11
28		mov r12, rax
29		# Stack pointer unchanged, no stack allocated arguments
30		# array pointers are shifted forward by 4 bytes, so correct it back to original pointer before free
31		mov rax, r12
32		sub rax, 4
33		push rax
34		pop rax
35		mov rax, rax
36		mov rdi, rax
37		# statement primitives do not return results (but will clobber r0/rax)
38		call _free
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
83	_free:
84		push rbp
85		mov rbp, rsp
86		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
87		and rsp, -16
88		call free@plt
89		mov rsp, rbp
90		pop rbp
91		ret
92	
93	_exit:
94		push rbp
95		mov rbp, rsp
96		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
97		and rsp, -16
98		call exit@plt
99		mov rsp, rbp
100		pop rbp
101		ret
102	
103	.section .rodata
104	# length of .L._errOutOfMemory_str0
105		.int 27
106	.L._errOutOfMemory_str0:
107		.asciz "fatal error: out of memory\n"
108	.text
109	_errOutOfMemory:
110		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
111		and rsp, -16
112		lea rdi, [rip + .L._errOutOfMemory_str0]
113		call _prints
114		mov dil, -1
115		call exit@plt
===========================================================
-- Finished

