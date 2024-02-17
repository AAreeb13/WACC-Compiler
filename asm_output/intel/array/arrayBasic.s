./valid/array/arrayBasic.wacc
calling the reference compiler on ./valid/array/arrayBasic.wacc
-- Test: arrayBasic.wacc

-- Uploaded file: 
---------------------------------------------------------------
# basic array declaration and assignment

# Output:

# Program:

begin
  int[] a = [0]
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
arrayBasic.s contents are:
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
25		# Stack pointer unchanged, no stack allocated variables
26		mov rax, 0
27		# pop {rbx, r12}
28		mov rbx, qword ptr [rsp]
29		mov r12, qword ptr [rsp + 8]
30		add rsp, 16
31		pop rbp
32		ret
33	
34	.section .rodata
35	# length of .L._prints_str0
36		.int 4
37	.L._prints_str0:
38		.asciz "%.*s"
39	.text
40	_prints:
41		push rbp
42		mov rbp, rsp
43		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
44		and rsp, -16
45		mov rdx, rdi
46		mov esi, dword ptr [rdi - 4]
47		lea rdi, [rip + .L._prints_str0]
48		# on x86, al represents the number of SIMD registers used as variadic arguments
49		mov al, 0
50		call printf@plt
51		mov rdi, 0
52		call fflush@plt
53		mov rsp, rbp
54		pop rbp
55		ret
56	
57	_malloc:
58		push rbp
59		mov rbp, rsp
60		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
61		and rsp, -16
62		call malloc@plt
63		cmp rax, 0
64		je _errOutOfMemory
65		mov rsp, rbp
66		pop rbp
67		ret
68	
69	_exit:
70		push rbp
71		mov rbp, rsp
72		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
73		and rsp, -16
74		call exit@plt
75		mov rsp, rbp
76		pop rbp
77		ret
78	
79	.section .rodata
80	# length of .L._errOutOfMemory_str0
81		.int 27
82	.L._errOutOfMemory_str0:
83		.asciz "fatal error: out of memory\n"
84	.text
85	_errOutOfMemory:
86		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
87		and rsp, -16
88		lea rdi, [rip + .L._errOutOfMemory_str0]
89		call _prints
90		mov dil, -1
91		call exit@plt
===========================================================
-- Finished

