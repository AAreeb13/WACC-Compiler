./valid/array/emptyArrayAloneIsFine.wacc
calling the reference compiler on ./valid/array/emptyArrayAloneIsFine.wacc
-- Test: emptyArrayAloneIsFine.wacc

-- Uploaded file: 
---------------------------------------------------------------
# this is fine

# Output:

# Program:

begin
  int[] x = []
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
emptyArrayAloneIsFine.s contents are:
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
12		# 0 element array
13		mov edi, 4
14		call _malloc
15		mov r11, rax
16		# array pointers are shifted forwards by 4 bytes (to account for size)
17		mov r11, r11
18		add r11, 4
19		mov rax, 0
20		mov dword ptr [r11 - 4], eax
21		mov rax, r11
22		mov r12, rax
23		# Stack pointer unchanged, no stack allocated variables
24		mov rax, 0
25		# pop {rbx, r12}
26		mov rbx, qword ptr [rsp]
27		mov r12, qword ptr [rsp + 8]
28		add rsp, 16
29		pop rbp
30		ret
31	
32	.section .rodata
33	# length of .L._prints_str0
34		.int 4
35	.L._prints_str0:
36		.asciz "%.*s"
37	.text
38	_prints:
39		push rbp
40		mov rbp, rsp
41		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
42		and rsp, -16
43		mov rdx, rdi
44		mov esi, dword ptr [rdi - 4]
45		lea rdi, [rip + .L._prints_str0]
46		# on x86, al represents the number of SIMD registers used as variadic arguments
47		mov al, 0
48		call printf@plt
49		mov rdi, 0
50		call fflush@plt
51		mov rsp, rbp
52		pop rbp
53		ret
54	
55	_malloc:
56		push rbp
57		mov rbp, rsp
58		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
59		and rsp, -16
60		call malloc@plt
61		cmp rax, 0
62		je _errOutOfMemory
63		mov rsp, rbp
64		pop rbp
65		ret
66	
67	_exit:
68		push rbp
69		mov rbp, rsp
70		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
71		and rsp, -16
72		call exit@plt
73		mov rsp, rbp
74		pop rbp
75		ret
76	
77	.section .rodata
78	# length of .L._errOutOfMemory_str0
79		.int 27
80	.L._errOutOfMemory_str0:
81		.asciz "fatal error: out of memory\n"
82	.text
83	_errOutOfMemory:
84		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
85		and rsp, -16
86		lea rdi, [rip + .L._errOutOfMemory_str0]
87		call _prints
88		mov dil, -1
89		call exit@plt
===========================================================
-- Finished

