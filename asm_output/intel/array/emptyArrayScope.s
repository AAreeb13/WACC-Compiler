./valid/array/emptyArrayScope.wacc
calling the reference compiler on ./valid/array/emptyArrayScope.wacc
-- Test: emptyArrayScope.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Scoping shouldn't affect arrays

# Output:

# Program:

begin
  int[] x = [] ;
  begin
    bool y = true
  end
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
emptyArrayScope.s contents are:
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
24		# Stack pointer unchanged, no stack allocated variables
25		mov rax, 1
26		mov r13, rax
27		# Stack pointer unchanged, no stack allocated variables
28		# Stack pointer unchanged, no stack allocated variables
29		mov rax, 0
30		# pop {rbx, r12, r13}
31		mov rbx, qword ptr [rsp]
32		mov r12, qword ptr [rsp + 8]
33		mov r13, qword ptr [rsp + 16]
34		add rsp, 24
35		pop rbp
36		ret
37	
38	.section .rodata
39	# length of .L._prints_str0
40		.int 4
41	.L._prints_str0:
42		.asciz "%.*s"
43	.text
44	_prints:
45		push rbp
46		mov rbp, rsp
47		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
48		and rsp, -16
49		mov rdx, rdi
50		mov esi, dword ptr [rdi - 4]
51		lea rdi, [rip + .L._prints_str0]
52		# on x86, al represents the number of SIMD registers used as variadic arguments
53		mov al, 0
54		call printf@plt
55		mov rdi, 0
56		call fflush@plt
57		mov rsp, rbp
58		pop rbp
59		ret
60	
61	_malloc:
62		push rbp
63		mov rbp, rsp
64		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
65		and rsp, -16
66		call malloc@plt
67		cmp rax, 0
68		je _errOutOfMemory
69		mov rsp, rbp
70		pop rbp
71		ret
72	
73	_exit:
74		push rbp
75		mov rbp, rsp
76		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
77		and rsp, -16
78		call exit@plt
79		mov rsp, rbp
80		pop rbp
81		ret
82	
83	.section .rodata
84	# length of .L._errOutOfMemory_str0
85		.int 27
86	.L._errOutOfMemory_str0:
87		.asciz "fatal error: out of memory\n"
88	.text
89	_errOutOfMemory:
90		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
91		and rsp, -16
92		lea rdi, [rip + .L._errOutOfMemory_str0]
93		call _prints
94		mov dil, -1
95		call exit@plt
===========================================================
-- Finished

