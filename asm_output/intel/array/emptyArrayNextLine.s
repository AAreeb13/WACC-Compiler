./valid/array/emptyArrayNextLine.wacc
calling the reference compiler on ./valid/array/emptyArrayNextLine.wacc
-- Test: emptyArrayNextLine.wacc

-- Uploaded file: 
---------------------------------------------------------------
# This should work just fine

# Output:

# Program:

begin
  int[] x = [] ;
  bool y = true
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
emptyArrayNextLine.s contents are:
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
24		mov rax, 1
25		mov r13, rax
26		# Stack pointer unchanged, no stack allocated variables
27		mov rax, 0
28		# pop {rbx, r12, r13}
29		mov rbx, qword ptr [rsp]
30		mov r12, qword ptr [rsp + 8]
31		mov r13, qword ptr [rsp + 16]
32		add rsp, 24
33		pop rbp
34		ret
35	
36	.section .rodata
37	# length of .L._prints_str0
38		.int 4
39	.L._prints_str0:
40		.asciz "%.*s"
41	.text
42	_prints:
43		push rbp
44		mov rbp, rsp
45		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
46		and rsp, -16
47		mov rdx, rdi
48		mov esi, dword ptr [rdi - 4]
49		lea rdi, [rip + .L._prints_str0]
50		# on x86, al represents the number of SIMD registers used as variadic arguments
51		mov al, 0
52		call printf@plt
53		mov rdi, 0
54		call fflush@plt
55		mov rsp, rbp
56		pop rbp
57		ret
58	
59	_malloc:
60		push rbp
61		mov rbp, rsp
62		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
63		and rsp, -16
64		call malloc@plt
65		cmp rax, 0
66		je _errOutOfMemory
67		mov rsp, rbp
68		pop rbp
69		ret
70	
71	_exit:
72		push rbp
73		mov rbp, rsp
74		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
75		and rsp, -16
76		call exit@plt
77		mov rsp, rbp
78		pop rbp
79		ret
80	
81	.section .rodata
82	# length of .L._errOutOfMemory_str0
83		.int 27
84	.L._errOutOfMemory_str0:
85		.asciz "fatal error: out of memory\n"
86	.text
87	_errOutOfMemory:
88		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
89		and rsp, -16
90		lea rdi, [rip + .L._errOutOfMemory_str0]
91		call _prints
92		mov dil, -1
93		call exit@plt
===========================================================
-- Finished

