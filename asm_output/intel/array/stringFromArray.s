./valid/array/stringFromArray.wacc
calling the reference compiler on ./valid/array/stringFromArray.wacc
-- Test: stringFromArray.wacc

-- Uploaded file: 
---------------------------------------------------------------
# char[] can be directly assigned to string

# Output:

# Program:

begin
  string a = ['a', 'b'] ;
  exit 0
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
stringFromArray.s contents are:
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
12		# 2 element array
13		mov edi, 6
14		call _malloc
15		mov r11, rax
16		# array pointers are shifted forwards by 4 bytes (to account for size)
17		mov r11, r11
18		add r11, 4
19		mov rax, 2
20		mov dword ptr [r11 - 4], eax
21		mov rax, 97
22		mov byte ptr [r11], al
23		mov rax, 98
24		mov byte ptr [r11 + 1], al
25		mov rax, r11
26		mov r12, rax
27		# Stack pointer unchanged, no stack allocated arguments
28		mov rax, 0
29		mov rdi, rax
30		# statement primitives do not return results (but will clobber r0/rax)
31		call _exit
32		# Stack pointer unchanged, no stack allocated variables
33		mov rax, 0
34		# pop {rbx, r12}
35		mov rbx, qword ptr [rsp]
36		mov r12, qword ptr [rsp + 8]
37		add rsp, 16
38		pop rbp
39		ret
40	
41	.section .rodata
42	# length of .L._prints_str0
43		.int 4
44	.L._prints_str0:
45		.asciz "%.*s"
46	.text
47	_prints:
48		push rbp
49		mov rbp, rsp
50		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
51		and rsp, -16
52		mov rdx, rdi
53		mov esi, dword ptr [rdi - 4]
54		lea rdi, [rip + .L._prints_str0]
55		# on x86, al represents the number of SIMD registers used as variadic arguments
56		mov al, 0
57		call printf@plt
58		mov rdi, 0
59		call fflush@plt
60		mov rsp, rbp
61		pop rbp
62		ret
63	
64	_malloc:
65		push rbp
66		mov rbp, rsp
67		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
68		and rsp, -16
69		call malloc@plt
70		cmp rax, 0
71		je _errOutOfMemory
72		mov rsp, rbp
73		pop rbp
74		ret
75	
76	_exit:
77		push rbp
78		mov rbp, rsp
79		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
80		and rsp, -16
81		call exit@plt
82		mov rsp, rbp
83		pop rbp
84		ret
85	
86	.section .rodata
87	# length of .L._errOutOfMemory_str0
88		.int 27
89	.L._errOutOfMemory_str0:
90		.asciz "fatal error: out of memory\n"
91	.text
92	_errOutOfMemory:
93		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
94		and rsp, -16
95		lea rdi, [rip + .L._errOutOfMemory_str0]
96		call _prints
97		mov dil, -1
98		call exit@plt
===========================================================
-- Finished

