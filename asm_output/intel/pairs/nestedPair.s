./valid/pairs/nestedPair.wacc
calling the reference compiler on ./valid/pairs/nestedPair.wacc
-- Test: nestedPair.wacc

-- Uploaded file: 
---------------------------------------------------------------
# create a pair (int, pair (int, int) )

# Output:

# Program:

begin
  pair(int, int) p = newpair(2, 3) ;
  pair(int, pair) q = newpair(1, p)
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
nestedPair.s contents are:
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
13		mov edi, 16
14		call _malloc
15		mov r11, rax
16		mov rax, 2
17		mov qword ptr [r11], rax
18		mov rax, 3
19		mov qword ptr [r11 + 8], rax
20		mov rax, r11
21		mov r12, rax
22		mov edi, 16
23		call _malloc
24		mov r11, rax
25		mov rax, 1
26		mov qword ptr [r11], rax
27		mov rax, r12
28		mov qword ptr [r11 + 8], rax
29		mov rax, r11
30		mov r13, rax
31		# Stack pointer unchanged, no stack allocated variables
32		mov rax, 0
33		# pop {rbx, r12, r13}
34		mov rbx, qword ptr [rsp]
35		mov r12, qword ptr [rsp + 8]
36		mov r13, qword ptr [rsp + 16]
37		add rsp, 24
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

