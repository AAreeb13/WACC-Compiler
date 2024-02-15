./valid/runtimeErr/nullDereference/readNull2.wacc
calling the reference compiler on ./valid/runtimeErr/nullDereference/readNull2.wacc
-- Test: readNull2.wacc

-- Uploaded file: 
---------------------------------------------------------------
# attempt dereference of a null pair by reading into an element of it

# Output:
# #runtime_error#

# Exit:
# 255

# Program:

begin
	pair(int, int) p = null ;
	read snd p
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
readNull2.s contents are:
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
12		mov rax, 0
13		mov r12, rax
14		# Stack pointer unchanged, no stack allocated arguments
15		# load the current value in the destination of the read so it supports defaults
16		cmp r12, 0
17		je _errNull
18		mov rax, qword ptr [r12 + 8]
19		mov rax, rax
20		mov rdi, rax
21		call _readi
22		mov r11, rax
23		cmp r12, 0
24		je _errNull
25		mov rax, r11
26		mov qword ptr [r12 + 8], rax
27		# Stack pointer unchanged, no stack allocated variables
28		mov rax, 0
29		# pop {rbx, r12}
30		mov rbx, qword ptr [rsp]
31		mov r12, qword ptr [rsp + 8]
32		add rsp, 16
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
59	.section .rodata
60	# length of .L._readi_str0
61		.int 2
62	.L._readi_str0:
63		.asciz "%d"
64	.text
65	_readi:
66		push rbp
67		mov rbp, rsp
68		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
69		and rsp, -16
70		# RDI contains the "original" value of the destination of the read
71		# allocate space on the stack to store the read: preserve alignment!
72		# the passed default argument should be stored in case of EOF
73		sub rsp, 16
74		mov dword ptr [rsp], edi
75		lea rsi, qword ptr [rsp]
76		lea rdi, [rip + .L._readi_str0]
77		# on x86, al represents the number of SIMD registers used as variadic arguments
78		mov al, 0
79		call scanf@plt
80		movsx rax, dword ptr [rsp]
81		add rsp, 16
82		mov rsp, rbp
83		pop rbp
84		ret
85	
86	_exit:
87		push rbp
88		mov rbp, rsp
89		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
90		and rsp, -16
91		call exit@plt
92		mov rsp, rbp
93		pop rbp
94		ret
95	
96	.section .rodata
97	# length of .L._errNull_str0
98		.int 45
99	.L._errNull_str0:
100		.asciz "fatal error: null pair dereferenced or freed\n"
101	.text
102	_errNull:
103		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
104		and rsp, -16
105		lea rdi, [rip + .L._errNull_str0]
106		call _prints
107		mov dil, -1
108		call exit@plt
===========================================================
-- Finished

