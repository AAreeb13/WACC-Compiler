./valid/IO/read/read.wacc
calling the reference compiler on ./valid/IO/read/read.wacc
-- Test: read.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple read statement

#Input: 350

# Output:
# input an integer to continue...
#

# Program:

begin
	int x = 10;
  println "input an integer to continue..." ;
	read x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
read.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 31
5	.L.str0:
6		.asciz "input an integer to continue..."
7	.text
8	main:
9		push rbp
10		# push {rbx, r12}
11		sub rsp, 16
12		mov qword ptr [rsp], rbx
13		mov qword ptr [rsp + 8], r12
14		mov rbp, rsp
15		# Stack pointer unchanged, no stack allocated variables
16		mov rax, 10
17		mov r12, rax
18		# Stack pointer unchanged, no stack allocated arguments
19		lea rax, [rip + .L.str0]
20		push rax
21		pop rax
22		mov rax, rax
23		mov rdi, rax
24		# statement primitives do not return results (but will clobber r0/rax)
25		call _prints
26		call _println
27		# Stack pointer unchanged, no stack allocated arguments
28		# load the current value in the destination of the read so it supports defaults
29		mov rax, r12
30		mov rdi, rax
31		call _readi
32		mov r11, rax
33		mov rax, r11
34		mov r12, rax
35		# Stack pointer unchanged, no stack allocated variables
36		mov rax, 0
37		# pop {rbx, r12}
38		mov rbx, qword ptr [rsp]
39		mov r12, qword ptr [rsp + 8]
40		add rsp, 16
41		pop rbp
42		ret
43	
44	.section .rodata
45	# length of .L._prints_str0
46		.int 4
47	.L._prints_str0:
48		.asciz "%.*s"
49	.text
50	_prints:
51		push rbp
52		mov rbp, rsp
53		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
54		and rsp, -16
55		mov rdx, rdi
56		mov esi, dword ptr [rdi - 4]
57		lea rdi, [rip + .L._prints_str0]
58		# on x86, al represents the number of SIMD registers used as variadic arguments
59		mov al, 0
60		call printf@plt
61		mov rdi, 0
62		call fflush@plt
63		mov rsp, rbp
64		pop rbp
65		ret
66	
67	.section .rodata
68	# length of .L._readi_str0
69		.int 2
70	.L._readi_str0:
71		.asciz "%d"
72	.text
73	_readi:
74		push rbp
75		mov rbp, rsp
76		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
77		and rsp, -16
78		# RDI contains the "original" value of the destination of the read
79		# allocate space on the stack to store the read: preserve alignment!
80		# the passed default argument should be stored in case of EOF
81		sub rsp, 16
82		mov dword ptr [rsp], edi
83		lea rsi, qword ptr [rsp]
84		lea rdi, [rip + .L._readi_str0]
85		# on x86, al represents the number of SIMD registers used as variadic arguments
86		mov al, 0
87		call scanf@plt
88		movsx rax, dword ptr [rsp]
89		add rsp, 16
90		mov rsp, rbp
91		pop rbp
92		ret
93	
94	.section .rodata
95	# length of .L._println_str0
96		.int 0
97	.L._println_str0:
98		.asciz ""
99	.text
100	_println:
101		push rbp
102		mov rbp, rsp
103		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
104		and rsp, -16
105		lea rdi, [rip + .L._println_str0]
106		call puts@plt
107		mov rdi, 0
108		call fflush@plt
109		mov rsp, rbp
110		pop rbp
111		ret
===========================================================
-- Finished

