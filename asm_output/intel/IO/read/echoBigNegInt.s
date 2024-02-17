./valid/IO/read/echoBigNegInt.wacc
calling the reference compiler on ./valid/IO/read/echoBigNegInt.wacc
-- Test: echoBigNegInt.wacc

-- Uploaded file: 
---------------------------------------------------------------
# echo the user's input int

# Input: -2147483648

# Output:
# enter an integer to echo
# -2147483648
#

# Program:

begin
  int x = 1 ;
  println "enter an integer to echo";
  read x ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
echoBigNegInt.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 24
5	.L.str0:
6		.asciz "enter an integer to echo"
7	.text
8	main:
9		push rbp
10		# push {rbx, r12}
11		sub rsp, 16
12		mov qword ptr [rsp], rbx
13		mov qword ptr [rsp + 8], r12
14		mov rbp, rsp
15		# Stack pointer unchanged, no stack allocated variables
16		mov rax, 1
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
35		# Stack pointer unchanged, no stack allocated arguments
36		mov rax, r12
37		mov rdi, rax
38		# statement primitives do not return results (but will clobber r0/rax)
39		call _printi
40		call _println
41		# Stack pointer unchanged, no stack allocated variables
42		mov rax, 0
43		# pop {rbx, r12}
44		mov rbx, qword ptr [rsp]
45		mov r12, qword ptr [rsp + 8]
46		add rsp, 16
47		pop rbp
48		ret
49	
50	.section .rodata
51	# length of .L._prints_str0
52		.int 4
53	.L._prints_str0:
54		.asciz "%.*s"
55	.text
56	_prints:
57		push rbp
58		mov rbp, rsp
59		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
60		and rsp, -16
61		mov rdx, rdi
62		mov esi, dword ptr [rdi - 4]
63		lea rdi, [rip + .L._prints_str0]
64		# on x86, al represents the number of SIMD registers used as variadic arguments
65		mov al, 0
66		call printf@plt
67		mov rdi, 0
68		call fflush@plt
69		mov rsp, rbp
70		pop rbp
71		ret
72	
73	.section .rodata
74	# length of .L._readi_str0
75		.int 2
76	.L._readi_str0:
77		.asciz "%d"
78	.text
79	_readi:
80		push rbp
81		mov rbp, rsp
82		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
83		and rsp, -16
84		# RDI contains the "original" value of the destination of the read
85		# allocate space on the stack to store the read: preserve alignment!
86		# the passed default argument should be stored in case of EOF
87		sub rsp, 16
88		mov dword ptr [rsp], edi
89		lea rsi, qword ptr [rsp]
90		lea rdi, [rip + .L._readi_str0]
91		# on x86, al represents the number of SIMD registers used as variadic arguments
92		mov al, 0
93		call scanf@plt
94		movsx rax, dword ptr [rsp]
95		add rsp, 16
96		mov rsp, rbp
97		pop rbp
98		ret
99	
100	.section .rodata
101	# length of .L._printi_str0
102		.int 2
103	.L._printi_str0:
104		.asciz "%d"
105	.text
106	_printi:
107		push rbp
108		mov rbp, rsp
109		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
110		and rsp, -16
111		mov esi, edi
112		lea rdi, [rip + .L._printi_str0]
113		# on x86, al represents the number of SIMD registers used as variadic arguments
114		mov al, 0
115		call printf@plt
116		mov rdi, 0
117		call fflush@plt
118		mov rsp, rbp
119		pop rbp
120		ret
121	
122	.section .rodata
123	# length of .L._println_str0
124		.int 0
125	.L._println_str0:
126		.asciz ""
127	.text
128	_println:
129		push rbp
130		mov rbp, rsp
131		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
132		and rsp, -16
133		lea rdi, [rip + .L._println_str0]
134		call puts@plt
135		mov rdi, 0
136		call fflush@plt
137		mov rsp, rbp
138		pop rbp
139		ret
===========================================================
-- Finished

