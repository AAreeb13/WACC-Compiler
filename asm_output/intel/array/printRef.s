./valid/array/printRef.wacc
calling the reference compiler on ./valid/array/printRef.wacc
-- Test: printRef.wacc

-- Uploaded file: 
---------------------------------------------------------------
# basic array (reference) printing

# Output:
# Printing an array variable gives an address, such as #addrs#
#

# Program:

begin
  print "Printing an array variable gives an address, such as " ;
  int[] a = [1,2,3] ;
  println a
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
printRef.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 53
5	.L.str0:
6		.asciz "Printing an array variable gives an address, such as "
7	.text
8	main:
9		push rbp
10		# push {rbx, r12}
11		sub rsp, 16
12		mov qword ptr [rsp], rbx
13		mov qword ptr [rsp + 8], r12
14		mov rbp, rsp
15		# Stack pointer unchanged, no stack allocated variables
16		# Stack pointer unchanged, no stack allocated arguments
17		lea rax, [rip + .L.str0]
18		push rax
19		pop rax
20		mov rax, rax
21		mov rdi, rax
22		# statement primitives do not return results (but will clobber r0/rax)
23		call _prints
24		# 3 element array
25		mov edi, 16
26		call _malloc
27		mov r11, rax
28		# array pointers are shifted forwards by 4 bytes (to account for size)
29		mov r11, r11
30		add r11, 4
31		mov rax, 3
32		mov dword ptr [r11 - 4], eax
33		mov rax, 1
34		mov dword ptr [r11], eax
35		mov rax, 2
36		mov dword ptr [r11 + 4], eax
37		mov rax, 3
38		mov dword ptr [r11 + 8], eax
39		mov rax, r11
40		mov r12, rax
41		# Stack pointer unchanged, no stack allocated arguments
42		mov rax, r12
43		mov rdi, rax
44		# statement primitives do not return results (but will clobber r0/rax)
45		call _printp
46		call _println
47		# Stack pointer unchanged, no stack allocated variables
48		mov rax, 0
49		# pop {rbx, r12}
50		mov rbx, qword ptr [rsp]
51		mov r12, qword ptr [rsp + 8]
52		add rsp, 16
53		pop rbp
54		ret
55	
56	.section .rodata
57	# length of .L._prints_str0
58		.int 4
59	.L._prints_str0:
60		.asciz "%.*s"
61	.text
62	_prints:
63		push rbp
64		mov rbp, rsp
65		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
66		and rsp, -16
67		mov rdx, rdi
68		mov esi, dword ptr [rdi - 4]
69		lea rdi, [rip + .L._prints_str0]
70		# on x86, al represents the number of SIMD registers used as variadic arguments
71		mov al, 0
72		call printf@plt
73		mov rdi, 0
74		call fflush@plt
75		mov rsp, rbp
76		pop rbp
77		ret
78	
79	.section .rodata
80	# length of .L._printp_str0
81		.int 2
82	.L._printp_str0:
83		.asciz "%p"
84	.text
85	_printp:
86		push rbp
87		mov rbp, rsp
88		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
89		and rsp, -16
90		mov rsi, rdi
91		lea rdi, [rip + .L._printp_str0]
92		# on x86, al represents the number of SIMD registers used as variadic arguments
93		mov al, 0
94		call printf@plt
95		mov rdi, 0
96		call fflush@plt
97		mov rsp, rbp
98		pop rbp
99		ret
100	
101	_malloc:
102		push rbp
103		mov rbp, rsp
104		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
105		and rsp, -16
106		call malloc@plt
107		cmp rax, 0
108		je _errOutOfMemory
109		mov rsp, rbp
110		pop rbp
111		ret
112	
113	.section .rodata
114	# length of .L._println_str0
115		.int 0
116	.L._println_str0:
117		.asciz ""
118	.text
119	_println:
120		push rbp
121		mov rbp, rsp
122		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
123		and rsp, -16
124		lea rdi, [rip + .L._println_str0]
125		call puts@plt
126		mov rdi, 0
127		call fflush@plt
128		mov rsp, rbp
129		pop rbp
130		ret
131	
132	_exit:
133		push rbp
134		mov rbp, rsp
135		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
136		and rsp, -16
137		call exit@plt
138		mov rsp, rbp
139		pop rbp
140		ret
141	
142	.section .rodata
143	# length of .L._errOutOfMemory_str0
144		.int 27
145	.L._errOutOfMemory_str0:
146		.asciz "fatal error: out of memory\n"
147	.text
148	_errOutOfMemory:
149		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
150		and rsp, -16
151		lea rdi, [rip + .L._errOutOfMemory_str0]
152		call _prints
153		mov dil, -1
154		call exit@plt
===========================================================
-- Finished

