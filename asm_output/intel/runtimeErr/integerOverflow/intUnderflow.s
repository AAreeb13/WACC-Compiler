./valid/runtimeErr/integerOverflow/intUnderflow.wacc
calling the reference compiler on ./valid/runtimeErr/integerOverflow/intUnderflow.wacc
-- Test: intUnderflow.wacc

-- Uploaded file: 
---------------------------------------------------------------
# integer underflow

# Output:
# -2147483647
# -2147483648
# #runtime_error#

# Exit:
# 255

# Program:

begin
  int x = -2147483647 ;
  println x ;
  x = x - 1 ;
  println x ;
  x = x - 1 ; #err here?
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
intUnderflow.s contents are:
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
12		mov rax, -2147483647
13		mov r12, rax
14		# Stack pointer unchanged, no stack allocated arguments
15		mov rax, r12
16		mov rdi, rax
17		# statement primitives do not return results (but will clobber r0/rax)
18		call _printi
19		call _println
20		mov eax, r12d
21		sub eax, 1
22		jo _errOverflow
23		movsx rax, eax
24		push rax
25		pop rax
26		mov rax, rax
27		mov r12, rax
28		# Stack pointer unchanged, no stack allocated arguments
29		mov rax, r12
30		mov rdi, rax
31		# statement primitives do not return results (but will clobber r0/rax)
32		call _printi
33		call _println
34		mov eax, r12d
35		sub eax, 1
36		jo _errOverflow
37		movsx rax, eax
38		push rax
39		pop rax
40		mov rax, rax
41		mov r12, rax
42		# Stack pointer unchanged, no stack allocated arguments
43		mov rax, r12
44		mov rdi, rax
45		# statement primitives do not return results (but will clobber r0/rax)
46		call _printi
47		call _println
48		# Stack pointer unchanged, no stack allocated variables
49		mov rax, 0
50		# pop {rbx, r12}
51		mov rbx, qword ptr [rsp]
52		mov r12, qword ptr [rsp + 8]
53		add rsp, 16
54		pop rbp
55		ret
56	
57	.section .rodata
58	# length of .L._prints_str0
59		.int 4
60	.L._prints_str0:
61		.asciz "%.*s"
62	.text
63	_prints:
64		push rbp
65		mov rbp, rsp
66		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
67		and rsp, -16
68		mov rdx, rdi
69		mov esi, dword ptr [rdi - 4]
70		lea rdi, [rip + .L._prints_str0]
71		# on x86, al represents the number of SIMD registers used as variadic arguments
72		mov al, 0
73		call printf@plt
74		mov rdi, 0
75		call fflush@plt
76		mov rsp, rbp
77		pop rbp
78		ret
79	
80	.section .rodata
81	# length of .L._printi_str0
82		.int 2
83	.L._printi_str0:
84		.asciz "%d"
85	.text
86	_printi:
87		push rbp
88		mov rbp, rsp
89		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
90		and rsp, -16
91		mov esi, edi
92		lea rdi, [rip + .L._printi_str0]
93		# on x86, al represents the number of SIMD registers used as variadic arguments
94		mov al, 0
95		call printf@plt
96		mov rdi, 0
97		call fflush@plt
98		mov rsp, rbp
99		pop rbp
100		ret
101	
102	.section .rodata
103	# length of .L._println_str0
104		.int 0
105	.L._println_str0:
106		.asciz ""
107	.text
108	_println:
109		push rbp
110		mov rbp, rsp
111		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
112		and rsp, -16
113		lea rdi, [rip + .L._println_str0]
114		call puts@plt
115		mov rdi, 0
116		call fflush@plt
117		mov rsp, rbp
118		pop rbp
119		ret
120	
121	_exit:
122		push rbp
123		mov rbp, rsp
124		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
125		and rsp, -16
126		call exit@plt
127		mov rsp, rbp
128		pop rbp
129		ret
130	
131	.section .rodata
132	# length of .L._errOverflow_str0
133		.int 52
134	.L._errOverflow_str0:
135		.asciz "fatal error: integer overflow or underflow occurred\n"
136	.text
137	_errOverflow:
138		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
139		and rsp, -16
140		lea rdi, [rip + .L._errOverflow_str0]
141		call _prints
142		mov dil, -1
143		call exit@plt
===========================================================
-- Finished

