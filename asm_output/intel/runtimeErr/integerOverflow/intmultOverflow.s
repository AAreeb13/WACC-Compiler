./valid/runtimeErr/integerOverflow/intmultOverflow.wacc
calling the reference compiler on ./valid/runtimeErr/integerOverflow/intmultOverflow.wacc
-- Test: intmultOverflow.wacc

-- Uploaded file: 
---------------------------------------------------------------
# integer overflow - generates odd assembly error!

# Output:
# 2147483
# 2147483000
# #runtime_error#

# Exit:
# 255

# Program:

begin
  int x = 2147483 ;
  println x ;
  x = x * 1000 ;
  println x ;
  x = x * 1000 ;
  println x ;
  x = x * 1000 ; #err here?
  println x 
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
intmultOverflow.s contents are:
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
12		mov rax, 2147483
13		mov r12, rax
14		# Stack pointer unchanged, no stack allocated arguments
15		mov rax, r12
16		mov rdi, rax
17		# statement primitives do not return results (but will clobber r0/rax)
18		call _printi
19		call _println
20		imul eax, r12d, 1000
21		jo _errOverflow
22		movsx rax, eax
23		push rax
24		pop rax
25		mov rax, rax
26		mov r12, rax
27		# Stack pointer unchanged, no stack allocated arguments
28		mov rax, r12
29		mov rdi, rax
30		# statement primitives do not return results (but will clobber r0/rax)
31		call _printi
32		call _println
33		imul eax, r12d, 1000
34		jo _errOverflow
35		movsx rax, eax
36		push rax
37		pop rax
38		mov rax, rax
39		mov r12, rax
40		# Stack pointer unchanged, no stack allocated arguments
41		mov rax, r12
42		mov rdi, rax
43		# statement primitives do not return results (but will clobber r0/rax)
44		call _printi
45		call _println
46		imul eax, r12d, 1000
47		jo _errOverflow
48		movsx rax, eax
49		push rax
50		pop rax
51		mov rax, rax
52		mov r12, rax
53		# Stack pointer unchanged, no stack allocated arguments
54		mov rax, r12
55		mov rdi, rax
56		# statement primitives do not return results (but will clobber r0/rax)
57		call _printi
58		call _println
59		# Stack pointer unchanged, no stack allocated variables
60		mov rax, 0
61		# pop {rbx, r12}
62		mov rbx, qword ptr [rsp]
63		mov r12, qword ptr [rsp + 8]
64		add rsp, 16
65		pop rbp
66		ret
67	
68	.section .rodata
69	# length of .L._prints_str0
70		.int 4
71	.L._prints_str0:
72		.asciz "%.*s"
73	.text
74	_prints:
75		push rbp
76		mov rbp, rsp
77		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
78		and rsp, -16
79		mov rdx, rdi
80		mov esi, dword ptr [rdi - 4]
81		lea rdi, [rip + .L._prints_str0]
82		# on x86, al represents the number of SIMD registers used as variadic arguments
83		mov al, 0
84		call printf@plt
85		mov rdi, 0
86		call fflush@plt
87		mov rsp, rbp
88		pop rbp
89		ret
90	
91	.section .rodata
92	# length of .L._printi_str0
93		.int 2
94	.L._printi_str0:
95		.asciz "%d"
96	.text
97	_printi:
98		push rbp
99		mov rbp, rsp
100		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
101		and rsp, -16
102		mov esi, edi
103		lea rdi, [rip + .L._printi_str0]
104		# on x86, al represents the number of SIMD registers used as variadic arguments
105		mov al, 0
106		call printf@plt
107		mov rdi, 0
108		call fflush@plt
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
143	# length of .L._errOverflow_str0
144		.int 52
145	.L._errOverflow_str0:
146		.asciz "fatal error: integer overflow or underflow occurred\n"
147	.text
148	_errOverflow:
149		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
150		and rsp, -16
151		lea rdi, [rip + .L._errOverflow_str0]
152		call _prints
153		mov dil, -1
154		call exit@plt
===========================================================
-- Finished

