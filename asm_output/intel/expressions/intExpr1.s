./valid/expressions/intExpr1.wacc
calling the reference compiler on ./valid/expressions/intExpr1.wacc
-- Test: intExpr1.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating a moderately complex integer expression

# Output:
# Correct
#

# Program:

begin
  int a = ( 10 * 1 + 2 * 15 );
  if a == 40 then
    println "Correct"
  else
    println "Wrong"
  fi
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
intExpr1.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 5
5	.L.str0:
6		.asciz "Wrong"
7	# length of .L.str1
8		.int 7
9	.L.str1:
10		.asciz "Correct"
11	.text
12	main:
13		push rbp
14		# push {rbx, r12}
15		sub rsp, 16
16		mov qword ptr [rsp], rbx
17		mov qword ptr [rsp + 8], r12
18		mov rbp, rsp
19		# Stack pointer unchanged, no stack allocated variables
20		mov rax, 10
21		imul eax, eax, 1
22		jo _errOverflow
23		movsx rax, eax
24		push rax
25		mov rax, 2
26		imul eax, eax, 15
27		jo _errOverflow
28		movsx rax, eax
29		push rax
30		pop rbx
31		pop rax
32		mov eax, eax
33		add eax, ebx
34		jo _errOverflow
35		movsx rax, eax
36		push rax
37		pop rax
38		mov rax, rax
39		mov r12, rax
40		cmp r12, 40
41		je .L0
42		# Stack pointer unchanged, no stack allocated arguments
43		lea rax, [rip + .L.str0]
44		push rax
45		pop rax
46		mov rax, rax
47		mov rdi, rax
48		# statement primitives do not return results (but will clobber r0/rax)
49		call _prints
50		call _println
51		jmp .L1
52	.L0:
53		# Stack pointer unchanged, no stack allocated arguments
54		lea rax, [rip + .L.str1]
55		push rax
56		pop rax
57		mov rax, rax
58		mov rdi, rax
59		# statement primitives do not return results (but will clobber r0/rax)
60		call _prints
61		call _println
62	.L1:
63		# Stack pointer unchanged, no stack allocated variables
64		mov rax, 0
65		# pop {rbx, r12}
66		mov rbx, qword ptr [rsp]
67		mov r12, qword ptr [rsp + 8]
68		add rsp, 16
69		pop rbp
70		ret
71	
72	.section .rodata
73	# length of .L._prints_str0
74		.int 4
75	.L._prints_str0:
76		.asciz "%.*s"
77	.text
78	_prints:
79		push rbp
80		mov rbp, rsp
81		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
82		and rsp, -16
83		mov rdx, rdi
84		mov esi, dword ptr [rdi - 4]
85		lea rdi, [rip + .L._prints_str0]
86		# on x86, al represents the number of SIMD registers used as variadic arguments
87		mov al, 0
88		call printf@plt
89		mov rdi, 0
90		call fflush@plt
91		mov rsp, rbp
92		pop rbp
93		ret
94	
95	.section .rodata
96	# length of .L._println_str0
97		.int 0
98	.L._println_str0:
99		.asciz ""
100	.text
101	_println:
102		push rbp
103		mov rbp, rsp
104		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
105		and rsp, -16
106		lea rdi, [rip + .L._println_str0]
107		call puts@plt
108		mov rdi, 0
109		call fflush@plt
110		mov rsp, rbp
111		pop rbp
112		ret
113	
114	_exit:
115		push rbp
116		mov rbp, rsp
117		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
118		and rsp, -16
119		call exit@plt
120		mov rsp, rbp
121		pop rbp
122		ret
123	
124	.section .rodata
125	# length of .L._errOverflow_str0
126		.int 52
127	.L._errOverflow_str0:
128		.asciz "fatal error: integer overflow or underflow occurred\n"
129	.text
130	_errOverflow:
131		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
132		and rsp, -16
133		lea rdi, [rip + .L._errOverflow_str0]
134		call _prints
135		mov dil, -1
136		call exit@plt
===========================================================
-- Finished

