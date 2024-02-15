./valid/while/whileCount.wacc
calling the reference compiler on ./valid/while/whileCount.wacc
-- Test: whileCount.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple counting while loop

# Output:
# Can you count to 10?
# 1
# 2
# 3
# 4
# 5
# 6
# 7
# 8
# 9
# 10
#

# Program:

begin
  int i = 1 ;
  println "Can you count to 10?" ;
  while i <= 10 do
    println i ;
    i = i + 1
  done
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
whileCount.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 20
5	.L.str0:
6		.asciz "Can you count to 10?"
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
27		jmp .L0
28	.L1:
29		# Stack pointer unchanged, no stack allocated arguments
30		mov rax, r12
31		mov rdi, rax
32		# statement primitives do not return results (but will clobber r0/rax)
33		call _printi
34		call _println
35		mov eax, r12d
36		add eax, 1
37		jo _errOverflow
38		movsx rax, eax
39		push rax
40		pop rax
41		mov rax, rax
42		mov r12, rax
43	.L0:
44		cmp r12, 10
45		jle .L1
46		# Stack pointer unchanged, no stack allocated variables
47		mov rax, 0
48		# pop {rbx, r12}
49		mov rbx, qword ptr [rsp]
50		mov r12, qword ptr [rsp + 8]
51		add rsp, 16
52		pop rbp
53		ret
54	
55	.section .rodata
56	# length of .L._prints_str0
57		.int 4
58	.L._prints_str0:
59		.asciz "%.*s"
60	.text
61	_prints:
62		push rbp
63		mov rbp, rsp
64		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
65		and rsp, -16
66		mov rdx, rdi
67		mov esi, dword ptr [rdi - 4]
68		lea rdi, [rip + .L._prints_str0]
69		# on x86, al represents the number of SIMD registers used as variadic arguments
70		mov al, 0
71		call printf@plt
72		mov rdi, 0
73		call fflush@plt
74		mov rsp, rbp
75		pop rbp
76		ret
77	
78	.section .rodata
79	# length of .L._printi_str0
80		.int 2
81	.L._printi_str0:
82		.asciz "%d"
83	.text
84	_printi:
85		push rbp
86		mov rbp, rsp
87		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
88		and rsp, -16
89		mov esi, edi
90		lea rdi, [rip + .L._printi_str0]
91		# on x86, al represents the number of SIMD registers used as variadic arguments
92		mov al, 0
93		call printf@plt
94		mov rdi, 0
95		call fflush@plt
96		mov rsp, rbp
97		pop rbp
98		ret
99	
100	.section .rodata
101	# length of .L._println_str0
102		.int 0
103	.L._println_str0:
104		.asciz ""
105	.text
106	_println:
107		push rbp
108		mov rbp, rsp
109		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
110		and rsp, -16
111		lea rdi, [rip + .L._println_str0]
112		call puts@plt
113		mov rdi, 0
114		call fflush@plt
115		mov rsp, rbp
116		pop rbp
117		ret
118	
119	_exit:
120		push rbp
121		mov rbp, rsp
122		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
123		and rsp, -16
124		call exit@plt
125		mov rsp, rbp
126		pop rbp
127		ret
128	
129	.section .rodata
130	# length of .L._errOverflow_str0
131		.int 52
132	.L._errOverflow_str0:
133		.asciz "fatal error: integer overflow or underflow occurred\n"
134	.text
135	_errOverflow:
136		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
137		and rsp, -16
138		lea rdi, [rip + .L._errOverflow_str0]
139		call _prints
140		mov dil, -1
141		call exit@plt
===========================================================
-- Finished

