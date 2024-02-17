./valid/function/simple_functions/punning.wacc
calling the reference compiler on ./valid/function/simple_functions/punning.wacc
-- Test: punning.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Functions should be able to have the same name as variables
# Thanks to Jordan Hall

# Output:
# 0
#

begin
    int inc(int x) is
        return x + 1
    end
    int inc = 0;
    int tmp = call inc(inc);
    println inc
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
punning.s contents are:
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
13		mov rax, 0
14		mov r12, rax
15		# Stack pointer unchanged, no stack allocated arguments
16		mov rax, r12
17		mov rdi, rax
18		call wacc_inc
19		mov r11, rax
20		# Stack pointer unchanged, no stack allocated arguments
21		mov rax, r11
22		mov r13, rax
23		# Stack pointer unchanged, no stack allocated arguments
24		mov rax, r12
25		mov rdi, rax
26		# statement primitives do not return results (but will clobber r0/rax)
27		call _printi
28		call _println
29		# Stack pointer unchanged, no stack allocated variables
30		mov rax, 0
31		# pop {rbx, r12, r13}
32		mov rbx, qword ptr [rsp]
33		mov r12, qword ptr [rsp + 8]
34		mov r13, qword ptr [rsp + 16]
35		add rsp, 24
36		pop rbp
37		ret
38	
39	wacc_inc:
40		push rbp
41		mov rbp, rsp
42		mov eax, edi
43		add eax, 1
44		jo _errOverflow
45		movsx rax, eax
46		push rax
47		pop rax
48		mov rax, rax
49		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
50		mov rsp, rbp
51		pop rbp
52		ret
53		# 'ere be dragons: this is 100% dead code, functions always end in returns!
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

