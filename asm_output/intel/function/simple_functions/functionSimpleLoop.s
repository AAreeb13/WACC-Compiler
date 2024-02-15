./valid/function/simple_functions/functionSimpleLoop.wacc
calling the reference compiler on ./valid/function/simple_functions/functionSimpleLoop.wacc
-- Test: functionSimpleLoop.wacc

-- Uploaded file: 
---------------------------------------------------------------
# define and call a function with a simple loop

# Output:
# 10
#

# Program:

begin
  int f(int n) is
    int i = 0;
    while i < n do
      i = i + 1
    done;
    return i
  end
  int x = call f(10) ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
functionSimpleLoop.s contents are:
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
12		# Stack pointer unchanged, no stack allocated arguments
13		mov rax, 10
14		mov rdi, rax
15		call wacc_f
16		mov r11, rax
17		# Stack pointer unchanged, no stack allocated arguments
18		mov rax, r11
19		mov r12, rax
20		# Stack pointer unchanged, no stack allocated arguments
21		mov rax, r12
22		mov rdi, rax
23		# statement primitives do not return results (but will clobber r0/rax)
24		call _printi
25		call _println
26		# Stack pointer unchanged, no stack allocated variables
27		mov rax, 0
28		# pop {rbx, r12}
29		mov rbx, qword ptr [rsp]
30		mov r12, qword ptr [rsp + 8]
31		add rsp, 16
32		pop rbp
33		ret
34	
35	wacc_f:
36		push rbp
37		push r12
38		mov rbp, rsp
39		# Stack pointer unchanged, no stack allocated variables
40		mov rax, 0
41		mov r12, rax
42		jmp .L0
43	.L1:
44		mov eax, r12d
45		add eax, 1
46		jo _errOverflow
47		movsx rax, eax
48		push rax
49		pop rax
50		mov rax, rax
51		mov r12, rax
52	.L0:
53		cmp r12, rdi
54		jl .L1
55		mov rax, r12
56		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
57		mov rsp, rbp
58		pop r12
59		pop rbp
60		ret
61		# Stack pointer unchanged, no stack allocated variables
62		# 'ere be dragons: this is 100% dead code, functions always end in returns!
63	
64	.section .rodata
65	# length of .L._prints_str0
66		.int 4
67	.L._prints_str0:
68		.asciz "%.*s"
69	.text
70	_prints:
71		push rbp
72		mov rbp, rsp
73		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
74		and rsp, -16
75		mov rdx, rdi
76		mov esi, dword ptr [rdi - 4]
77		lea rdi, [rip + .L._prints_str0]
78		# on x86, al represents the number of SIMD registers used as variadic arguments
79		mov al, 0
80		call printf@plt
81		mov rdi, 0
82		call fflush@plt
83		mov rsp, rbp
84		pop rbp
85		ret
86	
87	.section .rodata
88	# length of .L._printi_str0
89		.int 2
90	.L._printi_str0:
91		.asciz "%d"
92	.text
93	_printi:
94		push rbp
95		mov rbp, rsp
96		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
97		and rsp, -16
98		mov esi, edi
99		lea rdi, [rip + .L._printi_str0]
100		# on x86, al represents the number of SIMD registers used as variadic arguments
101		mov al, 0
102		call printf@plt
103		mov rdi, 0
104		call fflush@plt
105		mov rsp, rbp
106		pop rbp
107		ret
108	
109	.section .rodata
110	# length of .L._println_str0
111		.int 0
112	.L._println_str0:
113		.asciz ""
114	.text
115	_println:
116		push rbp
117		mov rbp, rsp
118		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
119		and rsp, -16
120		lea rdi, [rip + .L._println_str0]
121		call puts@plt
122		mov rdi, 0
123		call fflush@plt
124		mov rsp, rbp
125		pop rbp
126		ret
127	
128	_exit:
129		push rbp
130		mov rbp, rsp
131		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
132		and rsp, -16
133		call exit@plt
134		mov rsp, rbp
135		pop rbp
136		ret
137	
138	.section .rodata
139	# length of .L._errOverflow_str0
140		.int 52
141	.L._errOverflow_str0:
142		.asciz "fatal error: integer overflow or underflow occurred\n"
143	.text
144	_errOverflow:
145		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
146		and rsp, -16
147		lea rdi, [rip + .L._errOverflow_str0]
148		call _prints
149		mov dil, -1
150		call exit@plt
===========================================================
-- Finished

