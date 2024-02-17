./valid/function/simple_functions/manyArgumentsInt.wacc
calling the reference compiler on ./valid/function/simple_functions/manyArgumentsInt.wacc
-- Test: manyArgumentsInt.wacc

-- Uploaded file: 
---------------------------------------------------------------
# tests a function with more arguments than fit into registers

# Output:
# 23
#

# Program:

begin
  # w, x, y and z are stack allocated on aarch32
  # y and z are stack allocated on x86-64
  int f(int s, int t, int u, int v, int w, int x, int y, int z) is
    int i = u + v ;
    int j = w * x ;
    int k = y - z ;
    return i + j * k
  end

  int r = call f(0, 0, 1, 4, 2, 3, 7, 4) ;
  println r
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
manyArgumentsInt.s contents are:
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
12		sub rsp, 8
13		mov rax, 0
14		mov rdi, rax
15		mov rax, 0
16		mov rsi, rax
17		mov rax, 1
18		mov rdx, rax
19		mov rax, 4
20		mov rcx, rax
21		mov rax, 2
22		mov r8, rax
23		mov rax, 3
24		mov r9, rax
25		mov rax, 7
26		mov dword ptr [rsp + 4], eax
27		mov rax, 4
28		mov dword ptr [rsp], eax
29		call wacc_f
30		mov r11, rax
31		add rsp, 8
32		mov rax, r11
33		mov r12, rax
34		# Stack pointer unchanged, no stack allocated arguments
35		mov rax, r12
36		mov rdi, rax
37		# statement primitives do not return results (but will clobber r0/rax)
38		call _printi
39		call _println
40		# Stack pointer unchanged, no stack allocated variables
41		mov rax, 0
42		# pop {rbx, r12}
43		mov rbx, qword ptr [rsp]
44		mov r12, qword ptr [rsp + 8]
45		add rsp, 16
46		pop rbp
47		ret
48	
49	wacc_f:
50		push rbp
51		# push {r12, r13, r14}
52		sub rsp, 24
53		mov qword ptr [rsp], r12
54		mov qword ptr [rsp + 8], r13
55		mov qword ptr [rsp + 16], r14
56		mov rbp, rsp
57		# Stack pointer unchanged, no stack allocated variables
58		mov eax, edx
59		add eax, ecx
60		jo _errOverflow
61		movsx rax, eax
62		push rax
63		pop rax
64		mov rax, rax
65		mov r12, rax
66		mov eax, r8d
67		imul eax, r9d
68		jo _errOverflow
69		movsx rax, eax
70		push rax
71		pop rax
72		mov rax, rax
73		mov r13, rax
74		movsx rbx, dword ptr [rbp + 40]
75		movsx rax, dword ptr [rbp + 44]
76		mov eax, eax
77		sub eax, ebx
78		jo _errOverflow
79		movsx rax, eax
80		push rax
81		pop rax
82		mov rax, rax
83		mov r14, rax
84		mov eax, r13d
85		imul eax, r14d
86		jo _errOverflow
87		movsx rax, eax
88		push rax
89		pop rbx
90		mov eax, r12d
91		add eax, ebx
92		jo _errOverflow
93		movsx rax, eax
94		push rax
95		pop rax
96		mov rax, rax
97		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
98		mov rsp, rbp
99		# pop {r12, r13, r14}
100		mov r12, qword ptr [rsp]
101		mov r13, qword ptr [rsp + 8]
102		mov r14, qword ptr [rsp + 16]
103		add rsp, 24
104		pop rbp
105		ret
106		# Stack pointer unchanged, no stack allocated variables
107		# 'ere be dragons: this is 100% dead code, functions always end in returns!
108	
109	.section .rodata
110	# length of .L._prints_str0
111		.int 4
112	.L._prints_str0:
113		.asciz "%.*s"
114	.text
115	_prints:
116		push rbp
117		mov rbp, rsp
118		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
119		and rsp, -16
120		mov rdx, rdi
121		mov esi, dword ptr [rdi - 4]
122		lea rdi, [rip + .L._prints_str0]
123		# on x86, al represents the number of SIMD registers used as variadic arguments
124		mov al, 0
125		call printf@plt
126		mov rdi, 0
127		call fflush@plt
128		mov rsp, rbp
129		pop rbp
130		ret
131	
132	.section .rodata
133	# length of .L._printi_str0
134		.int 2
135	.L._printi_str0:
136		.asciz "%d"
137	.text
138	_printi:
139		push rbp
140		mov rbp, rsp
141		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
142		and rsp, -16
143		mov esi, edi
144		lea rdi, [rip + .L._printi_str0]
145		# on x86, al represents the number of SIMD registers used as variadic arguments
146		mov al, 0
147		call printf@plt
148		mov rdi, 0
149		call fflush@plt
150		mov rsp, rbp
151		pop rbp
152		ret
153	
154	.section .rodata
155	# length of .L._println_str0
156		.int 0
157	.L._println_str0:
158		.asciz ""
159	.text
160	_println:
161		push rbp
162		mov rbp, rsp
163		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
164		and rsp, -16
165		lea rdi, [rip + .L._println_str0]
166		call puts@plt
167		mov rdi, 0
168		call fflush@plt
169		mov rsp, rbp
170		pop rbp
171		ret
172	
173	_exit:
174		push rbp
175		mov rbp, rsp
176		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
177		and rsp, -16
178		call exit@plt
179		mov rsp, rbp
180		pop rbp
181		ret
182	
183	.section .rodata
184	# length of .L._errOverflow_str0
185		.int 52
186	.L._errOverflow_str0:
187		.asciz "fatal error: integer overflow or underflow occurred\n"
188	.text
189	_errOverflow:
190		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
191		and rsp, -16
192		lea rdi, [rip + .L._errOverflow_str0]
193		call _prints
194		mov dil, -1
195		call exit@plt
===========================================================
-- Finished

