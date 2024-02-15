./valid/function/simple_functions/incFunction.wacc
calling the reference compiler on ./valid/function/simple_functions/incFunction.wacc
-- Test: incFunction.wacc

-- Uploaded file: 
---------------------------------------------------------------
# a simple increment function definition and usage

# Output:
# 1
# 4
#

# Program:

begin
  int inc(int x) is
    return x + 1
  end
  int x = 0 ;
  x = call inc(x) ;
  println x ;
  x = call inc(x) ;
  x = call inc(x) ;
  x = call inc(x) ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
incFunction.s contents are:
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
12		mov rax, 0
13		mov r12, rax
14		# Stack pointer unchanged, no stack allocated arguments
15		mov rax, r12
16		mov rdi, rax
17		call wacc_inc
18		mov r11, rax
19		# Stack pointer unchanged, no stack allocated arguments
20		mov rax, r11
21		mov r12, rax
22		# Stack pointer unchanged, no stack allocated arguments
23		mov rax, r12
24		mov rdi, rax
25		# statement primitives do not return results (but will clobber r0/rax)
26		call _printi
27		call _println
28		# Stack pointer unchanged, no stack allocated arguments
29		mov rax, r12
30		mov rdi, rax
31		call wacc_inc
32		mov r11, rax
33		# Stack pointer unchanged, no stack allocated arguments
34		mov rax, r11
35		mov r12, rax
36		# Stack pointer unchanged, no stack allocated arguments
37		mov rax, r12
38		mov rdi, rax
39		call wacc_inc
40		mov r11, rax
41		# Stack pointer unchanged, no stack allocated arguments
42		mov rax, r11
43		mov r12, rax
44		# Stack pointer unchanged, no stack allocated arguments
45		mov rax, r12
46		mov rdi, rax
47		call wacc_inc
48		mov r11, rax
49		# Stack pointer unchanged, no stack allocated arguments
50		mov rax, r11
51		mov r12, rax
52		# Stack pointer unchanged, no stack allocated arguments
53		mov rax, r12
54		mov rdi, rax
55		# statement primitives do not return results (but will clobber r0/rax)
56		call _printi
57		call _println
58		# Stack pointer unchanged, no stack allocated variables
59		mov rax, 0
60		# pop {rbx, r12}
61		mov rbx, qword ptr [rsp]
62		mov r12, qword ptr [rsp + 8]
63		add rsp, 16
64		pop rbp
65		ret
66	
67	wacc_inc:
68		push rbp
69		mov rbp, rsp
70		mov eax, edi
71		add eax, 1
72		jo _errOverflow
73		movsx rax, eax
74		push rax
75		pop rax
76		mov rax, rax
77		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
78		mov rsp, rbp
79		pop rbp
80		ret
81		# 'ere be dragons: this is 100% dead code, functions always end in returns!
82	
83	.section .rodata
84	# length of .L._prints_str0
85		.int 4
86	.L._prints_str0:
87		.asciz "%.*s"
88	.text
89	_prints:
90		push rbp
91		mov rbp, rsp
92		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
93		and rsp, -16
94		mov rdx, rdi
95		mov esi, dword ptr [rdi - 4]
96		lea rdi, [rip + .L._prints_str0]
97		# on x86, al represents the number of SIMD registers used as variadic arguments
98		mov al, 0
99		call printf@plt
100		mov rdi, 0
101		call fflush@plt
102		mov rsp, rbp
103		pop rbp
104		ret
105	
106	.section .rodata
107	# length of .L._printi_str0
108		.int 2
109	.L._printi_str0:
110		.asciz "%d"
111	.text
112	_printi:
113		push rbp
114		mov rbp, rsp
115		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
116		and rsp, -16
117		mov esi, edi
118		lea rdi, [rip + .L._printi_str0]
119		# on x86, al represents the number of SIMD registers used as variadic arguments
120		mov al, 0
121		call printf@plt
122		mov rdi, 0
123		call fflush@plt
124		mov rsp, rbp
125		pop rbp
126		ret
127	
128	.section .rodata
129	# length of .L._println_str0
130		.int 0
131	.L._println_str0:
132		.asciz ""
133	.text
134	_println:
135		push rbp
136		mov rbp, rsp
137		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
138		and rsp, -16
139		lea rdi, [rip + .L._println_str0]
140		call puts@plt
141		mov rdi, 0
142		call fflush@plt
143		mov rsp, rbp
144		pop rbp
145		ret
146	
147	_exit:
148		push rbp
149		mov rbp, rsp
150		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
151		and rsp, -16
152		call exit@plt
153		mov rsp, rbp
154		pop rbp
155		ret
156	
157	.section .rodata
158	# length of .L._errOverflow_str0
159		.int 52
160	.L._errOverflow_str0:
161		.asciz "fatal error: integer overflow or underflow occurred\n"
162	.text
163	_errOverflow:
164		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
165		and rsp, -16
166		lea rdi, [rip + .L._errOverflow_str0]
167		call _prints
168		mov dil, -1
169		call exit@plt
===========================================================
-- Finished

