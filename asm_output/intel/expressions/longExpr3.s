./valid/expressions/longExpr3.wacc
calling the reference compiler on ./valid/expressions/longExpr3.wacc
-- Test: longExpr3.wacc

-- Uploaded file: 
---------------------------------------------------------------
# tests whether the compiler can handle long expressions

# Output:

# Exit:
# 9

begin

  int x = ((((((((((((((((1 - 2) + 3) - 4) + 5) - 6) + 7) - 8) + 9) - 10) + 11) - 12) + 13) - 14) + 15) - 16) + 17);
  exit x

end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
longExpr3.s contents are:
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
12		mov eax, 1
13		sub eax, 2
14		jo _errOverflow
15		movsx rax, eax
16		push rax
17		pop rax
18		mov eax, eax
19		add eax, 3
20		jo _errOverflow
21		movsx rax, eax
22		push rax
23		pop rax
24		mov eax, eax
25		sub eax, 4
26		jo _errOverflow
27		movsx rax, eax
28		push rax
29		pop rax
30		mov eax, eax
31		add eax, 5
32		jo _errOverflow
33		movsx rax, eax
34		push rax
35		pop rax
36		mov eax, eax
37		sub eax, 6
38		jo _errOverflow
39		movsx rax, eax
40		push rax
41		pop rax
42		mov eax, eax
43		add eax, 7
44		jo _errOverflow
45		movsx rax, eax
46		push rax
47		pop rax
48		mov eax, eax
49		sub eax, 8
50		jo _errOverflow
51		movsx rax, eax
52		push rax
53		pop rax
54		mov eax, eax
55		add eax, 9
56		jo _errOverflow
57		movsx rax, eax
58		push rax
59		pop rax
60		mov eax, eax
61		sub eax, 10
62		jo _errOverflow
63		movsx rax, eax
64		push rax
65		pop rax
66		mov eax, eax
67		add eax, 11
68		jo _errOverflow
69		movsx rax, eax
70		push rax
71		pop rax
72		mov eax, eax
73		sub eax, 12
74		jo _errOverflow
75		movsx rax, eax
76		push rax
77		pop rax
78		mov eax, eax
79		add eax, 13
80		jo _errOverflow
81		movsx rax, eax
82		push rax
83		pop rax
84		mov eax, eax
85		sub eax, 14
86		jo _errOverflow
87		movsx rax, eax
88		push rax
89		pop rax
90		mov eax, eax
91		add eax, 15
92		jo _errOverflow
93		movsx rax, eax
94		push rax
95		pop rax
96		mov eax, eax
97		sub eax, 16
98		jo _errOverflow
99		movsx rax, eax
100		push rax
101		pop rax
102		mov eax, eax
103		add eax, 17
104		jo _errOverflow
105		movsx rax, eax
106		push rax
107		pop rax
108		mov rax, rax
109		mov r12, rax
110		# Stack pointer unchanged, no stack allocated arguments
111		mov rax, r12
112		mov rdi, rax
113		# statement primitives do not return results (but will clobber r0/rax)
114		call _exit
115		# Stack pointer unchanged, no stack allocated variables
116		mov rax, 0
117		# pop {rbx, r12}
118		mov rbx, qword ptr [rsp]
119		mov r12, qword ptr [rsp + 8]
120		add rsp, 16
121		pop rbp
122		ret
123	
124	.section .rodata
125	# length of .L._prints_str0
126		.int 4
127	.L._prints_str0:
128		.asciz "%.*s"
129	.text
130	_prints:
131		push rbp
132		mov rbp, rsp
133		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
134		and rsp, -16
135		mov rdx, rdi
136		mov esi, dword ptr [rdi - 4]
137		lea rdi, [rip + .L._prints_str0]
138		# on x86, al represents the number of SIMD registers used as variadic arguments
139		mov al, 0
140		call printf@plt
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

