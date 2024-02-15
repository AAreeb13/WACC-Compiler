./valid/while/max.wacc
calling the reference compiler on ./valid/while/max.wacc
-- Test: max.wacc

-- Uploaded file: 
---------------------------------------------------------------
# find the max of two numbers

# Output:
# max value = 17
#

# Program:

begin
  int i = 0 ;
  int x = 10 ;
  int y = 17 ;
  while (y > 0 || x > 0) do
    x = x - 1 ;
    y = y - 1 ;
    i = i + 1
  done ;
  print "max value = ";
  println i
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
max.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 12
5	.L.str0:
6		.asciz "max value = "
7	.text
8	main:
9		push rbp
10		# push {rbx, r12, r13, r14}
11		sub rsp, 32
12		mov qword ptr [rsp], rbx
13		mov qword ptr [rsp + 8], r12
14		mov qword ptr [rsp + 16], r13
15		mov qword ptr [rsp + 24], r14
16		mov rbp, rsp
17		# Stack pointer unchanged, no stack allocated variables
18		mov rax, 0
19		mov r12, rax
20		mov rax, 10
21		mov r13, rax
22		mov rax, 17
23		mov r14, rax
24		jmp .L0
25	.L1:
26		mov eax, r13d
27		sub eax, 1
28		jo _errOverflow
29		movsx rax, eax
30		push rax
31		pop rax
32		mov rax, rax
33		mov r13, rax
34		mov eax, r14d
35		sub eax, 1
36		jo _errOverflow
37		movsx rax, eax
38		push rax
39		pop rax
40		mov rax, rax
41		mov r14, rax
42		mov eax, r12d
43		add eax, 1
44		jo _errOverflow
45		movsx rax, eax
46		push rax
47		pop rax
48		mov rax, rax
49		mov r12, rax
50	.L0:
51		cmp r14, 0
52		setg al
53		movsx rax, al
54		push rax
55		pop rax
56		cmp rax, 1
57		je .L2
58		cmp r13, 0
59		setg al
60		movsx rax, al
61		push rax
62		pop rax
63		cmp rax, 1
64	.L2:
65		je .L1
66		# Stack pointer unchanged, no stack allocated arguments
67		lea rax, [rip + .L.str0]
68		push rax
69		pop rax
70		mov rax, rax
71		mov rdi, rax
72		# statement primitives do not return results (but will clobber r0/rax)
73		call _prints
74		# Stack pointer unchanged, no stack allocated arguments
75		mov rax, r12
76		mov rdi, rax
77		# statement primitives do not return results (but will clobber r0/rax)
78		call _printi
79		call _println
80		# Stack pointer unchanged, no stack allocated variables
81		mov rax, 0
82		# pop {rbx, r12, r13, r14}
83		mov rbx, qword ptr [rsp]
84		mov r12, qword ptr [rsp + 8]
85		mov r13, qword ptr [rsp + 16]
86		mov r14, qword ptr [rsp + 24]
87		add rsp, 32
88		pop rbp
89		ret
90	
91	.section .rodata
92	# length of .L._prints_str0
93		.int 4
94	.L._prints_str0:
95		.asciz "%.*s"
96	.text
97	_prints:
98		push rbp
99		mov rbp, rsp
100		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
101		and rsp, -16
102		mov rdx, rdi
103		mov esi, dword ptr [rdi - 4]
104		lea rdi, [rip + .L._prints_str0]
105		# on x86, al represents the number of SIMD registers used as variadic arguments
106		mov al, 0
107		call printf@plt
108		mov rdi, 0
109		call fflush@plt
110		mov rsp, rbp
111		pop rbp
112		ret
113	
114	.section .rodata
115	# length of .L._printi_str0
116		.int 2
117	.L._printi_str0:
118		.asciz "%d"
119	.text
120	_printi:
121		push rbp
122		mov rbp, rsp
123		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
124		and rsp, -16
125		mov esi, edi
126		lea rdi, [rip + .L._printi_str0]
127		# on x86, al represents the number of SIMD registers used as variadic arguments
128		mov al, 0
129		call printf@plt
130		mov rdi, 0
131		call fflush@plt
132		mov rsp, rbp
133		pop rbp
134		ret
135	
136	.section .rodata
137	# length of .L._println_str0
138		.int 0
139	.L._println_str0:
140		.asciz ""
141	.text
142	_println:
143		push rbp
144		mov rbp, rsp
145		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
146		and rsp, -16
147		lea rdi, [rip + .L._println_str0]
148		call puts@plt
149		mov rdi, 0
150		call fflush@plt
151		mov rsp, rbp
152		pop rbp
153		ret
154	
155	_exit:
156		push rbp
157		mov rbp, rsp
158		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
159		and rsp, -16
160		call exit@plt
161		mov rsp, rbp
162		pop rbp
163		ret
164	
165	.section .rodata
166	# length of .L._errOverflow_str0
167		.int 52
168	.L._errOverflow_str0:
169		.asciz "fatal error: integer overflow or underflow occurred\n"
170	.text
171	_errOverflow:
172		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
173		and rsp, -16
174		lea rdi, [rip + .L._errOverflow_str0]
175		call _prints
176		mov dil, -1
177		call exit@plt
===========================================================
-- Finished

