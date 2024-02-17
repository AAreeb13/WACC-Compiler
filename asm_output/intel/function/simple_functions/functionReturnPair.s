./valid/function/simple_functions/functionReturnPair.wacc
calling the reference compiler on ./valid/function/simple_functions/functionReturnPair.wacc
-- Test: functionReturnPair.wacc

-- Uploaded file: 
---------------------------------------------------------------
# creates a pair which is returned from a function

# Output:
# 10
#

# Program:

begin

  pair(int, int) getPair() is
    pair(int, int) p = newpair(10,15);
    return p
  end

  pair(int, int) p = call getPair();
  int x = fst p;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
functionReturnPair.s contents are:
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
13		# Stack pointer unchanged, no stack allocated arguments
14		call wacc_getPair
15		mov r11, rax
16		# Stack pointer unchanged, no stack allocated arguments
17		mov rax, r11
18		mov r12, rax
19		cmp r12, 0
20		je _errNull
21		mov rax, qword ptr [r12]
22		mov rax, rax
23		mov r13, rax
24		# Stack pointer unchanged, no stack allocated arguments
25		mov rax, r13
26		mov rdi, rax
27		# statement primitives do not return results (but will clobber r0/rax)
28		call _printi
29		call _println
30		# Stack pointer unchanged, no stack allocated variables
31		mov rax, 0
32		# pop {rbx, r12, r13}
33		mov rbx, qword ptr [rsp]
34		mov r12, qword ptr [rsp + 8]
35		mov r13, qword ptr [rsp + 16]
36		add rsp, 24
37		pop rbp
38		ret
39	
40	wacc_getPair:
41		push rbp
42		push r12
43		mov rbp, rsp
44		# Stack pointer unchanged, no stack allocated variables
45		mov edi, 16
46		call _malloc
47		mov r11, rax
48		mov rax, 10
49		mov qword ptr [r11], rax
50		mov rax, 15
51		mov qword ptr [r11 + 8], rax
52		mov rax, r11
53		mov r12, rax
54		mov rax, r12
55		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
56		mov rsp, rbp
57		pop r12
58		pop rbp
59		ret
60		# Stack pointer unchanged, no stack allocated variables
61		# 'ere be dragons: this is 100% dead code, functions always end in returns!
62	
63	.section .rodata
64	# length of .L._prints_str0
65		.int 4
66	.L._prints_str0:
67		.asciz "%.*s"
68	.text
69	_prints:
70		push rbp
71		mov rbp, rsp
72		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
73		and rsp, -16
74		mov rdx, rdi
75		mov esi, dword ptr [rdi - 4]
76		lea rdi, [rip + .L._prints_str0]
77		# on x86, al represents the number of SIMD registers used as variadic arguments
78		mov al, 0
79		call printf@plt
80		mov rdi, 0
81		call fflush@plt
82		mov rsp, rbp
83		pop rbp
84		ret
85	
86	_malloc:
87		push rbp
88		mov rbp, rsp
89		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
90		and rsp, -16
91		call malloc@plt
92		cmp rax, 0
93		je _errOutOfMemory
94		mov rsp, rbp
95		pop rbp
96		ret
97	
98	.section .rodata
99	# length of .L._printi_str0
100		.int 2
101	.L._printi_str0:
102		.asciz "%d"
103	.text
104	_printi:
105		push rbp
106		mov rbp, rsp
107		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
108		and rsp, -16
109		mov esi, edi
110		lea rdi, [rip + .L._printi_str0]
111		# on x86, al represents the number of SIMD registers used as variadic arguments
112		mov al, 0
113		call printf@plt
114		mov rdi, 0
115		call fflush@plt
116		mov rsp, rbp
117		pop rbp
118		ret
119	
120	.section .rodata
121	# length of .L._println_str0
122		.int 0
123	.L._println_str0:
124		.asciz ""
125	.text
126	_println:
127		push rbp
128		mov rbp, rsp
129		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
130		and rsp, -16
131		lea rdi, [rip + .L._println_str0]
132		call puts@plt
133		mov rdi, 0
134		call fflush@plt
135		mov rsp, rbp
136		pop rbp
137		ret
138	
139	_exit:
140		push rbp
141		mov rbp, rsp
142		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
143		and rsp, -16
144		call exit@plt
145		mov rsp, rbp
146		pop rbp
147		ret
148	
149	.section .rodata
150	# length of .L._errOutOfMemory_str0
151		.int 27
152	.L._errOutOfMemory_str0:
153		.asciz "fatal error: out of memory\n"
154	.text
155	_errOutOfMemory:
156		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
157		and rsp, -16
158		lea rdi, [rip + .L._errOutOfMemory_str0]
159		call _prints
160		mov dil, -1
161		call exit@plt
162	
163	.section .rodata
164	# length of .L._errNull_str0
165		.int 45
166	.L._errNull_str0:
167		.asciz "fatal error: null pair dereferenced or freed\n"
168	.text
169	_errNull:
170		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
171		and rsp, -16
172		lea rdi, [rip + .L._errNull_str0]
173		call _prints
174		mov dil, -1
175		call exit@plt
===========================================================
-- Finished

