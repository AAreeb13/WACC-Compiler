./valid/pairs/writeSnd.wacc
calling the reference compiler on ./valid/pairs/writeSnd.wacc
-- Test: writeSnd.wacc

-- Uploaded file: 
---------------------------------------------------------------
# create a pair and write to its second element

# Output:
# a
# Z
#

# Program:

begin
  pair(int, char) p = newpair(10, 'a') ;
  char s = snd p ;
  println s ;
  snd p = 'Z' ;
  s = snd p ;
  println s
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
writeSnd.s contents are:
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
13		mov edi, 16
14		call _malloc
15		mov r11, rax
16		mov rax, 10
17		mov qword ptr [r11], rax
18		mov rax, 97
19		mov qword ptr [r11 + 8], rax
20		mov rax, r11
21		mov r12, rax
22		cmp r12, 0
23		je _errNull
24		mov rax, qword ptr [r12 + 8]
25		mov rax, rax
26		mov r13, rax
27		# Stack pointer unchanged, no stack allocated arguments
28		mov rax, r13
29		mov rdi, rax
30		# statement primitives do not return results (but will clobber r0/rax)
31		call _printc
32		call _println
33		cmp r12, 0
34		je _errNull
35		mov rax, 90
36		mov qword ptr [r12 + 8], rax
37		cmp r12, 0
38		je _errNull
39		mov rax, qword ptr [r12 + 8]
40		mov rax, rax
41		push rax
42		pop rax
43		mov rax, rax
44		mov r13, rax
45		# Stack pointer unchanged, no stack allocated arguments
46		mov rax, r13
47		mov rdi, rax
48		# statement primitives do not return results (but will clobber r0/rax)
49		call _printc
50		call _println
51		# Stack pointer unchanged, no stack allocated variables
52		mov rax, 0
53		# pop {rbx, r12, r13}
54		mov rbx, qword ptr [rsp]
55		mov r12, qword ptr [rsp + 8]
56		mov r13, qword ptr [rsp + 16]
57		add rsp, 24
58		pop rbp
59		ret
60	
61	.section .rodata
62	# length of .L._prints_str0
63		.int 4
64	.L._prints_str0:
65		.asciz "%.*s"
66	.text
67	_prints:
68		push rbp
69		mov rbp, rsp
70		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
71		and rsp, -16
72		mov rdx, rdi
73		mov esi, dword ptr [rdi - 4]
74		lea rdi, [rip + .L._prints_str0]
75		# on x86, al represents the number of SIMD registers used as variadic arguments
76		mov al, 0
77		call printf@plt
78		mov rdi, 0
79		call fflush@plt
80		mov rsp, rbp
81		pop rbp
82		ret
83	
84	_malloc:
85		push rbp
86		mov rbp, rsp
87		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
88		and rsp, -16
89		call malloc@plt
90		cmp rax, 0
91		je _errOutOfMemory
92		mov rsp, rbp
93		pop rbp
94		ret
95	
96	.section .rodata
97	# length of .L._printc_str0
98		.int 2
99	.L._printc_str0:
100		.asciz "%c"
101	.text
102	_printc:
103		push rbp
104		mov rbp, rsp
105		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
106		and rsp, -16
107		mov sil, dil
108		lea rdi, [rip + .L._printc_str0]
109		# on x86, al represents the number of SIMD registers used as variadic arguments
110		mov al, 0
111		call printf@plt
112		mov rdi, 0
113		call fflush@plt
114		mov rsp, rbp
115		pop rbp
116		ret
117	
118	.section .rodata
119	# length of .L._println_str0
120		.int 0
121	.L._println_str0:
122		.asciz ""
123	.text
124	_println:
125		push rbp
126		mov rbp, rsp
127		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
128		and rsp, -16
129		lea rdi, [rip + .L._println_str0]
130		call puts@plt
131		mov rdi, 0
132		call fflush@plt
133		mov rsp, rbp
134		pop rbp
135		ret
136	
137	_exit:
138		push rbp
139		mov rbp, rsp
140		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
141		and rsp, -16
142		call exit@plt
143		mov rsp, rbp
144		pop rbp
145		ret
146	
147	.section .rodata
148	# length of .L._errOutOfMemory_str0
149		.int 27
150	.L._errOutOfMemory_str0:
151		.asciz "fatal error: out of memory\n"
152	.text
153	_errOutOfMemory:
154		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
155		and rsp, -16
156		lea rdi, [rip + .L._errOutOfMemory_str0]
157		call _prints
158		mov dil, -1
159		call exit@plt
160	
161	.section .rodata
162	# length of .L._errNull_str0
163		.int 45
164	.L._errNull_str0:
165		.asciz "fatal error: null pair dereferenced or freed\n"
166	.text
167	_errNull:
168		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
169		and rsp, -16
170		lea rdi, [rip + .L._errNull_str0]
171		call _prints
172		mov dil, -1
173		call exit@plt
===========================================================
-- Finished

