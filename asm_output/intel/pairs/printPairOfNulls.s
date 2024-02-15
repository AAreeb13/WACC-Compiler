./valid/pairs/printPairOfNulls.wacc
calling the reference compiler on ./valid/pairs/printPairOfNulls.wacc
-- Test: printPairOfNulls.wacc

-- Uploaded file: 
---------------------------------------------------------------
# print a pair of null pairs

# Output:
# #addrs# = ((nil),(nil))
#

# Program:

begin
  pair(pair, pair) p = newpair(null, null) ;
  print p ;
  print " = (" ;
  pair(pair, pair) q = fst p ;
  print q ;
  print "," ;
  pair(int, bool) r = snd p ;
  print r ;
  println ")"
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
printPairOfNulls.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 4
5	.L.str0:
6		.asciz " = ("
7	# length of .L.str1
8		.int 1
9	.L.str1:
10		.asciz ","
11	# length of .L.str2
12		.int 1
13	.L.str2:
14		.asciz ")"
15	.text
16	main:
17		push rbp
18		# push {rbx, r12, r13, r14}
19		sub rsp, 32
20		mov qword ptr [rsp], rbx
21		mov qword ptr [rsp + 8], r12
22		mov qword ptr [rsp + 16], r13
23		mov qword ptr [rsp + 24], r14
24		mov rbp, rsp
25		# Stack pointer unchanged, no stack allocated variables
26		mov edi, 16
27		call _malloc
28		mov r11, rax
29		mov rax, 0
30		mov qword ptr [r11], rax
31		mov rax, 0
32		mov qword ptr [r11 + 8], rax
33		mov rax, r11
34		mov r12, rax
35		# Stack pointer unchanged, no stack allocated arguments
36		mov rax, r12
37		mov rdi, rax
38		# statement primitives do not return results (but will clobber r0/rax)
39		call _printp
40		# Stack pointer unchanged, no stack allocated arguments
41		lea rax, [rip + .L.str0]
42		push rax
43		pop rax
44		mov rax, rax
45		mov rdi, rax
46		# statement primitives do not return results (but will clobber r0/rax)
47		call _prints
48		cmp r12, 0
49		je _errNull
50		mov rax, qword ptr [r12]
51		mov rax, rax
52		mov r13, rax
53		# Stack pointer unchanged, no stack allocated arguments
54		mov rax, r13
55		mov rdi, rax
56		# statement primitives do not return results (but will clobber r0/rax)
57		call _printp
58		# Stack pointer unchanged, no stack allocated arguments
59		lea rax, [rip + .L.str1]
60		push rax
61		pop rax
62		mov rax, rax
63		mov rdi, rax
64		# statement primitives do not return results (but will clobber r0/rax)
65		call _prints
66		cmp r12, 0
67		je _errNull
68		mov rax, qword ptr [r12 + 8]
69		mov rax, rax
70		mov r14, rax
71		# Stack pointer unchanged, no stack allocated arguments
72		mov rax, r14
73		mov rdi, rax
74		# statement primitives do not return results (but will clobber r0/rax)
75		call _printp
76		# Stack pointer unchanged, no stack allocated arguments
77		lea rax, [rip + .L.str2]
78		push rax
79		pop rax
80		mov rax, rax
81		mov rdi, rax
82		# statement primitives do not return results (but will clobber r0/rax)
83		call _prints
84		call _println
85		# Stack pointer unchanged, no stack allocated variables
86		mov rax, 0
87		# pop {rbx, r12, r13, r14}
88		mov rbx, qword ptr [rsp]
89		mov r12, qword ptr [rsp + 8]
90		mov r13, qword ptr [rsp + 16]
91		mov r14, qword ptr [rsp + 24]
92		add rsp, 32
93		pop rbp
94		ret
95	
96	.section .rodata
97	# length of .L._prints_str0
98		.int 4
99	.L._prints_str0:
100		.asciz "%.*s"
101	.text
102	_prints:
103		push rbp
104		mov rbp, rsp
105		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
106		and rsp, -16
107		mov rdx, rdi
108		mov esi, dword ptr [rdi - 4]
109		lea rdi, [rip + .L._prints_str0]
110		# on x86, al represents the number of SIMD registers used as variadic arguments
111		mov al, 0
112		call printf@plt
113		mov rdi, 0
114		call fflush@plt
115		mov rsp, rbp
116		pop rbp
117		ret
118	
119	.section .rodata
120	# length of .L._printp_str0
121		.int 2
122	.L._printp_str0:
123		.asciz "%p"
124	.text
125	_printp:
126		push rbp
127		mov rbp, rsp
128		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
129		and rsp, -16
130		mov rsi, rdi
131		lea rdi, [rip + .L._printp_str0]
132		# on x86, al represents the number of SIMD registers used as variadic arguments
133		mov al, 0
134		call printf@plt
135		mov rdi, 0
136		call fflush@plt
137		mov rsp, rbp
138		pop rbp
139		ret
140	
141	_malloc:
142		push rbp
143		mov rbp, rsp
144		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
145		and rsp, -16
146		call malloc@plt
147		cmp rax, 0
148		je _errOutOfMemory
149		mov rsp, rbp
150		pop rbp
151		ret
152	
153	.section .rodata
154	# length of .L._println_str0
155		.int 0
156	.L._println_str0:
157		.asciz ""
158	.text
159	_println:
160		push rbp
161		mov rbp, rsp
162		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
163		and rsp, -16
164		lea rdi, [rip + .L._println_str0]
165		call puts@plt
166		mov rdi, 0
167		call fflush@plt
168		mov rsp, rbp
169		pop rbp
170		ret
171	
172	_exit:
173		push rbp
174		mov rbp, rsp
175		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
176		and rsp, -16
177		call exit@plt
178		mov rsp, rbp
179		pop rbp
180		ret
181	
182	.section .rodata
183	# length of .L._errOutOfMemory_str0
184		.int 27
185	.L._errOutOfMemory_str0:
186		.asciz "fatal error: out of memory\n"
187	.text
188	_errOutOfMemory:
189		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
190		and rsp, -16
191		lea rdi, [rip + .L._errOutOfMemory_str0]
192		call _prints
193		mov dil, -1
194		call exit@plt
195	
196	.section .rodata
197	# length of .L._errNull_str0
198		.int 45
199	.L._errNull_str0:
200		.asciz "fatal error: null pair dereferenced or freed\n"
201	.text
202	_errNull:
203		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
204		and rsp, -16
205		lea rdi, [rip + .L._errNull_str0]
206		call _prints
207		mov dil, -1
208		call exit@plt
===========================================================
-- Finished

