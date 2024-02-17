./valid/expressions/ordAndchrExpr.wacc
calling the reference compiler on ./valid/expressions/ordAndchrExpr.wacc
-- Test: ordAndchrExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evalutaing ord and chr

# Output:
# a is 97
# 99 is c
#

# Program:

begin
  char a = 'a' ;
  int i = 99 ;

  print a ;
  print " is " ;
  println ord a ;

  print i ;
  print " is " ;
  println chr i
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
ordAndchrExpr.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 4
5	.L.str0:
6		.asciz " is "
7	.text
8	main:
9		push rbp
10		# push {rbx, r12, r13}
11		sub rsp, 24
12		mov qword ptr [rsp], rbx
13		mov qword ptr [rsp + 8], r12
14		mov qword ptr [rsp + 16], r13
15		mov rbp, rsp
16		# Stack pointer unchanged, no stack allocated variables
17		mov rax, 97
18		mov r12, rax
19		mov rax, 99
20		mov r13, rax
21		# Stack pointer unchanged, no stack allocated arguments
22		mov rax, r12
23		mov rdi, rax
24		# statement primitives do not return results (but will clobber r0/rax)
25		call _printc
26		# Stack pointer unchanged, no stack allocated arguments
27		lea rax, [rip + .L.str0]
28		push rax
29		pop rax
30		mov rax, rax
31		mov rdi, rax
32		# statement primitives do not return results (but will clobber r0/rax)
33		call _prints
34		# Stack pointer unchanged, no stack allocated arguments
35		mov rax, r12
36		mov rdi, rax
37		# statement primitives do not return results (but will clobber r0/rax)
38		call _printi
39		call _println
40		# Stack pointer unchanged, no stack allocated arguments
41		mov rax, r13
42		mov rdi, rax
43		# statement primitives do not return results (but will clobber r0/rax)
44		call _printi
45		# Stack pointer unchanged, no stack allocated arguments
46		lea rax, [rip + .L.str0]
47		push rax
48		pop rax
49		mov rax, rax
50		mov rdi, rax
51		# statement primitives do not return results (but will clobber r0/rax)
52		call _prints
53		# Stack pointer unchanged, no stack allocated arguments
54		mov rax, r13
55		test rax, -128
56		cmovne rsi, rax
57		jne _errBadChar
58		push rax
59		pop rax
60		mov rax, rax
61		mov rdi, rax
62		# statement primitives do not return results (but will clobber r0/rax)
63		call _printc
64		call _println
65		# Stack pointer unchanged, no stack allocated variables
66		mov rax, 0
67		# pop {rbx, r12, r13}
68		mov rbx, qword ptr [rsp]
69		mov r12, qword ptr [rsp + 8]
70		mov r13, qword ptr [rsp + 16]
71		add rsp, 24
72		pop rbp
73		ret
74	
75	.section .rodata
76	# length of .L._prints_str0
77		.int 4
78	.L._prints_str0:
79		.asciz "%.*s"
80	.text
81	_prints:
82		push rbp
83		mov rbp, rsp
84		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
85		and rsp, -16
86		mov rdx, rdi
87		mov esi, dword ptr [rdi - 4]
88		lea rdi, [rip + .L._prints_str0]
89		# on x86, al represents the number of SIMD registers used as variadic arguments
90		mov al, 0
91		call printf@plt
92		mov rdi, 0
93		call fflush@plt
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
121	# length of .L._printc_str0
122		.int 2
123	.L._printc_str0:
124		.asciz "%c"
125	.text
126	_printc:
127		push rbp
128		mov rbp, rsp
129		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
130		and rsp, -16
131		mov sil, dil
132		lea rdi, [rip + .L._printc_str0]
133		# on x86, al represents the number of SIMD registers used as variadic arguments
134		mov al, 0
135		call printf@plt
136		mov rdi, 0
137		call fflush@plt
138		mov rsp, rbp
139		pop rbp
140		ret
141	
142	.section .rodata
143	# length of .L._println_str0
144		.int 0
145	.L._println_str0:
146		.asciz ""
147	.text
148	_println:
149		push rbp
150		mov rbp, rsp
151		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
152		and rsp, -16
153		lea rdi, [rip + .L._println_str0]
154		call puts@plt
155		mov rdi, 0
156		call fflush@plt
157		mov rsp, rbp
158		pop rbp
159		ret
160	
161	_exit:
162		push rbp
163		mov rbp, rsp
164		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
165		and rsp, -16
166		call exit@plt
167		mov rsp, rbp
168		pop rbp
169		ret
170	
171	.section .rodata
172	# length of .L._errBadChar_str0
173		.int 50
174	.L._errBadChar_str0:
175		.asciz "fatal error: int %d is not ascii character 0-127 \n"
176	.text
177	_errBadChar:
178		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
179		and rsp, -16
180		lea rdi, [rip + .L._errBadChar_str0]
181		# on x86, al represents the number of SIMD registers used as variadic arguments
182		mov al, 0
183		call printf@plt
184		mov rdi, 0
185		call fflush@plt
186		mov dil, -1
187		call exit@plt
===========================================================
-- Finished

