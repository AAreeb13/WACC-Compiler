./valid/scope/scopeWhileRedefine.wacc
calling the reference compiler on ./valid/scope/scopeWhileRedefine.wacc
-- Test: scopeWhileRedefine.wacc

-- Uploaded file: 
---------------------------------------------------------------
# variable scoping test that redefines a variable within a while-loop

# Output:
# counting... 5
# counting... 4
# counting... 3
# counting... 2
# counting... 1
# 0 Boom!
#

# Program:

begin
  int x = 5 ;
  string y = " Boom!" ;
  while x > 0 do
    string y = "counting... " ;
    print y;
    println x;
    x = x - 1
  done ;
  print x;
  println y
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
scopeWhileRedefine.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 6
5	.L.str0:
6		.asciz " Boom!"
7	# length of .L.str1
8		.int 12
9	.L.str1:
10		.asciz "counting... "
11	.text
12	main:
13		push rbp
14		# push {rbx, r12, r13, r14}
15		sub rsp, 32
16		mov qword ptr [rsp], rbx
17		mov qword ptr [rsp + 8], r12
18		mov qword ptr [rsp + 16], r13
19		mov qword ptr [rsp + 24], r14
20		mov rbp, rsp
21		# Stack pointer unchanged, no stack allocated variables
22		mov rax, 5
23		mov r12, rax
24		lea rax, [rip + .L.str0]
25		push rax
26		pop rax
27		mov rax, rax
28		mov r13, rax
29		jmp .L0
30	.L1:
31		# Stack pointer unchanged, no stack allocated variables
32		lea rax, [rip + .L.str1]
33		push rax
34		pop rax
35		mov rax, rax
36		mov r14, rax
37		# Stack pointer unchanged, no stack allocated arguments
38		mov rax, r14
39		mov rdi, rax
40		# statement primitives do not return results (but will clobber r0/rax)
41		call _prints
42		# Stack pointer unchanged, no stack allocated arguments
43		mov rax, r12
44		mov rdi, rax
45		# statement primitives do not return results (but will clobber r0/rax)
46		call _printi
47		call _println
48		mov eax, r12d
49		sub eax, 1
50		jo _errOverflow
51		movsx rax, eax
52		push rax
53		pop rax
54		mov rax, rax
55		mov r12, rax
56		# Stack pointer unchanged, no stack allocated variables
57	.L0:
58		cmp r12, 0
59		jg .L1
60		# Stack pointer unchanged, no stack allocated arguments
61		mov rax, r12
62		mov rdi, rax
63		# statement primitives do not return results (but will clobber r0/rax)
64		call _printi
65		# Stack pointer unchanged, no stack allocated arguments
66		mov rax, r13
67		mov rdi, rax
68		# statement primitives do not return results (but will clobber r0/rax)
69		call _prints
70		call _println
71		# Stack pointer unchanged, no stack allocated variables
72		mov rax, 0
73		# pop {rbx, r12, r13, r14}
74		mov rbx, qword ptr [rsp]
75		mov r12, qword ptr [rsp + 8]
76		mov r13, qword ptr [rsp + 16]
77		mov r14, qword ptr [rsp + 24]
78		add rsp, 32
79		pop rbp
80		ret
81	
82	.section .rodata
83	# length of .L._prints_str0
84		.int 4
85	.L._prints_str0:
86		.asciz "%.*s"
87	.text
88	_prints:
89		push rbp
90		mov rbp, rsp
91		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
92		and rsp, -16
93		mov rdx, rdi
94		mov esi, dword ptr [rdi - 4]
95		lea rdi, [rip + .L._prints_str0]
96		# on x86, al represents the number of SIMD registers used as variadic arguments
97		mov al, 0
98		call printf@plt
99		mov rdi, 0
100		call fflush@plt
101		mov rsp, rbp
102		pop rbp
103		ret
104	
105	.section .rodata
106	# length of .L._printi_str0
107		.int 2
108	.L._printi_str0:
109		.asciz "%d"
110	.text
111	_printi:
112		push rbp
113		mov rbp, rsp
114		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
115		and rsp, -16
116		mov esi, edi
117		lea rdi, [rip + .L._printi_str0]
118		# on x86, al represents the number of SIMD registers used as variadic arguments
119		mov al, 0
120		call printf@plt
121		mov rdi, 0
122		call fflush@plt
123		mov rsp, rbp
124		pop rbp
125		ret
126	
127	.section .rodata
128	# length of .L._println_str0
129		.int 0
130	.L._println_str0:
131		.asciz ""
132	.text
133	_println:
134		push rbp
135		mov rbp, rsp
136		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
137		and rsp, -16
138		lea rdi, [rip + .L._println_str0]
139		call puts@plt
140		mov rdi, 0
141		call fflush@plt
142		mov rsp, rbp
143		pop rbp
144		ret
145	
146	_exit:
147		push rbp
148		mov rbp, rsp
149		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
150		and rsp, -16
151		call exit@plt
152		mov rsp, rbp
153		pop rbp
154		ret
155	
156	.section .rodata
157	# length of .L._errOverflow_str0
158		.int 52
159	.L._errOverflow_str0:
160		.asciz "fatal error: integer overflow or underflow occurred\n"
161	.text
162	_errOverflow:
163		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
164		and rsp, -16
165		lea rdi, [rip + .L._errOverflow_str0]
166		call _prints
167		mov dil, -1
168		call exit@plt
===========================================================
-- Finished

