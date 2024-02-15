./valid/scope/scopeWhileNested.wacc
calling the reference compiler on ./valid/scope/scopeWhileNested.wacc
-- Test: scopeWhileNested.wacc

-- Uploaded file: 
---------------------------------------------------------------
# variable scoping nested within a while-loop

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
  while x > 0 do
    begin
      string x = "counting... " ;
      print x
    end ;
    println x ;
    x = x - 1
  done ;
  print x ;
  println " Boom!"
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
scopeWhileNested.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 12
5	.L.str0:
6		.asciz "counting... "
7	# length of .L.str1
8		.int 6
9	.L.str1:
10		.asciz " Boom!"
11	.text
12	main:
13		push rbp
14		# push {rbx, r12, r13}
15		sub rsp, 24
16		mov qword ptr [rsp], rbx
17		mov qword ptr [rsp + 8], r12
18		mov qword ptr [rsp + 16], r13
19		mov rbp, rsp
20		# Stack pointer unchanged, no stack allocated variables
21		mov rax, 5
22		mov r12, rax
23		jmp .L0
24	.L1:
25		# Stack pointer unchanged, no stack allocated variables
26		lea rax, [rip + .L.str0]
27		push rax
28		pop rax
29		mov rax, rax
30		mov r13, rax
31		# Stack pointer unchanged, no stack allocated arguments
32		mov rax, r13
33		mov rdi, rax
34		# statement primitives do not return results (but will clobber r0/rax)
35		call _prints
36		# Stack pointer unchanged, no stack allocated variables
37		# Stack pointer unchanged, no stack allocated arguments
38		mov rax, r12
39		mov rdi, rax
40		# statement primitives do not return results (but will clobber r0/rax)
41		call _printi
42		call _println
43		mov eax, r12d
44		sub eax, 1
45		jo _errOverflow
46		movsx rax, eax
47		push rax
48		pop rax
49		mov rax, rax
50		mov r12, rax
51	.L0:
52		cmp r12, 0
53		jg .L1
54		# Stack pointer unchanged, no stack allocated arguments
55		mov rax, r12
56		mov rdi, rax
57		# statement primitives do not return results (but will clobber r0/rax)
58		call _printi
59		# Stack pointer unchanged, no stack allocated arguments
60		lea rax, [rip + .L.str1]
61		push rax
62		pop rax
63		mov rax, rax
64		mov rdi, rax
65		# statement primitives do not return results (but will clobber r0/rax)
66		call _prints
67		call _println
68		# Stack pointer unchanged, no stack allocated variables
69		mov rax, 0
70		# pop {rbx, r12, r13}
71		mov rbx, qword ptr [rsp]
72		mov r12, qword ptr [rsp + 8]
73		mov r13, qword ptr [rsp + 16]
74		add rsp, 24
75		pop rbp
76		ret
77	
78	.section .rodata
79	# length of .L._prints_str0
80		.int 4
81	.L._prints_str0:
82		.asciz "%.*s"
83	.text
84	_prints:
85		push rbp
86		mov rbp, rsp
87		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
88		and rsp, -16
89		mov rdx, rdi
90		mov esi, dword ptr [rdi - 4]
91		lea rdi, [rip + .L._prints_str0]
92		# on x86, al represents the number of SIMD registers used as variadic arguments
93		mov al, 0
94		call printf@plt
95		mov rdi, 0
96		call fflush@plt
97		mov rsp, rbp
98		pop rbp
99		ret
100	
101	.section .rodata
102	# length of .L._printi_str0
103		.int 2
104	.L._printi_str0:
105		.asciz "%d"
106	.text
107	_printi:
108		push rbp
109		mov rbp, rsp
110		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
111		and rsp, -16
112		mov esi, edi
113		lea rdi, [rip + .L._printi_str0]
114		# on x86, al represents the number of SIMD registers used as variadic arguments
115		mov al, 0
116		call printf@plt
117		mov rdi, 0
118		call fflush@plt
119		mov rsp, rbp
120		pop rbp
121		ret
122	
123	.section .rodata
124	# length of .L._println_str0
125		.int 0
126	.L._println_str0:
127		.asciz ""
128	.text
129	_println:
130		push rbp
131		mov rbp, rsp
132		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
133		and rsp, -16
134		lea rdi, [rip + .L._println_str0]
135		call puts@plt
136		mov rdi, 0
137		call fflush@plt
138		mov rsp, rbp
139		pop rbp
140		ret
141	
142	_exit:
143		push rbp
144		mov rbp, rsp
145		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
146		and rsp, -16
147		call exit@plt
148		mov rsp, rbp
149		pop rbp
150		ret
151	
152	.section .rodata
153	# length of .L._errOverflow_str0
154		.int 52
155	.L._errOverflow_str0:
156		.asciz "fatal error: integer overflow or underflow occurred\n"
157	.text
158	_errOverflow:
159		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
160		and rsp, -16
161		lea rdi, [rip + .L._errOverflow_str0]
162		call _prints
163		mov dil, -1
164		call exit@plt
===========================================================
-- Finished

