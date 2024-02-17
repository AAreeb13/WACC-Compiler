./valid/while/rmStyleAdd.wacc
calling the reference compiler on ./valid/while/rmStyleAdd.wacc
-- Test: rmStyleAdd.wacc

-- Uploaded file: 
---------------------------------------------------------------
# register machine style addition

# Output:
# initial value of x: 3
# (+)(+)(+)(+)(+)(+)(+)
# final value of x: 10
#

# Program:

begin
  int x = 3 ;
  int y = 7 ;
  print "initial value of x: " ;
  println x ;
  while y > 0 do
    print "(+)" ;
    x = x + 1 ;
    y = y - 1
  done ;
  println "" ;
  print "final value of x: " ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
rmStyleAdd.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 20
5	.L.str0:
6		.asciz "initial value of x: "
7	# length of .L.str1
8		.int 3
9	.L.str1:
10		.asciz "(+)"
11	# length of .L.str2
12		.int 0
13	.L.str2:
14		.asciz ""
15	# length of .L.str3
16		.int 18
17	.L.str3:
18		.asciz "final value of x: "
19	.text
20	main:
21		push rbp
22		# push {rbx, r12, r13}
23		sub rsp, 24
24		mov qword ptr [rsp], rbx
25		mov qword ptr [rsp + 8], r12
26		mov qword ptr [rsp + 16], r13
27		mov rbp, rsp
28		# Stack pointer unchanged, no stack allocated variables
29		mov rax, 3
30		mov r12, rax
31		mov rax, 7
32		mov r13, rax
33		# Stack pointer unchanged, no stack allocated arguments
34		lea rax, [rip + .L.str0]
35		push rax
36		pop rax
37		mov rax, rax
38		mov rdi, rax
39		# statement primitives do not return results (but will clobber r0/rax)
40		call _prints
41		# Stack pointer unchanged, no stack allocated arguments
42		mov rax, r12
43		mov rdi, rax
44		# statement primitives do not return results (but will clobber r0/rax)
45		call _printi
46		call _println
47		jmp .L0
48	.L1:
49		# Stack pointer unchanged, no stack allocated arguments
50		lea rax, [rip + .L.str1]
51		push rax
52		pop rax
53		mov rax, rax
54		mov rdi, rax
55		# statement primitives do not return results (but will clobber r0/rax)
56		call _prints
57		mov eax, r12d
58		add eax, 1
59		jo _errOverflow
60		movsx rax, eax
61		push rax
62		pop rax
63		mov rax, rax
64		mov r12, rax
65		mov eax, r13d
66		sub eax, 1
67		jo _errOverflow
68		movsx rax, eax
69		push rax
70		pop rax
71		mov rax, rax
72		mov r13, rax
73	.L0:
74		cmp r13, 0
75		jg .L1
76		# Stack pointer unchanged, no stack allocated arguments
77		lea rax, [rip + .L.str2]
78		push rax
79		pop rax
80		mov rax, rax
81		mov rdi, rax
82		# statement primitives do not return results (but will clobber r0/rax)
83		call _prints
84		call _println
85		# Stack pointer unchanged, no stack allocated arguments
86		lea rax, [rip + .L.str3]
87		push rax
88		pop rax
89		mov rax, rax
90		mov rdi, rax
91		# statement primitives do not return results (but will clobber r0/rax)
92		call _prints
93		# Stack pointer unchanged, no stack allocated arguments
94		mov rax, r12
95		mov rdi, rax
96		# statement primitives do not return results (but will clobber r0/rax)
97		call _printi
98		call _println
99		# Stack pointer unchanged, no stack allocated variables
100		mov rax, 0
101		# pop {rbx, r12, r13}
102		mov rbx, qword ptr [rsp]
103		mov r12, qword ptr [rsp + 8]
104		mov r13, qword ptr [rsp + 16]
105		add rsp, 24
106		pop rbp
107		ret
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

