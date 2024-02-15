./valid/IO/IOLoop.wacc
calling the reference compiler on ./valid/IO/IOLoop.wacc
-- Test: IOLoop.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple input/output loop

# Input: 1 Y 2 Y 3 Y 4 Y 5 Y 142 N

# Output:
# Please input an integer: echo input: 1
# Do you want to continue entering input?
# (enter Y for 'yes' and N for 'no')
# Please input an integer: echo input: 2
# Do you want to continue entering input?
# (enter Y for 'yes' and N for 'no')
# Please input an integer: echo input: 3
# Do you want to continue entering input?
# (enter Y for 'yes' and N for 'no')
# Please input an integer: echo input: 4
# Do you want to continue entering input?
# (enter Y for 'yes' and N for 'no')
# Please input an integer: echo input: 5
# Do you want to continue entering input?
# (enter Y for 'yes' and N for 'no')
# Please input an integer: echo input: 142
# Do you want to continue entering input?
# (enter Y for 'yes' and N for 'no')
#

# Program:

begin
  char continue = 'Y' ;
  int buff = 0 ;
  while continue != 'N' do
    print "Please input an integer: " ;
    read buff ;
    print "echo input: " ;
    println buff ;
    println "Do you want to continue entering input?" ;
    println "(enter Y for \'yes\' and N for \'no\')" ;
    read continue
  done
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
IOLoop.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 25
5	.L.str0:
6		.asciz "Please input an integer: "
7	# length of .L.str1
8		.int 12
9	.L.str1:
10		.asciz "echo input: "
11	# length of .L.str2
12		.int 39
13	.L.str2:
14		.asciz "Do you want to continue entering input?"
15	# length of .L.str3
16		.int 34
17	.L.str3:
18		.asciz "(enter Y for 'yes' and N for 'no')"
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
29		mov rax, 89
30		mov r12, rax
31		mov rax, 0
32		mov r13, rax
33		jmp .L0
34	.L1:
35		# Stack pointer unchanged, no stack allocated arguments
36		lea rax, [rip + .L.str0]
37		push rax
38		pop rax
39		mov rax, rax
40		mov rdi, rax
41		# statement primitives do not return results (but will clobber r0/rax)
42		call _prints
43		# Stack pointer unchanged, no stack allocated arguments
44		# load the current value in the destination of the read so it supports defaults
45		mov rax, r13
46		mov rdi, rax
47		call _readi
48		mov r11, rax
49		mov rax, r11
50		mov r13, rax
51		# Stack pointer unchanged, no stack allocated arguments
52		lea rax, [rip + .L.str1]
53		push rax
54		pop rax
55		mov rax, rax
56		mov rdi, rax
57		# statement primitives do not return results (but will clobber r0/rax)
58		call _prints
59		# Stack pointer unchanged, no stack allocated arguments
60		mov rax, r13
61		mov rdi, rax
62		# statement primitives do not return results (but will clobber r0/rax)
63		call _printi
64		call _println
65		# Stack pointer unchanged, no stack allocated arguments
66		lea rax, [rip + .L.str2]
67		push rax
68		pop rax
69		mov rax, rax
70		mov rdi, rax
71		# statement primitives do not return results (but will clobber r0/rax)
72		call _prints
73		call _println
74		# Stack pointer unchanged, no stack allocated arguments
75		lea rax, [rip + .L.str3]
76		push rax
77		pop rax
78		mov rax, rax
79		mov rdi, rax
80		# statement primitives do not return results (but will clobber r0/rax)
81		call _prints
82		call _println
83		# Stack pointer unchanged, no stack allocated arguments
84		# load the current value in the destination of the read so it supports defaults
85		mov rax, r12
86		mov rdi, rax
87		call _readc
88		mov r11, rax
89		mov rax, r11
90		mov r12, rax
91	.L0:
92		cmp r12, 78
93		jne .L1
94		# Stack pointer unchanged, no stack allocated variables
95		mov rax, 0
96		# pop {rbx, r12, r13}
97		mov rbx, qword ptr [rsp]
98		mov r12, qword ptr [rsp + 8]
99		mov r13, qword ptr [rsp + 16]
100		add rsp, 24
101		pop rbp
102		ret
103	
104	.section .rodata
105	# length of .L._readc_str0
106		.int 3
107	.L._readc_str0:
108		.asciz " %c"
109	.text
110	_readc:
111		push rbp
112		mov rbp, rsp
113		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
114		and rsp, -16
115		# RDI contains the "original" value of the destination of the read
116		# allocate space on the stack to store the read: preserve alignment!
117		# the passed default argument should be stored in case of EOF
118		sub rsp, 16
119		mov byte ptr [rsp], dil
120		lea rsi, qword ptr [rsp]
121		lea rdi, [rip + .L._readc_str0]
122		# on x86, al represents the number of SIMD registers used as variadic arguments
123		mov al, 0
124		call scanf@plt
125		movsx rax, byte ptr [rsp]
126		add rsp, 16
127		mov rsp, rbp
128		pop rbp
129		ret
130	
131	.section .rodata
132	# length of .L._prints_str0
133		.int 4
134	.L._prints_str0:
135		.asciz "%.*s"
136	.text
137	_prints:
138		push rbp
139		mov rbp, rsp
140		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
141		and rsp, -16
142		mov rdx, rdi
143		mov esi, dword ptr [rdi - 4]
144		lea rdi, [rip + .L._prints_str0]
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
155	# length of .L._readi_str0
156		.int 2
157	.L._readi_str0:
158		.asciz "%d"
159	.text
160	_readi:
161		push rbp
162		mov rbp, rsp
163		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
164		and rsp, -16
165		# RDI contains the "original" value of the destination of the read
166		# allocate space on the stack to store the read: preserve alignment!
167		# the passed default argument should be stored in case of EOF
168		sub rsp, 16
169		mov dword ptr [rsp], edi
170		lea rsi, qword ptr [rsp]
171		lea rdi, [rip + .L._readi_str0]
172		# on x86, al represents the number of SIMD registers used as variadic arguments
173		mov al, 0
174		call scanf@plt
175		movsx rax, dword ptr [rsp]
176		add rsp, 16
177		mov rsp, rbp
178		pop rbp
179		ret
180	
181	.section .rodata
182	# length of .L._printi_str0
183		.int 2
184	.L._printi_str0:
185		.asciz "%d"
186	.text
187	_printi:
188		push rbp
189		mov rbp, rsp
190		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
191		and rsp, -16
192		mov esi, edi
193		lea rdi, [rip + .L._printi_str0]
194		# on x86, al represents the number of SIMD registers used as variadic arguments
195		mov al, 0
196		call printf@plt
197		mov rdi, 0
198		call fflush@plt
199		mov rsp, rbp
200		pop rbp
201		ret
202	
203	.section .rodata
204	# length of .L._println_str0
205		.int 0
206	.L._println_str0:
207		.asciz ""
208	.text
209	_println:
210		push rbp
211		mov rbp, rsp
212		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
213		and rsp, -16
214		lea rdi, [rip + .L._println_str0]
215		call puts@plt
216		mov rdi, 0
217		call fflush@plt
218		mov rsp, rbp
219		pop rbp
220		ret
===========================================================
-- Finished

