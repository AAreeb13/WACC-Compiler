./valid/IO/IOSequence.wacc
calling the reference compiler on ./valid/IO/IOSequence.wacc
-- Test: IOSequence.wacc

-- Uploaded file: 
---------------------------------------------------------------
# basic input/output sequence

# Input: 37

# Output:
# Please input an integer: You input: 37
#

# Program:

begin
  int x = 0 ;
  print "Please input an integer: " ;
  read x ;
  print "You input: " ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
IOSequence.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 25
5	.L.str0:
6		.asciz "Please input an integer: "
7	# length of .L.str1
8		.int 11
9	.L.str1:
10		.asciz "You input: "
11	.text
12	main:
13		push rbp
14		# push {rbx, r12}
15		sub rsp, 16
16		mov qword ptr [rsp], rbx
17		mov qword ptr [rsp + 8], r12
18		mov rbp, rsp
19		# Stack pointer unchanged, no stack allocated variables
20		mov rax, 0
21		mov r12, rax
22		# Stack pointer unchanged, no stack allocated arguments
23		lea rax, [rip + .L.str0]
24		push rax
25		pop rax
26		mov rax, rax
27		mov rdi, rax
28		# statement primitives do not return results (but will clobber r0/rax)
29		call _prints
30		# Stack pointer unchanged, no stack allocated arguments
31		# load the current value in the destination of the read so it supports defaults
32		mov rax, r12
33		mov rdi, rax
34		call _readi
35		mov r11, rax
36		mov rax, r11
37		mov r12, rax
38		# Stack pointer unchanged, no stack allocated arguments
39		lea rax, [rip + .L.str1]
40		push rax
41		pop rax
42		mov rax, rax
43		mov rdi, rax
44		# statement primitives do not return results (but will clobber r0/rax)
45		call _prints
46		# Stack pointer unchanged, no stack allocated arguments
47		mov rax, r12
48		mov rdi, rax
49		# statement primitives do not return results (but will clobber r0/rax)
50		call _printi
51		call _println
52		# Stack pointer unchanged, no stack allocated variables
53		mov rax, 0
54		# pop {rbx, r12}
55		mov rbx, qword ptr [rsp]
56		mov r12, qword ptr [rsp + 8]
57		add rsp, 16
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
84	.section .rodata
85	# length of .L._readi_str0
86		.int 2
87	.L._readi_str0:
88		.asciz "%d"
89	.text
90	_readi:
91		push rbp
92		mov rbp, rsp
93		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
94		and rsp, -16
95		# RDI contains the "original" value of the destination of the read
96		# allocate space on the stack to store the read: preserve alignment!
97		# the passed default argument should be stored in case of EOF
98		sub rsp, 16
99		mov dword ptr [rsp], edi
100		lea rsi, qword ptr [rsp]
101		lea rdi, [rip + .L._readi_str0]
102		# on x86, al represents the number of SIMD registers used as variadic arguments
103		mov al, 0
104		call scanf@plt
105		movsx rax, dword ptr [rsp]
106		add rsp, 16
107		mov rsp, rbp
108		pop rbp
109		ret
110	
111	.section .rodata
112	# length of .L._printi_str0
113		.int 2
114	.L._printi_str0:
115		.asciz "%d"
116	.text
117	_printi:
118		push rbp
119		mov rbp, rsp
120		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
121		and rsp, -16
122		mov esi, edi
123		lea rdi, [rip + .L._printi_str0]
124		# on x86, al represents the number of SIMD registers used as variadic arguments
125		mov al, 0
126		call printf@plt
127		mov rdi, 0
128		call fflush@plt
129		mov rsp, rbp
130		pop rbp
131		ret
132	
133	.section .rodata
134	# length of .L._println_str0
135		.int 0
136	.L._println_str0:
137		.asciz ""
138	.text
139	_println:
140		push rbp
141		mov rbp, rsp
142		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
143		and rsp, -16
144		lea rdi, [rip + .L._println_str0]
145		call puts@plt
146		mov rdi, 0
147		call fflush@plt
148		mov rsp, rbp
149		pop rbp
150		ret
===========================================================
-- Finished

