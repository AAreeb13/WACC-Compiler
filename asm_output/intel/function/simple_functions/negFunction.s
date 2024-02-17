./valid/function/simple_functions/negFunction.wacc
calling the reference compiler on ./valid/function/simple_functions/negFunction.wacc
-- Test: negFunction.wacc

-- Uploaded file: 
---------------------------------------------------------------
# a simple negation function definition and usage

# Output:
# true
# false
# true
#

# Program:

begin
  bool neg(bool b) is
    return !b
  end
  bool b = true ;
  println b ;
  b = call neg(b) ;
  println b ;
  b = call neg(b) ;
  b = call neg(b) ;
  b = call neg(b) ;
  println b
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
negFunction.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	.text
4	main:
5		push rbp
6		# push {rbx, r12}
7		sub rsp, 16
8		mov qword ptr [rsp], rbx
9		mov qword ptr [rsp + 8], r12
10		mov rbp, rsp
11		# Stack pointer unchanged, no stack allocated variables
12		mov rax, 1
13		mov r12, rax
14		# Stack pointer unchanged, no stack allocated arguments
15		mov rax, r12
16		mov rdi, rax
17		# statement primitives do not return results (but will clobber r0/rax)
18		call _printb
19		call _println
20		# Stack pointer unchanged, no stack allocated arguments
21		mov rax, r12
22		mov rdi, rax
23		call wacc_neg
24		mov r11, rax
25		# Stack pointer unchanged, no stack allocated arguments
26		mov rax, r11
27		mov r12, rax
28		# Stack pointer unchanged, no stack allocated arguments
29		mov rax, r12
30		mov rdi, rax
31		# statement primitives do not return results (but will clobber r0/rax)
32		call _printb
33		call _println
34		# Stack pointer unchanged, no stack allocated arguments
35		mov rax, r12
36		mov rdi, rax
37		call wacc_neg
38		mov r11, rax
39		# Stack pointer unchanged, no stack allocated arguments
40		mov rax, r11
41		mov r12, rax
42		# Stack pointer unchanged, no stack allocated arguments
43		mov rax, r12
44		mov rdi, rax
45		call wacc_neg
46		mov r11, rax
47		# Stack pointer unchanged, no stack allocated arguments
48		mov rax, r11
49		mov r12, rax
50		# Stack pointer unchanged, no stack allocated arguments
51		mov rax, r12
52		mov rdi, rax
53		call wacc_neg
54		mov r11, rax
55		# Stack pointer unchanged, no stack allocated arguments
56		mov rax, r11
57		mov r12, rax
58		# Stack pointer unchanged, no stack allocated arguments
59		mov rax, r12
60		mov rdi, rax
61		# statement primitives do not return results (but will clobber r0/rax)
62		call _printb
63		call _println
64		# Stack pointer unchanged, no stack allocated variables
65		mov rax, 0
66		# pop {rbx, r12}
67		mov rbx, qword ptr [rsp]
68		mov r12, qword ptr [rsp + 8]
69		add rsp, 16
70		pop rbp
71		ret
72	
73	wacc_neg:
74		push rbp
75		mov rbp, rsp
76		cmp rdi, 1
77		setne al
78		movsx rax, al
79		push rax
80		pop rax
81		mov rax, rax
82		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
83		mov rsp, rbp
84		pop rbp
85		ret
86		# 'ere be dragons: this is 100% dead code, functions always end in returns!
87	
88	.section .rodata
89	# length of .L._printb_str0
90		.int 5
91	.L._printb_str0:
92		.asciz "false"
93	# length of .L._printb_str1
94		.int 4
95	.L._printb_str1:
96		.asciz "true"
97	# length of .L._printb_str2
98		.int 4
99	.L._printb_str2:
100		.asciz "%.*s"
101	.text
102	_printb:
103		push rbp
104		mov rbp, rsp
105		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
106		and rsp, -16
107		cmp dil, 0
108		jne .L_printb0
109		lea rdx, [rip + .L._printb_str0]
110		jmp .L_printb1
111	.L_printb0:
112		lea rdx, [rip + .L._printb_str1]
113	.L_printb1:
114		mov esi, dword ptr [rdx - 4]
115		lea rdi, [rip + .L._printb_str2]
116		# on x86, al represents the number of SIMD registers used as variadic arguments
117		mov al, 0
118		call printf@plt
119		mov rdi, 0
120		call fflush@plt
121		mov rsp, rbp
122		pop rbp
123		ret
124	
125	.section .rodata
126	# length of .L._println_str0
127		.int 0
128	.L._println_str0:
129		.asciz ""
130	.text
131	_println:
132		push rbp
133		mov rbp, rsp
134		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
135		and rsp, -16
136		lea rdi, [rip + .L._println_str0]
137		call puts@plt
138		mov rdi, 0
139		call fflush@plt
140		mov rsp, rbp
141		pop rbp
142		ret
===========================================================
-- Finished

