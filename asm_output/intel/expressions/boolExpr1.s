./valid/expressions/boolExpr1.wacc
calling the reference compiler on ./valid/expressions/boolExpr1.wacc
-- Test: boolExpr1.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating a moderately complex boolean expression

# Output:
# Correct
#

# Program:

begin
  bool b = ! ( ( true && false) || (true && false) );
  if b == true then
    println "Correct"
  else
    println "Wrong"
  fi
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
boolExpr1.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 5
5	.L.str0:
6		.asciz "Wrong"
7	# length of .L.str1
8		.int 7
9	.L.str1:
10		.asciz "Correct"
11	.text
12	main:
13		push rbp
14		# push {rbx, r12}
15		sub rsp, 16
16		mov qword ptr [rsp], rbx
17		mov qword ptr [rsp + 8], r12
18		mov rbp, rsp
19		# Stack pointer unchanged, no stack allocated variables
20		mov rax, 1
21		cmp rax, 1
22		jne .L0
23		mov rax, 0
24		cmp rax, 1
25	.L0:
26		sete al
27		movsx rax, al
28		push rax
29		pop rax
30		cmp rax, 1
31		je .L1
32		mov rax, 1
33		cmp rax, 1
34		jne .L2
35		mov rax, 0
36		cmp rax, 1
37	.L2:
38		sete al
39		movsx rax, al
40		push rax
41		pop rax
42		cmp rax, 1
43	.L1:
44		setne al
45		movsx rax, al
46		push rax
47		pop rax
48		mov rax, rax
49		mov r12, rax
50		cmp r12, 1
51		je .L3
52		# Stack pointer unchanged, no stack allocated arguments
53		lea rax, [rip + .L.str0]
54		push rax
55		pop rax
56		mov rax, rax
57		mov rdi, rax
58		# statement primitives do not return results (but will clobber r0/rax)
59		call _prints
60		call _println
61		jmp .L4
62	.L3:
63		# Stack pointer unchanged, no stack allocated arguments
64		lea rax, [rip + .L.str1]
65		push rax
66		pop rax
67		mov rax, rax
68		mov rdi, rax
69		# statement primitives do not return results (but will clobber r0/rax)
70		call _prints
71		call _println
72	.L4:
73		# Stack pointer unchanged, no stack allocated variables
74		mov rax, 0
75		# pop {rbx, r12}
76		mov rbx, qword ptr [rsp]
77		mov r12, qword ptr [rsp + 8]
78		add rsp, 16
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
106	# length of .L._println_str0
107		.int 0
108	.L._println_str0:
109		.asciz ""
110	.text
111	_println:
112		push rbp
113		mov rbp, rsp
114		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
115		and rsp, -16
116		lea rdi, [rip + .L._println_str0]
117		call puts@plt
118		mov rdi, 0
119		call fflush@plt
120		mov rsp, rbp
121		pop rbp
122		ret
===========================================================
-- Finished

