./valid/scope/scopeRedefine.wacc
calling the reference compiler on ./valid/scope/scopeRedefine.wacc
-- Test: scopeRedefine.wacc

-- Uploaded file: 
---------------------------------------------------------------
# more complex variable scoping test that redefines a variable

# Output:
# true
# 2
#

# Program:

begin
  int x = 1 ;
  begin
    x = 2 ;
    bool x = true ;
    println x
  end ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
scopeRedefine.s contents are:
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
13		mov rax, 1
14		mov r12, rax
15		# Stack pointer unchanged, no stack allocated variables
16		mov rax, 2
17		mov r12, rax
18		mov rax, 1
19		mov r13, rax
20		# Stack pointer unchanged, no stack allocated arguments
21		mov rax, r13
22		mov rdi, rax
23		# statement primitives do not return results (but will clobber r0/rax)
24		call _printb
25		call _println
26		# Stack pointer unchanged, no stack allocated variables
27		# Stack pointer unchanged, no stack allocated arguments
28		mov rax, r12
29		mov rdi, rax
30		# statement primitives do not return results (but will clobber r0/rax)
31		call _printi
32		call _println
33		# Stack pointer unchanged, no stack allocated variables
34		mov rax, 0
35		# pop {rbx, r12, r13}
36		mov rbx, qword ptr [rsp]
37		mov r12, qword ptr [rsp + 8]
38		mov r13, qword ptr [rsp + 16]
39		add rsp, 24
40		pop rbp
41		ret
42	
43	.section .rodata
44	# length of .L._printi_str0
45		.int 2
46	.L._printi_str0:
47		.asciz "%d"
48	.text
49	_printi:
50		push rbp
51		mov rbp, rsp
52		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
53		and rsp, -16
54		mov esi, edi
55		lea rdi, [rip + .L._printi_str0]
56		# on x86, al represents the number of SIMD registers used as variadic arguments
57		mov al, 0
58		call printf@plt
59		mov rdi, 0
60		call fflush@plt
61		mov rsp, rbp
62		pop rbp
63		ret
64	
65	.section .rodata
66	# length of .L._printb_str0
67		.int 5
68	.L._printb_str0:
69		.asciz "false"
70	# length of .L._printb_str1
71		.int 4
72	.L._printb_str1:
73		.asciz "true"
74	# length of .L._printb_str2
75		.int 4
76	.L._printb_str2:
77		.asciz "%.*s"
78	.text
79	_printb:
80		push rbp
81		mov rbp, rsp
82		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
83		and rsp, -16
84		cmp dil, 0
85		jne .L_printb0
86		lea rdx, [rip + .L._printb_str0]
87		jmp .L_printb1
88	.L_printb0:
89		lea rdx, [rip + .L._printb_str1]
90	.L_printb1:
91		mov esi, dword ptr [rdx - 4]
92		lea rdi, [rip + .L._printb_str2]
93		# on x86, al represents the number of SIMD registers used as variadic arguments
94		mov al, 0
95		call printf@plt
96		mov rdi, 0
97		call fflush@plt
98		mov rsp, rbp
99		pop rbp
100		ret
101	
102	.section .rodata
103	# length of .L._println_str0
104		.int 0
105	.L._println_str0:
106		.asciz ""
107	.text
108	_println:
109		push rbp
110		mov rbp, rsp
111		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
112		and rsp, -16
113		lea rdi, [rip + .L._println_str0]
114		call puts@plt
115		mov rdi, 0
116		call fflush@plt
117		mov rsp, rbp
118		pop rbp
119		ret
===========================================================
-- Finished

