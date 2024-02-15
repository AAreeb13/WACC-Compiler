./valid/scope/scopeSimpleRedefine.wacc
calling the reference compiler on ./valid/scope/scopeSimpleRedefine.wacc
-- Test: scopeSimpleRedefine.wacc

-- Uploaded file: 
---------------------------------------------------------------
# variable scoping test that redefines a variable

# Output:
# true
# 12
#

# Program:

begin
  int x = 12 ;
  begin
    bool x = true ;
    println x
  end ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
scopeSimpleRedefine.s contents are:
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
13		mov rax, 12
14		mov r12, rax
15		# Stack pointer unchanged, no stack allocated variables
16		mov rax, 1
17		mov r13, rax
18		# Stack pointer unchanged, no stack allocated arguments
19		mov rax, r13
20		mov rdi, rax
21		# statement primitives do not return results (but will clobber r0/rax)
22		call _printb
23		call _println
24		# Stack pointer unchanged, no stack allocated variables
25		# Stack pointer unchanged, no stack allocated arguments
26		mov rax, r12
27		mov rdi, rax
28		# statement primitives do not return results (but will clobber r0/rax)
29		call _printi
30		call _println
31		# Stack pointer unchanged, no stack allocated variables
32		mov rax, 0
33		# pop {rbx, r12, r13}
34		mov rbx, qword ptr [rsp]
35		mov r12, qword ptr [rsp + 8]
36		mov r13, qword ptr [rsp + 16]
37		add rsp, 24
38		pop rbp
39		ret
40	
41	.section .rodata
42	# length of .L._printi_str0
43		.int 2
44	.L._printi_str0:
45		.asciz "%d"
46	.text
47	_printi:
48		push rbp
49		mov rbp, rsp
50		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
51		and rsp, -16
52		mov esi, edi
53		lea rdi, [rip + .L._printi_str0]
54		# on x86, al represents the number of SIMD registers used as variadic arguments
55		mov al, 0
56		call printf@plt
57		mov rdi, 0
58		call fflush@plt
59		mov rsp, rbp
60		pop rbp
61		ret
62	
63	.section .rodata
64	# length of .L._printb_str0
65		.int 5
66	.L._printb_str0:
67		.asciz "false"
68	# length of .L._printb_str1
69		.int 4
70	.L._printb_str1:
71		.asciz "true"
72	# length of .L._printb_str2
73		.int 4
74	.L._printb_str2:
75		.asciz "%.*s"
76	.text
77	_printb:
78		push rbp
79		mov rbp, rsp
80		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
81		and rsp, -16
82		cmp dil, 0
83		jne .L_printb0
84		lea rdx, [rip + .L._printb_str0]
85		jmp .L_printb1
86	.L_printb0:
87		lea rdx, [rip + .L._printb_str1]
88	.L_printb1:
89		mov esi, dword ptr [rdx - 4]
90		lea rdi, [rip + .L._printb_str2]
91		# on x86, al represents the number of SIMD registers used as variadic arguments
92		mov al, 0
93		call printf@plt
94		mov rdi, 0
95		call fflush@plt
96		mov rsp, rbp
97		pop rbp
98		ret
99	
100	.section .rodata
101	# length of .L._println_str0
102		.int 0
103	.L._println_str0:
104		.asciz ""
105	.text
106	_println:
107		push rbp
108		mov rbp, rsp
109		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
110		and rsp, -16
111		lea rdi, [rip + .L._println_str0]
112		call puts@plt
113		mov rdi, 0
114		call fflush@plt
115		mov rsp, rbp
116		pop rbp
117		ret
===========================================================
-- Finished

