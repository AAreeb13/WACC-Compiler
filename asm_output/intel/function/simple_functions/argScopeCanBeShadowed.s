./valid/function/simple_functions/argScopeCanBeShadowed.wacc
calling the reference compiler on ./valid/function/simple_functions/argScopeCanBeShadowed.wacc
-- Test: argScopeCanBeShadowed.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Arguments can be shadowed by the function body

# Output:
# true
#

#Program:

begin
  bool f(int x) is
    bool x = true ;
    return x
  end

  bool x = call f(5);
  println x

end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
argScopeCanBeShadowed.s contents are:
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
12		# Stack pointer unchanged, no stack allocated arguments
13		mov rax, 5
14		mov rdi, rax
15		call wacc_f
16		mov r11, rax
17		# Stack pointer unchanged, no stack allocated arguments
18		mov rax, r11
19		mov r12, rax
20		# Stack pointer unchanged, no stack allocated arguments
21		mov rax, r12
22		mov rdi, rax
23		# statement primitives do not return results (but will clobber r0/rax)
24		call _printb
25		call _println
26		# Stack pointer unchanged, no stack allocated variables
27		mov rax, 0
28		# pop {rbx, r12}
29		mov rbx, qword ptr [rsp]
30		mov r12, qword ptr [rsp + 8]
31		add rsp, 16
32		pop rbp
33		ret
34	
35	wacc_f:
36		push rbp
37		push r12
38		mov rbp, rsp
39		# Stack pointer unchanged, no stack allocated variables
40		mov rax, 1
41		mov r12, rax
42		mov rax, r12
43		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
44		mov rsp, rbp
45		pop r12
46		pop rbp
47		ret
48		# Stack pointer unchanged, no stack allocated variables
49		# 'ere be dragons: this is 100% dead code, functions always end in returns!
50	
51	.section .rodata
52	# length of .L._printb_str0
53		.int 5
54	.L._printb_str0:
55		.asciz "false"
56	# length of .L._printb_str1
57		.int 4
58	.L._printb_str1:
59		.asciz "true"
60	# length of .L._printb_str2
61		.int 4
62	.L._printb_str2:
63		.asciz "%.*s"
64	.text
65	_printb:
66		push rbp
67		mov rbp, rsp
68		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
69		and rsp, -16
70		cmp dil, 0
71		jne .L_printb0
72		lea rdx, [rip + .L._printb_str0]
73		jmp .L_printb1
74	.L_printb0:
75		lea rdx, [rip + .L._printb_str1]
76	.L_printb1:
77		mov esi, dword ptr [rdx - 4]
78		lea rdi, [rip + .L._printb_str2]
79		# on x86, al represents the number of SIMD registers used as variadic arguments
80		mov al, 0
81		call printf@plt
82		mov rdi, 0
83		call fflush@plt
84		mov rsp, rbp
85		pop rbp
86		ret
87	
88	.section .rodata
89	# length of .L._println_str0
90		.int 0
91	.L._println_str0:
92		.asciz ""
93	.text
94	_println:
95		push rbp
96		mov rbp, rsp
97		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
98		and rsp, -16
99		lea rdi, [rip + .L._println_str0]
100		call puts@plt
101		mov rdi, 0
102		call fflush@plt
103		mov rsp, rbp
104		pop rbp
105		ret
===========================================================
-- Finished

