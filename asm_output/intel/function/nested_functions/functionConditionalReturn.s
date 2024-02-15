./valid/function/nested_functions/functionConditionalReturn.wacc
calling the reference compiler on ./valid/function/nested_functions/functionConditionalReturn.wacc
-- Test: functionConditionalReturn.wacc

-- Uploaded file: 
---------------------------------------------------------------
# program has function which only contains an if statement and a return in each branch

# Output:
# true
#

# Program:

begin
  bool f() is
    if true then
      return true
    else
      return false
    fi
  end
  bool x = call f();
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
functionConditionalReturn.s contents are:
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
13		call wacc_f
14		mov r11, rax
15		# Stack pointer unchanged, no stack allocated arguments
16		mov rax, r11
17		mov r12, rax
18		# Stack pointer unchanged, no stack allocated arguments
19		mov rax, r12
20		mov rdi, rax
21		# statement primitives do not return results (but will clobber r0/rax)
22		call _printb
23		call _println
24		# Stack pointer unchanged, no stack allocated variables
25		mov rax, 0
26		# pop {rbx, r12}
27		mov rbx, qword ptr [rsp]
28		mov r12, qword ptr [rsp + 8]
29		add rsp, 16
30		pop rbp
31		ret
32	
33	wacc_f:
34		push rbp
35		mov rbp, rsp
36		mov rax, 1
37		cmp rax, 1
38		je .L0
39		mov rax, 0
40		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
41		mov rsp, rbp
42		pop rbp
43		ret
44		jmp .L1
45	.L0:
46		mov rax, 1
47		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
48		mov rsp, rbp
49		pop rbp
50		ret
51	.L1:
52		# 'ere be dragons: this is 100% dead code, functions always end in returns!
53	
54	.section .rodata
55	# length of .L._printb_str0
56		.int 5
57	.L._printb_str0:
58		.asciz "false"
59	# length of .L._printb_str1
60		.int 4
61	.L._printb_str1:
62		.asciz "true"
63	# length of .L._printb_str2
64		.int 4
65	.L._printb_str2:
66		.asciz "%.*s"
67	.text
68	_printb:
69		push rbp
70		mov rbp, rsp
71		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
72		and rsp, -16
73		cmp dil, 0
74		jne .L_printb0
75		lea rdx, [rip + .L._printb_str0]
76		jmp .L_printb1
77	.L_printb0:
78		lea rdx, [rip + .L._printb_str1]
79	.L_printb1:
80		mov esi, dword ptr [rdx - 4]
81		lea rdi, [rip + .L._printb_str2]
82		# on x86, al represents the number of SIMD registers used as variadic arguments
83		mov al, 0
84		call printf@plt
85		mov rdi, 0
86		call fflush@plt
87		mov rsp, rbp
88		pop rbp
89		ret
90	
91	.section .rodata
92	# length of .L._println_str0
93		.int 0
94	.L._println_str0:
95		.asciz ""
96	.text
97	_println:
98		push rbp
99		mov rbp, rsp
100		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
101		and rsp, -16
102		lea rdi, [rip + .L._println_str0]
103		call puts@plt
104		mov rdi, 0
105		call fflush@plt
106		mov rsp, rbp
107		pop rbp
108		ret
===========================================================
-- Finished

