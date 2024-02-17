./valid/function/simple_functions/functionDoubleReturn.wacc
calling the reference compiler on ./valid/function/simple_functions/functionDoubleReturn.wacc
-- Test: functionDoubleReturn.wacc

-- Uploaded file: 
---------------------------------------------------------------
# a simple function with two back-to-back returns at the end.

# Output:
# 3
#

# Program:

begin
    int f() is
        return 3;
        return 5
    end
    int ret = call f();
    println ret
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
functionDoubleReturn.s contents are:
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
22		call _printi
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
36		mov rax, 3
37		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
38		mov rsp, rbp
39		pop rbp
40		ret
41		mov rax, 5
42		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
43		mov rsp, rbp
44		pop rbp
45		ret
46		# 'ere be dragons: this is 100% dead code, functions always end in returns!
47	
48	.section .rodata
49	# length of .L._printi_str0
50		.int 2
51	.L._printi_str0:
52		.asciz "%d"
53	.text
54	_printi:
55		push rbp
56		mov rbp, rsp
57		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
58		and rsp, -16
59		mov esi, edi
60		lea rdi, [rip + .L._printi_str0]
61		# on x86, al represents the number of SIMD registers used as variadic arguments
62		mov al, 0
63		call printf@plt
64		mov rdi, 0
65		call fflush@plt
66		mov rsp, rbp
67		pop rbp
68		ret
69	
70	.section .rodata
71	# length of .L._println_str0
72		.int 0
73	.L._println_str0:
74		.asciz ""
75	.text
76	_println:
77		push rbp
78		mov rbp, rsp
79		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
80		and rsp, -16
81		lea rdi, [rip + .L._println_str0]
82		call puts@plt
83		mov rdi, 0
84		call fflush@plt
85		mov rsp, rbp
86		pop rbp
87		ret
===========================================================
-- Finished

