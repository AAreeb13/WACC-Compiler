./valid/function/simple_functions/functionSimple.wacc
calling the reference compiler on ./valid/function/simple_functions/functionSimple.wacc
-- Test: functionSimple.wacc

-- Uploaded file: 
---------------------------------------------------------------
# a simple function definition and call

# Output:
# 0
#

# Program:

begin
  int f() is
    return 0
  end
  int x = call f() ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
functionSimple.s contents are:
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
36		mov rax, 0
37		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
38		mov rsp, rbp
39		pop rbp
40		ret
41		# 'ere be dragons: this is 100% dead code, functions always end in returns!
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
66	# length of .L._println_str0
67		.int 0
68	.L._println_str0:
69		.asciz ""
70	.text
71	_println:
72		push rbp
73		mov rbp, rsp
74		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
75		and rsp, -16
76		lea rdi, [rip + .L._println_str0]
77		call puts@plt
78		mov rdi, 0
79		call fflush@plt
80		mov rsp, rbp
81		pop rbp
82		ret
===========================================================
-- Finished

