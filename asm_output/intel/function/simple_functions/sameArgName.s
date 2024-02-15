./valid/function/simple_functions/sameArgName.wacc
calling the reference compiler on ./valid/function/simple_functions/sameArgName.wacc
-- Test: sameArgName.wacc

-- Uploaded file: 
---------------------------------------------------------------
# program with function that has same parameter name as function

# Output:
# 99
#

# Program:

begin
  int f(int f) is
    return f
  end

  int x = call f(99);
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
sameArgName.s contents are:
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
13		mov rax, 99
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
24		call _printi
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
37		mov rbp, rsp
38		mov rax, rdi
39		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
40		mov rsp, rbp
41		pop rbp
42		ret
43		# 'ere be dragons: this is 100% dead code, functions always end in returns!
44	
45	.section .rodata
46	# length of .L._printi_str0
47		.int 2
48	.L._printi_str0:
49		.asciz "%d"
50	.text
51	_printi:
52		push rbp
53		mov rbp, rsp
54		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
55		and rsp, -16
56		mov esi, edi
57		lea rdi, [rip + .L._printi_str0]
58		# on x86, al represents the number of SIMD registers used as variadic arguments
59		mov al, 0
60		call printf@plt
61		mov rdi, 0
62		call fflush@plt
63		mov rsp, rbp
64		pop rbp
65		ret
66	
67	.section .rodata
68	# length of .L._println_str0
69		.int 0
70	.L._println_str0:
71		.asciz ""
72	.text
73	_println:
74		push rbp
75		mov rbp, rsp
76		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
77		and rsp, -16
78		lea rdi, [rip + .L._println_str0]
79		call puts@plt
80		mov rdi, 0
81		call fflush@plt
82		mov rsp, rbp
83		pop rbp
84		ret
===========================================================
-- Finished

