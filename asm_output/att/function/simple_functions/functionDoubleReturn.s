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
0	.globl main
1	.section .rodata
2	.text
3	main:
4		pushq %rbp
5		# pushq {%rbx, %r12}
6		subq $16, %rsp
7		movq %rbx, (%rsp)
8		movq %r12, 8(%rsp)
9		movq %rsp, %rbp
10		# Stack pointer unchanged, no stack allocated variables
11		# Stack pointer unchanged, no stack allocated arguments
12		call wacc_f
13		movq %rax, %r11
14		# Stack pointer unchanged, no stack allocated arguments
15		movq %r11, %rax
16		movq %rax, %r12
17		# Stack pointer unchanged, no stack allocated arguments
18		movq %r12, %rax
19		movq %rax, %rdi
20		# statement primitives do not return results (but will clobber r0/rax)
21		call _printi
22		call _println
23		# Stack pointer unchanged, no stack allocated variables
24		movq $0, %rax
25		# popq {%rbx, %r12}
26		movq (%rsp), %rbx
27		movq 8(%rsp), %r12
28		addq $16, %rsp
29		popq %rbp
30		ret
31	
32	wacc_f:
33		pushq %rbp
34		movq %rsp, %rbp
35		movq $3, %rax
36		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
37		movq %rbp, %rsp
38		popq %rbp
39		ret
40		movq $5, %rax
41		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
42		movq %rbp, %rsp
43		popq %rbp
44		ret
45		# 'ere be dragons: this is 100% dead code, functions always end in returns!
46	
47	.section .rodata
48	# length of .L._printi_str0
49		.int 2
50	.L._printi_str0:
51		.asciz "%d"
52	.text
53	_printi:
54		pushq %rbp
55		movq %rsp, %rbp
56		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
57		andq $-16, %rsp
58		movl %edi, %esi
59		leaq .L._printi_str0(%rip), %rdi
60		# on x86, al represents the number of SIMD registers used as variadic arguments
61		movb $0, %al
62		call printf@plt
63		movq $0, %rdi
64		call fflush@plt
65		movq %rbp, %rsp
66		popq %rbp
67		ret
68	
69	.section .rodata
70	# length of .L._println_str0
71		.int 0
72	.L._println_str0:
73		.asciz ""
74	.text
75	_println:
76		pushq %rbp
77		movq %rsp, %rbp
78		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
79		andq $-16, %rsp
80		leaq .L._println_str0(%rip), %rdi
81		call puts@plt
82		movq $0, %rdi
83		call fflush@plt
84		movq %rbp, %rsp
85		popq %rbp
86		ret
===========================================================
-- Finished

