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
35		movq $0, %rax
36		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
37		movq %rbp, %rsp
38		popq %rbp
39		ret
40		# 'ere be dragons: this is 100% dead code, functions always end in returns!
41	
42	.section .rodata
43	# length of .L._printi_str0
44		.int 2
45	.L._printi_str0:
46		.asciz "%d"
47	.text
48	_printi:
49		pushq %rbp
50		movq %rsp, %rbp
51		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
52		andq $-16, %rsp
53		movl %edi, %esi
54		leaq .L._printi_str0(%rip), %rdi
55		# on x86, al represents the number of SIMD registers used as variadic arguments
56		movb $0, %al
57		call printf@plt
58		movq $0, %rdi
59		call fflush@plt
60		movq %rbp, %rsp
61		popq %rbp
62		ret
63	
64	.section .rodata
65	# length of .L._println_str0
66		.int 0
67	.L._println_str0:
68		.asciz ""
69	.text
70	_println:
71		pushq %rbp
72		movq %rsp, %rbp
73		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
74		andq $-16, %rsp
75		leaq .L._println_str0(%rip), %rdi
76		call puts@plt
77		movq $0, %rdi
78		call fflush@plt
79		movq %rbp, %rsp
80		popq %rbp
81		ret
===========================================================
-- Finished

