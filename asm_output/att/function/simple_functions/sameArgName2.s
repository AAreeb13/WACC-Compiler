./valid/function/simple_functions/sameArgName2.wacc
calling the reference compiler on ./valid/function/simple_functions/sameArgName2.wacc
-- Test: sameArgName2.wacc

-- Uploaded file: 
---------------------------------------------------------------
# program with function that has same parameter name as function
# and also returns to same named variable

# Output:
# 99
#

# Program:

begin
  int f(int f) is
    return f
  end

  int f = call f(99);
  println f
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
sameArgName2.s contents are:
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
12		movq $99, %rax
13		movq %rax, %rdi
14		call wacc_f
15		movq %rax, %r11
16		# Stack pointer unchanged, no stack allocated arguments
17		movq %r11, %rax
18		movq %rax, %r12
19		# Stack pointer unchanged, no stack allocated arguments
20		movq %r12, %rax
21		movq %rax, %rdi
22		# statement primitives do not return results (but will clobber r0/rax)
23		call _printi
24		call _println
25		# Stack pointer unchanged, no stack allocated variables
26		movq $0, %rax
27		# popq {%rbx, %r12}
28		movq (%rsp), %rbx
29		movq 8(%rsp), %r12
30		addq $16, %rsp
31		popq %rbp
32		ret
33	
34	wacc_f:
35		pushq %rbp
36		movq %rsp, %rbp
37		movq %rdi, %rax
38		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
39		movq %rbp, %rsp
40		popq %rbp
41		ret
42		# 'ere be dragons: this is 100% dead code, functions always end in returns!
43	
44	.section .rodata
45	# length of .L._printi_str0
46		.int 2
47	.L._printi_str0:
48		.asciz "%d"
49	.text
50	_printi:
51		pushq %rbp
52		movq %rsp, %rbp
53		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
54		andq $-16, %rsp
55		movl %edi, %esi
56		leaq .L._printi_str0(%rip), %rdi
57		# on x86, al represents the number of SIMD registers used as variadic arguments
58		movb $0, %al
59		call printf@plt
60		movq $0, %rdi
61		call fflush@plt
62		movq %rbp, %rsp
63		popq %rbp
64		ret
65	
66	.section .rodata
67	# length of .L._println_str0
68		.int 0
69	.L._println_str0:
70		.asciz ""
71	.text
72	_println:
73		pushq %rbp
74		movq %rsp, %rbp
75		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
76		andq $-16, %rsp
77		leaq .L._println_str0(%rip), %rdi
78		call puts@plt
79		movq $0, %rdi
80		call fflush@plt
81		movq %rbp, %rsp
82		popq %rbp
83		ret
===========================================================
-- Finished

