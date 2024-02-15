./valid/IO/print/print.wacc
calling the reference compiler on ./valid/IO/print/print.wacc
-- Test: print.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple print statement

# Output:
# Hello World!

# Program:

begin
  print "Hello World!"
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
print.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 12
4	.L.str0:
5		.asciz "Hello World!"
6	.text
7	main:
8		pushq %rbp
9		pushq %rbx
10		movq %rsp, %rbp
11		# Stack pointer unchanged, no stack allocated arguments
12		leaq .L.str0(%rip), %rax
13		pushq %rax
14		popq %rax
15		movq %rax, %rax
16		movq %rax, %rdi
17		# statement primitives do not return results (but will clobber r0/rax)
18		call _prints
19		movq $0, %rax
20		popq %rbx
21		popq %rbp
22		ret
23	
24	.section .rodata
25	# length of .L._prints_str0
26		.int 4
27	.L._prints_str0:
28		.asciz "%.*s"
29	.text
30	_prints:
31		pushq %rbp
32		movq %rsp, %rbp
33		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
34		andq $-16, %rsp
35		movq %rdi, %rdx
36		movl -4(%rdi), %esi
37		leaq .L._prints_str0(%rip), %rdi
38		# on x86, al represents the number of SIMD registers used as variadic arguments
39		movb $0, %al
40		call printf@plt
41		movq $0, %rdi
42		call fflush@plt
43		movq %rbp, %rsp
44		popq %rbp
45		ret
===========================================================
-- Finished

