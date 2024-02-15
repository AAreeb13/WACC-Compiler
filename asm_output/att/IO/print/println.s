./valid/IO/print/println.wacc
calling the reference compiler on ./valid/IO/print/println.wacc
-- Test: println.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple println statement

# Output:
# Hello World!
#

# Program:

begin
  println "Hello World!"
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
println.s contents are:
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
19		call _println
20		movq $0, %rax
21		popq %rbx
22		popq %rbp
23		ret
24	
25	.section .rodata
26	# length of .L._prints_str0
27		.int 4
28	.L._prints_str0:
29		.asciz "%.*s"
30	.text
31	_prints:
32		pushq %rbp
33		movq %rsp, %rbp
34		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
35		andq $-16, %rsp
36		movq %rdi, %rdx
37		movl -4(%rdi), %esi
38		leaq .L._prints_str0(%rip), %rdi
39		# on x86, al represents the number of SIMD registers used as variadic arguments
40		movb $0, %al
41		call printf@plt
42		movq $0, %rdi
43		call fflush@plt
44		movq %rbp, %rsp
45		popq %rbp
46		ret
47	
48	.section .rodata
49	# length of .L._println_str0
50		.int 0
51	.L._println_str0:
52		.asciz ""
53	.text
54	_println:
55		pushq %rbp
56		movq %rsp, %rbp
57		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
58		andq $-16, %rsp
59		leaq .L._println_str0(%rip), %rdi
60		call puts@plt
61		movq $0, %rdi
62		call fflush@plt
63		movq %rbp, %rsp
64		popq %rbp
65		ret
===========================================================
-- Finished

