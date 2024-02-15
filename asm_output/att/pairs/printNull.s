./valid/pairs/printNull.wacc
calling the reference compiler on ./valid/pairs/printNull.wacc
-- Test: printNull.wacc

-- Uploaded file: 
---------------------------------------------------------------
# print the null reference

# Output:
# (nil)
#

# Program:

begin
  println null
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
printNull.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	.text
3	main:
4		pushq %rbp
5		pushq %rbx
6		movq %rsp, %rbp
7		# Stack pointer unchanged, no stack allocated arguments
8		movq $0, %rax
9		movq %rax, %rdi
10		# statement primitives do not return results (but will clobber r0/rax)
11		call _printp
12		call _println
13		movq $0, %rax
14		popq %rbx
15		popq %rbp
16		ret
17	
18	.section .rodata
19	# length of .L._printp_str0
20		.int 2
21	.L._printp_str0:
22		.asciz "%p"
23	.text
24	_printp:
25		pushq %rbp
26		movq %rsp, %rbp
27		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
28		andq $-16, %rsp
29		movq %rdi, %rsi
30		leaq .L._printp_str0(%rip), %rdi
31		# on x86, al represents the number of SIMD registers used as variadic arguments
32		movb $0, %al
33		call printf@plt
34		movq $0, %rdi
35		call fflush@plt
36		movq %rbp, %rsp
37		popq %rbp
38		ret
39	
40	.section .rodata
41	# length of .L._println_str0
42		.int 0
43	.L._println_str0:
44		.asciz ""
45	.text
46	_println:
47		pushq %rbp
48		movq %rsp, %rbp
49		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
50		andq $-16, %rsp
51		leaq .L._println_str0(%rip), %rdi
52		call puts@plt
53		movq $0, %rdi
54		call fflush@plt
55		movq %rbp, %rsp
56		popq %rbp
57		ret
===========================================================
-- Finished

