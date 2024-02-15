./valid/variables/stringCarriageReturn.wacc
calling the reference compiler on ./valid/variables/stringCarriageReturn.wacc
-- Test: stringCarriageReturn.wacc

-- Uploaded file: 
---------------------------------------------------------------
# carriage returns should be parsable, but their behaviour is non-portable, so the IO is not currently tested

# Output:

# Program:

begin
  string s = "Hello \r World!\n"
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
stringCarriageReturn.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 15
4	.L.str0:
5		.asciz "Hello \r World!\n"
6	.text
7	main:
8		pushq %rbp
9		# pushq {%rbx, %r12}
10		subq $16, %rsp
11		movq %rbx, (%rsp)
12		movq %r12, 8(%rsp)
13		movq %rsp, %rbp
14		# Stack pointer unchanged, no stack allocated variables
15		leaq .L.str0(%rip), %rax
16		pushq %rax
17		popq %rax
18		movq %rax, %rax
19		movq %rax, %r12
20		# Stack pointer unchanged, no stack allocated variables
21		movq $0, %rax
22		# popq {%rbx, %r12}
23		movq (%rsp), %rbx
24		movq 8(%rsp), %r12
25		addq $16, %rsp
26		popq %rbp
27		ret
===========================================================
-- Finished

