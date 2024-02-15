./valid/pairs/printNullPair.wacc
calling the reference compiler on ./valid/pairs/printNullPair.wacc
-- Test: printNullPair.wacc

-- Uploaded file: 
---------------------------------------------------------------
# print pair a null pair

# Output:
# (nil)
#

# Program:

begin
  pair(pair, pair) p = null ;
  println p
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
printNullPair.s contents are:
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
11		movq $0, %rax
12		movq %rax, %r12
13		# Stack pointer unchanged, no stack allocated arguments
14		movq %r12, %rax
15		movq %rax, %rdi
16		# statement primitives do not return results (but will clobber r0/rax)
17		call _printp
18		call _println
19		# Stack pointer unchanged, no stack allocated variables
20		movq $0, %rax
21		# popq {%rbx, %r12}
22		movq (%rsp), %rbx
23		movq 8(%rsp), %r12
24		addq $16, %rsp
25		popq %rbp
26		ret
27	
28	.section .rodata
29	# length of .L._printp_str0
30		.int 2
31	.L._printp_str0:
32		.asciz "%p"
33	.text
34	_printp:
35		pushq %rbp
36		movq %rsp, %rbp
37		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
38		andq $-16, %rsp
39		movq %rdi, %rsi
40		leaq .L._printp_str0(%rip), %rdi
41		# on x86, al represents the number of SIMD registers used as variadic arguments
42		movb $0, %al
43		call printf@plt
44		movq $0, %rdi
45		call fflush@plt
46		movq %rbp, %rsp
47		popq %rbp
48		ret
49	
50	.section .rodata
51	# length of .L._println_str0
52		.int 0
53	.L._println_str0:
54		.asciz ""
55	.text
56	_println:
57		pushq %rbp
58		movq %rsp, %rbp
59		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
60		andq $-16, %rsp
61		leaq .L._println_str0(%rip), %rdi
62		call puts@plt
63		movq $0, %rdi
64		call fflush@plt
65		movq %rbp, %rsp
66		popq %rbp
67		ret
===========================================================
-- Finished

