./valid/sequence/intLeadingZeros.wacc
calling the reference compiler on ./valid/sequence/intLeadingZeros.wacc
-- Test: intLeadingZeros.wacc

-- Uploaded file: 
---------------------------------------------------------------
# integer variable declaration with leading zeroes

# Output:
# 42
# 0
#

# Program:

begin
  int x = 0000000000000000000000000000000000000042 ;
  int y = 0000000000000000000000000000000000000000 ;
  println x ;
  println y
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
intLeadingZeros.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	.text
3	main:
4		pushq %rbp
5		# pushq {%rbx, %r12, %r13}
6		subq $24, %rsp
7		movq %rbx, (%rsp)
8		movq %r12, 8(%rsp)
9		movq %r13, 16(%rsp)
10		movq %rsp, %rbp
11		# Stack pointer unchanged, no stack allocated variables
12		movq $42, %rax
13		movq %rax, %r12
14		movq $0, %rax
15		movq %rax, %r13
16		# Stack pointer unchanged, no stack allocated arguments
17		movq %r12, %rax
18		movq %rax, %rdi
19		# statement primitives do not return results (but will clobber r0/rax)
20		call _printi
21		call _println
22		# Stack pointer unchanged, no stack allocated arguments
23		movq %r13, %rax
24		movq %rax, %rdi
25		# statement primitives do not return results (but will clobber r0/rax)
26		call _printi
27		call _println
28		# Stack pointer unchanged, no stack allocated variables
29		movq $0, %rax
30		# popq {%rbx, %r12, %r13}
31		movq (%rsp), %rbx
32		movq 8(%rsp), %r12
33		movq 16(%rsp), %r13
34		addq $24, %rsp
35		popq %rbp
36		ret
37	
38	.section .rodata
39	# length of .L._printi_str0
40		.int 2
41	.L._printi_str0:
42		.asciz "%d"
43	.text
44	_printi:
45		pushq %rbp
46		movq %rsp, %rbp
47		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
48		andq $-16, %rsp
49		movl %edi, %esi
50		leaq .L._printi_str0(%rip), %rdi
51		# on x86, al represents the number of SIMD registers used as variadic arguments
52		movb $0, %al
53		call printf@plt
54		movq $0, %rdi
55		call fflush@plt
56		movq %rbp, %rsp
57		popq %rbp
58		ret
59	
60	.section .rodata
61	# length of .L._println_str0
62		.int 0
63	.L._println_str0:
64		.asciz ""
65	.text
66	_println:
67		pushq %rbp
68		movq %rsp, %rbp
69		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
70		andq $-16, %rsp
71		leaq .L._println_str0(%rip), %rdi
72		call puts@plt
73		movq $0, %rdi
74		call fflush@plt
75		movq %rbp, %rsp
76		popq %rbp
77		ret
===========================================================
-- Finished

