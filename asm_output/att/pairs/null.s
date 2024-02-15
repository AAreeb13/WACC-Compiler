./valid/pairs/null.wacc
calling the reference compiler on ./valid/pairs/null.wacc
-- Test: null.wacc

-- Uploaded file: 
---------------------------------------------------------------
# null pair assignment

# Output:
# (nil)
# (nil)
#

# Program:

begin
  pair(pair, pair) p = null ;
  println p ;
  p = null ;
  println p
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
null.s contents are:
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
19		movq $0, %rax
20		movq %rax, %r12
21		# Stack pointer unchanged, no stack allocated arguments
22		movq %r12, %rax
23		movq %rax, %rdi
24		# statement primitives do not return results (but will clobber r0/rax)
25		call _printp
26		call _println
27		# Stack pointer unchanged, no stack allocated variables
28		movq $0, %rax
29		# popq {%rbx, %r12}
30		movq (%rsp), %rbx
31		movq 8(%rsp), %r12
32		addq $16, %rsp
33		popq %rbp
34		ret
35	
36	.section .rodata
37	# length of .L._printp_str0
38		.int 2
39	.L._printp_str0:
40		.asciz "%p"
41	.text
42	_printp:
43		pushq %rbp
44		movq %rsp, %rbp
45		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
46		andq $-16, %rsp
47		movq %rdi, %rsi
48		leaq .L._printp_str0(%rip), %rdi
49		# on x86, al represents the number of SIMD registers used as variadic arguments
50		movb $0, %al
51		call printf@plt
52		movq $0, %rdi
53		call fflush@plt
54		movq %rbp, %rsp
55		popq %rbp
56		ret
57	
58	.section .rodata
59	# length of .L._println_str0
60		.int 0
61	.L._println_str0:
62		.asciz ""
63	.text
64	_println:
65		pushq %rbp
66		movq %rsp, %rbp
67		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
68		andq $-16, %rsp
69		leaq .L._println_str0(%rip), %rdi
70		call puts@plt
71		movq $0, %rdi
72		call fflush@plt
73		movq %rbp, %rsp
74		popq %rbp
75		ret
===========================================================
-- Finished

