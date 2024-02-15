./valid/scope/splitScope.wacc
calling the reference compiler on ./valid/scope/splitScope.wacc
-- Test: splitScope.wacc

-- Uploaded file: 
---------------------------------------------------------------
# splits the first appearances of variables by a new scope to ensure proper grouping

# Output:
# 3
# 2
#

# Program:

begin
  int x = 1 ;
  begin
    x = 3 ;
    bool z = true
  end ;
  int y = 2 ;
  println x ;
  println y
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
splitScope.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	.text
3	main:
4		pushq %rbp
5		# pushq {%rbx, %r12, %r13, %r14}
6		subq $32, %rsp
7		movq %rbx, (%rsp)
8		movq %r12, 8(%rsp)
9		movq %r13, 16(%rsp)
10		movq %r14, 24(%rsp)
11		movq %rsp, %rbp
12		# Stack pointer unchanged, no stack allocated variables
13		movq $1, %rax
14		movq %rax, %r12
15		# Stack pointer unchanged, no stack allocated variables
16		movq $3, %rax
17		movq %rax, %r12
18		movq $1, %rax
19		movq %rax, %r14
20		# Stack pointer unchanged, no stack allocated variables
21		movq $2, %rax
22		movq %rax, %r13
23		# Stack pointer unchanged, no stack allocated arguments
24		movq %r12, %rax
25		movq %rax, %rdi
26		# statement primitives do not return results (but will clobber r0/rax)
27		call _printi
28		call _println
29		# Stack pointer unchanged, no stack allocated arguments
30		movq %r13, %rax
31		movq %rax, %rdi
32		# statement primitives do not return results (but will clobber r0/rax)
33		call _printi
34		call _println
35		# Stack pointer unchanged, no stack allocated variables
36		movq $0, %rax
37		# popq {%rbx, %r12, %r13, %r14}
38		movq (%rsp), %rbx
39		movq 8(%rsp), %r12
40		movq 16(%rsp), %r13
41		movq 24(%rsp), %r14
42		addq $32, %rsp
43		popq %rbp
44		ret
45	
46	.section .rodata
47	# length of .L._printi_str0
48		.int 2
49	.L._printi_str0:
50		.asciz "%d"
51	.text
52	_printi:
53		pushq %rbp
54		movq %rsp, %rbp
55		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
56		andq $-16, %rsp
57		movl %edi, %esi
58		leaq .L._printi_str0(%rip), %rdi
59		# on x86, al represents the number of SIMD registers used as variadic arguments
60		movb $0, %al
61		call printf@plt
62		movq $0, %rdi
63		call fflush@plt
64		movq %rbp, %rsp
65		popq %rbp
66		ret
67	
68	.section .rodata
69	# length of .L._println_str0
70		.int 0
71	.L._println_str0:
72		.asciz ""
73	.text
74	_println:
75		pushq %rbp
76		movq %rsp, %rbp
77		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
78		andq $-16, %rsp
79		leaq .L._println_str0(%rip), %rdi
80		call puts@plt
81		movq $0, %rdi
82		call fflush@plt
83		movq %rbp, %rsp
84		popq %rbp
85		ret
===========================================================
-- Finished

