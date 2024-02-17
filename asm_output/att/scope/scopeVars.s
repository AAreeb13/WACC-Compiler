./valid/scope/scopeVars.wacc
calling the reference compiler on ./valid/scope/scopeVars.wacc
-- Test: scopeVars.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple variable scoping test

# Output:
# 2
# 4
# 2
#

# Program:

begin
  int x = 2 ;
  println x ;
  begin
    int x = 4 ;
    println x
  end ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
scopeVars.s contents are:
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
12		movq $2, %rax
13		movq %rax, %r12
14		# Stack pointer unchanged, no stack allocated arguments
15		movq %r12, %rax
16		movq %rax, %rdi
17		# statement primitives do not return results (but will clobber r0/rax)
18		call _printi
19		call _println
20		# Stack pointer unchanged, no stack allocated variables
21		movq $4, %rax
22		movq %rax, %r13
23		# Stack pointer unchanged, no stack allocated arguments
24		movq %r13, %rax
25		movq %rax, %rdi
26		# statement primitives do not return results (but will clobber r0/rax)
27		call _printi
28		call _println
29		# Stack pointer unchanged, no stack allocated variables
30		# Stack pointer unchanged, no stack allocated arguments
31		movq %r12, %rax
32		movq %rax, %rdi
33		# statement primitives do not return results (but will clobber r0/rax)
34		call _printi
35		call _println
36		# Stack pointer unchanged, no stack allocated variables
37		movq $0, %rax
38		# popq {%rbx, %r12, %r13}
39		movq (%rsp), %rbx
40		movq 8(%rsp), %r12
41		movq 16(%rsp), %r13
42		addq $24, %rsp
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

