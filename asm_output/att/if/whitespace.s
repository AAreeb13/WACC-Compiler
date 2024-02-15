./valid/if/whitespace.wacc
calling the reference compiler on ./valid/if/whitespace.wacc
-- Test: whitespace.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Whitespace only important between keyword and variable tokens

# Output:
# 1
#

# Program:

begin
	int a=13;
  if a==13then a=1else a=0fi;
  println a
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
whitespace.s contents are:
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
11		movq $13, %rax
12		movq %rax, %r12
13		cmpq $13, %r12
14		je .L0
15		movq $0, %rax
16		movq %rax, %r12
17		jmp .L1
18	.L0:
19		movq $1, %rax
20		movq %rax, %r12
21	.L1:
22		# Stack pointer unchanged, no stack allocated arguments
23		movq %r12, %rax
24		movq %rax, %rdi
25		# statement primitives do not return results (but will clobber r0/rax)
26		call _printi
27		call _println
28		# Stack pointer unchanged, no stack allocated variables
29		movq $0, %rax
30		# popq {%rbx, %r12}
31		movq (%rsp), %rbx
32		movq 8(%rsp), %r12
33		addq $16, %rsp
34		popq %rbp
35		ret
36	
37	.section .rodata
38	# length of .L._printi_str0
39		.int 2
40	.L._printi_str0:
41		.asciz "%d"
42	.text
43	_printi:
44		pushq %rbp
45		movq %rsp, %rbp
46		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
47		andq $-16, %rsp
48		movl %edi, %esi
49		leaq .L._printi_str0(%rip), %rdi
50		# on x86, al represents the number of SIMD registers used as variadic arguments
51		movb $0, %al
52		call printf@plt
53		movq $0, %rdi
54		call fflush@plt
55		movq %rbp, %rsp
56		popq %rbp
57		ret
58	
59	.section .rodata
60	# length of .L._println_str0
61		.int 0
62	.L._println_str0:
63		.asciz ""
64	.text
65	_println:
66		pushq %rbp
67		movq %rsp, %rbp
68		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
69		andq $-16, %rsp
70		leaq .L._println_str0(%rip), %rdi
71		call puts@plt
72		movq $0, %rdi
73		call fflush@plt
74		movq %rbp, %rsp
75		popq %rbp
76		ret
===========================================================
-- Finished

