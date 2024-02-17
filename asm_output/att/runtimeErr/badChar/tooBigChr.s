./valid/runtimeErr/badChar/tooBigChr.wacc
calling the reference compiler on ./valid/runtimeErr/badChar/tooBigChr.wacc
-- Test: tooBigChr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# try to convert a too large number to character

# Output:
# #runtime_error#

# Exit:
# 255

# Program:

begin
    char c = chr 128
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
tooBigChr.s contents are:
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
11		movq $128, %rax
12		testq $-128, %rax
13		cmovne %rax, %rsi
14		jne _errBadChar
15		pushq %rax
16		popq %rax
17		movq %rax, %rax
18		movq %rax, %r12
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
29	# length of .L._prints_str0
30		.int 4
31	.L._prints_str0:
32		.asciz "%.*s"
33	.text
34	_prints:
35		pushq %rbp
36		movq %rsp, %rbp
37		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
38		andq $-16, %rsp
39		movq %rdi, %rdx
40		movl -4(%rdi), %esi
41		leaq .L._prints_str0(%rip), %rdi
42		# on x86, al represents the number of SIMD registers used as variadic arguments
43		movb $0, %al
44		call printf@plt
45		movq $0, %rdi
46		call fflush@plt
47		movq %rbp, %rsp
48		popq %rbp
49		ret
50	
51	_exit:
52		pushq %rbp
53		movq %rsp, %rbp
54		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
55		andq $-16, %rsp
56		call exit@plt
57		movq %rbp, %rsp
58		popq %rbp
59		ret
60	
61	.section .rodata
62	# length of .L._errBadChar_str0
63		.int 50
64	.L._errBadChar_str0:
65		.asciz "fatal error: int %d is not ascii character 0-127 \n"
66	.text
67	_errBadChar:
68		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
69		andq $-16, %rsp
70		leaq .L._errBadChar_str0(%rip), %rdi
71		# on x86, al represents the number of SIMD registers used as variadic arguments
72		movb $0, %al
73		call printf@plt
74		movq $0, %rdi
75		call fflush@plt
76		movb $-1, %dil
77		call exit@plt
===========================================================
-- Finished

