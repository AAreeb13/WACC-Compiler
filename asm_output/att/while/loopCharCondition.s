./valid/while/loopCharCondition.wacc
calling the reference compiler on ./valid/while/loopCharCondition.wacc
-- Test: loopCharCondition.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Use a character as a loop condition. Enter the loop once only, then exit the loop.

# Output:
# Change c
# Should print "Change c" once before.
#

# Program:

begin
	char c = '\0' ;
	while c == '\0' do
		c = 'a' ;
		println "Change c"
	done ;
	println "Should print \"Change c\" once before."
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
loopCharCondition.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 8
4	.L.str0:
5		.asciz "Change c"
6	# length of .L.str1
7		.int 36
8	.L.str1:
9		.asciz "Should print \"Change c\" once before."
10	.text
11	main:
12		pushq %rbp
13		# pushq {%rbx, %r12}
14		subq $16, %rsp
15		movq %rbx, (%rsp)
16		movq %r12, 8(%rsp)
17		movq %rsp, %rbp
18		# Stack pointer unchanged, no stack allocated variables
19		movq $0, %rax
20		movq %rax, %r12
21		jmp .L0
22	.L1:
23		movq $97, %rax
24		movq %rax, %r12
25		# Stack pointer unchanged, no stack allocated arguments
26		leaq .L.str0(%rip), %rax
27		pushq %rax
28		popq %rax
29		movq %rax, %rax
30		movq %rax, %rdi
31		# statement primitives do not return results (but will clobber r0/rax)
32		call _prints
33		call _println
34	.L0:
35		cmpq $0, %r12
36		je .L1
37		# Stack pointer unchanged, no stack allocated arguments
38		leaq .L.str1(%rip), %rax
39		pushq %rax
40		popq %rax
41		movq %rax, %rax
42		movq %rax, %rdi
43		# statement primitives do not return results (but will clobber r0/rax)
44		call _prints
45		call _println
46		# Stack pointer unchanged, no stack allocated variables
47		movq $0, %rax
48		# popq {%rbx, %r12}
49		movq (%rsp), %rbx
50		movq 8(%rsp), %r12
51		addq $16, %rsp
52		popq %rbp
53		ret
54	
55	.section .rodata
56	# length of .L._prints_str0
57		.int 4
58	.L._prints_str0:
59		.asciz "%.*s"
60	.text
61	_prints:
62		pushq %rbp
63		movq %rsp, %rbp
64		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
65		andq $-16, %rsp
66		movq %rdi, %rdx
67		movl -4(%rdi), %esi
68		leaq .L._prints_str0(%rip), %rdi
69		# on x86, al represents the number of SIMD registers used as variadic arguments
70		movb $0, %al
71		call printf@plt
72		movq $0, %rdi
73		call fflush@plt
74		movq %rbp, %rsp
75		popq %rbp
76		ret
77	
78	.section .rodata
79	# length of .L._println_str0
80		.int 0
81	.L._println_str0:
82		.asciz ""
83	.text
84	_println:
85		pushq %rbp
86		movq %rsp, %rbp
87		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
88		andq $-16, %rsp
89		leaq .L._println_str0(%rip), %rdi
90		call puts@plt
91		movq $0, %rdi
92		call fflush@plt
93		movq %rbp, %rsp
94		popq %rbp
95		ret
===========================================================
-- Finished

