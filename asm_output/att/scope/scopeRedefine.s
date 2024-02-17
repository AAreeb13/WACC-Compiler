./valid/scope/scopeRedefine.wacc
calling the reference compiler on ./valid/scope/scopeRedefine.wacc
-- Test: scopeRedefine.wacc

-- Uploaded file: 
---------------------------------------------------------------
# more complex variable scoping test that redefines a variable

# Output:
# true
# 2
#

# Program:

begin
  int x = 1 ;
  begin
    x = 2 ;
    bool x = true ;
    println x
  end ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
scopeRedefine.s contents are:
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
12		movq $1, %rax
13		movq %rax, %r12
14		# Stack pointer unchanged, no stack allocated variables
15		movq $2, %rax
16		movq %rax, %r12
17		movq $1, %rax
18		movq %rax, %r13
19		# Stack pointer unchanged, no stack allocated arguments
20		movq %r13, %rax
21		movq %rax, %rdi
22		# statement primitives do not return results (but will clobber r0/rax)
23		call _printb
24		call _println
25		# Stack pointer unchanged, no stack allocated variables
26		# Stack pointer unchanged, no stack allocated arguments
27		movq %r12, %rax
28		movq %rax, %rdi
29		# statement primitives do not return results (but will clobber r0/rax)
30		call _printi
31		call _println
32		# Stack pointer unchanged, no stack allocated variables
33		movq $0, %rax
34		# popq {%rbx, %r12, %r13}
35		movq (%rsp), %rbx
36		movq 8(%rsp), %r12
37		movq 16(%rsp), %r13
38		addq $24, %rsp
39		popq %rbp
40		ret
41	
42	.section .rodata
43	# length of .L._printi_str0
44		.int 2
45	.L._printi_str0:
46		.asciz "%d"
47	.text
48	_printi:
49		pushq %rbp
50		movq %rsp, %rbp
51		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
52		andq $-16, %rsp
53		movl %edi, %esi
54		leaq .L._printi_str0(%rip), %rdi
55		# on x86, al represents the number of SIMD registers used as variadic arguments
56		movb $0, %al
57		call printf@plt
58		movq $0, %rdi
59		call fflush@plt
60		movq %rbp, %rsp
61		popq %rbp
62		ret
63	
64	.section .rodata
65	# length of .L._printb_str0
66		.int 5
67	.L._printb_str0:
68		.asciz "false"
69	# length of .L._printb_str1
70		.int 4
71	.L._printb_str1:
72		.asciz "true"
73	# length of .L._printb_str2
74		.int 4
75	.L._printb_str2:
76		.asciz "%.*s"
77	.text
78	_printb:
79		pushq %rbp
80		movq %rsp, %rbp
81		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
82		andq $-16, %rsp
83		cmpb $0, %dil
84		jne .L_printb0
85		leaq .L._printb_str0(%rip), %rdx
86		jmp .L_printb1
87	.L_printb0:
88		leaq .L._printb_str1(%rip), %rdx
89	.L_printb1:
90		movl -4(%rdx), %esi
91		leaq .L._printb_str2(%rip), %rdi
92		# on x86, al represents the number of SIMD registers used as variadic arguments
93		movb $0, %al
94		call printf@plt
95		movq $0, %rdi
96		call fflush@plt
97		movq %rbp, %rsp
98		popq %rbp
99		ret
100	
101	.section .rodata
102	# length of .L._println_str0
103		.int 0
104	.L._println_str0:
105		.asciz ""
106	.text
107	_println:
108		pushq %rbp
109		movq %rsp, %rbp
110		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
111		andq $-16, %rsp
112		leaq .L._println_str0(%rip), %rdi
113		call puts@plt
114		movq $0, %rdi
115		call fflush@plt
116		movq %rbp, %rsp
117		popq %rbp
118		ret
===========================================================
-- Finished

