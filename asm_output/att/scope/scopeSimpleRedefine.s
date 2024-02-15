./valid/scope/scopeSimpleRedefine.wacc
calling the reference compiler on ./valid/scope/scopeSimpleRedefine.wacc
-- Test: scopeSimpleRedefine.wacc

-- Uploaded file: 
---------------------------------------------------------------
# variable scoping test that redefines a variable

# Output:
# true
# 12
#

# Program:

begin
  int x = 12 ;
  begin
    bool x = true ;
    println x
  end ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
scopeSimpleRedefine.s contents are:
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
12		movq $12, %rax
13		movq %rax, %r12
14		# Stack pointer unchanged, no stack allocated variables
15		movq $1, %rax
16		movq %rax, %r13
17		# Stack pointer unchanged, no stack allocated arguments
18		movq %r13, %rax
19		movq %rax, %rdi
20		# statement primitives do not return results (but will clobber r0/rax)
21		call _printb
22		call _println
23		# Stack pointer unchanged, no stack allocated variables
24		# Stack pointer unchanged, no stack allocated arguments
25		movq %r12, %rax
26		movq %rax, %rdi
27		# statement primitives do not return results (but will clobber r0/rax)
28		call _printi
29		call _println
30		# Stack pointer unchanged, no stack allocated variables
31		movq $0, %rax
32		# popq {%rbx, %r12, %r13}
33		movq (%rsp), %rbx
34		movq 8(%rsp), %r12
35		movq 16(%rsp), %r13
36		addq $24, %rsp
37		popq %rbp
38		ret
39	
40	.section .rodata
41	# length of .L._printi_str0
42		.int 2
43	.L._printi_str0:
44		.asciz "%d"
45	.text
46	_printi:
47		pushq %rbp
48		movq %rsp, %rbp
49		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
50		andq $-16, %rsp
51		movl %edi, %esi
52		leaq .L._printi_str0(%rip), %rdi
53		# on x86, al represents the number of SIMD registers used as variadic arguments
54		movb $0, %al
55		call printf@plt
56		movq $0, %rdi
57		call fflush@plt
58		movq %rbp, %rsp
59		popq %rbp
60		ret
61	
62	.section .rodata
63	# length of .L._printb_str0
64		.int 5
65	.L._printb_str0:
66		.asciz "false"
67	# length of .L._printb_str1
68		.int 4
69	.L._printb_str1:
70		.asciz "true"
71	# length of .L._printb_str2
72		.int 4
73	.L._printb_str2:
74		.asciz "%.*s"
75	.text
76	_printb:
77		pushq %rbp
78		movq %rsp, %rbp
79		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
80		andq $-16, %rsp
81		cmpb $0, %dil
82		jne .L_printb0
83		leaq .L._printb_str0(%rip), %rdx
84		jmp .L_printb1
85	.L_printb0:
86		leaq .L._printb_str1(%rip), %rdx
87	.L_printb1:
88		movl -4(%rdx), %esi
89		leaq .L._printb_str2(%rip), %rdi
90		# on x86, al represents the number of SIMD registers used as variadic arguments
91		movb $0, %al
92		call printf@plt
93		movq $0, %rdi
94		call fflush@plt
95		movq %rbp, %rsp
96		popq %rbp
97		ret
98	
99	.section .rodata
100	# length of .L._println_str0
101		.int 0
102	.L._println_str0:
103		.asciz ""
104	.text
105	_println:
106		pushq %rbp
107		movq %rsp, %rbp
108		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
109		andq $-16, %rsp
110		leaq .L._println_str0(%rip), %rdi
111		call puts@plt
112		movq $0, %rdi
113		call fflush@plt
114		movq %rbp, %rsp
115		popq %rbp
116		ret
===========================================================
-- Finished

