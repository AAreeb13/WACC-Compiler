./valid/function/nested_functions/functionConditionalReturn.wacc
calling the reference compiler on ./valid/function/nested_functions/functionConditionalReturn.wacc
-- Test: functionConditionalReturn.wacc

-- Uploaded file: 
---------------------------------------------------------------
# program has function which only contains an if statement and a return in each branch

# Output:
# true
#

# Program:

begin
  bool f() is
    if true then
      return true
    else
      return false
    fi
  end
  bool x = call f();
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
functionConditionalReturn.s contents are:
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
11		# Stack pointer unchanged, no stack allocated arguments
12		call wacc_f
13		movq %rax, %r11
14		# Stack pointer unchanged, no stack allocated arguments
15		movq %r11, %rax
16		movq %rax, %r12
17		# Stack pointer unchanged, no stack allocated arguments
18		movq %r12, %rax
19		movq %rax, %rdi
20		# statement primitives do not return results (but will clobber r0/rax)
21		call _printb
22		call _println
23		# Stack pointer unchanged, no stack allocated variables
24		movq $0, %rax
25		# popq {%rbx, %r12}
26		movq (%rsp), %rbx
27		movq 8(%rsp), %r12
28		addq $16, %rsp
29		popq %rbp
30		ret
31	
32	wacc_f:
33		pushq %rbp
34		movq %rsp, %rbp
35		movq $1, %rax
36		cmpq $1, %rax
37		je .L0
38		movq $0, %rax
39		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
40		movq %rbp, %rsp
41		popq %rbp
42		ret
43		jmp .L1
44	.L0:
45		movq $1, %rax
46		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
47		movq %rbp, %rsp
48		popq %rbp
49		ret
50	.L1:
51		# 'ere be dragons: this is 100% dead code, functions always end in returns!
52	
53	.section .rodata
54	# length of .L._printb_str0
55		.int 5
56	.L._printb_str0:
57		.asciz "false"
58	# length of .L._printb_str1
59		.int 4
60	.L._printb_str1:
61		.asciz "true"
62	# length of .L._printb_str2
63		.int 4
64	.L._printb_str2:
65		.asciz "%.*s"
66	.text
67	_printb:
68		pushq %rbp
69		movq %rsp, %rbp
70		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
71		andq $-16, %rsp
72		cmpb $0, %dil
73		jne .L_printb0
74		leaq .L._printb_str0(%rip), %rdx
75		jmp .L_printb1
76	.L_printb0:
77		leaq .L._printb_str1(%rip), %rdx
78	.L_printb1:
79		movl -4(%rdx), %esi
80		leaq .L._printb_str2(%rip), %rdi
81		# on x86, al represents the number of SIMD registers used as variadic arguments
82		movb $0, %al
83		call printf@plt
84		movq $0, %rdi
85		call fflush@plt
86		movq %rbp, %rsp
87		popq %rbp
88		ret
89	
90	.section .rodata
91	# length of .L._println_str0
92		.int 0
93	.L._println_str0:
94		.asciz ""
95	.text
96	_println:
97		pushq %rbp
98		movq %rsp, %rbp
99		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
100		andq $-16, %rsp
101		leaq .L._println_str0(%rip), %rdi
102		call puts@plt
103		movq $0, %rdi
104		call fflush@plt
105		movq %rbp, %rsp
106		popq %rbp
107		ret
===========================================================
-- Finished

