./valid/if/if3.wacc
calling the reference compiler on ./valid/if/if3.wacc
-- Test: if3.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Simple conditional statement with int comparison test

# Output:
# correct
#

# Program:

begin
  int a = 13;
  int b = 37;
  if a < b
  then
    println "correct"
  else
    println "incorrect"
  fi
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
if3.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 9
4	.L.str0:
5		.asciz "incorrect"
6	# length of .L.str1
7		.int 7
8	.L.str1:
9		.asciz "correct"
10	.text
11	main:
12		pushq %rbp
13		# pushq {%rbx, %r12, %r13}
14		subq $24, %rsp
15		movq %rbx, (%rsp)
16		movq %r12, 8(%rsp)
17		movq %r13, 16(%rsp)
18		movq %rsp, %rbp
19		# Stack pointer unchanged, no stack allocated variables
20		movq $13, %rax
21		movq %rax, %r12
22		movq $37, %rax
23		movq %rax, %r13
24		cmpq %r13, %r12
25		jl .L0
26		# Stack pointer unchanged, no stack allocated arguments
27		leaq .L.str0(%rip), %rax
28		pushq %rax
29		popq %rax
30		movq %rax, %rax
31		movq %rax, %rdi
32		# statement primitives do not return results (but will clobber r0/rax)
33		call _prints
34		call _println
35		jmp .L1
36	.L0:
37		# Stack pointer unchanged, no stack allocated arguments
38		leaq .L.str1(%rip), %rax
39		pushq %rax
40		popq %rax
41		movq %rax, %rax
42		movq %rax, %rdi
43		# statement primitives do not return results (but will clobber r0/rax)
44		call _prints
45		call _println
46	.L1:
47		# Stack pointer unchanged, no stack allocated variables
48		movq $0, %rax
49		# popq {%rbx, %r12, %r13}
50		movq (%rsp), %rbx
51		movq 8(%rsp), %r12
52		movq 16(%rsp), %r13
53		addq $24, %rsp
54		popq %rbp
55		ret
56	
57	.section .rodata
58	# length of .L._prints_str0
59		.int 4
60	.L._prints_str0:
61		.asciz "%.*s"
62	.text
63	_prints:
64		pushq %rbp
65		movq %rsp, %rbp
66		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
67		andq $-16, %rsp
68		movq %rdi, %rdx
69		movl -4(%rdi), %esi
70		leaq .L._prints_str0(%rip), %rdi
71		# on x86, al represents the number of SIMD registers used as variadic arguments
72		movb $0, %al
73		call printf@plt
74		movq $0, %rdi
75		call fflush@plt
76		movq %rbp, %rsp
77		popq %rbp
78		ret
79	
80	.section .rodata
81	# length of .L._println_str0
82		.int 0
83	.L._println_str0:
84		.asciz ""
85	.text
86	_println:
87		pushq %rbp
88		movq %rsp, %rbp
89		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
90		andq $-16, %rsp
91		leaq .L._println_str0(%rip), %rdi
92		call puts@plt
93		movq $0, %rdi
94		call fflush@plt
95		movq %rbp, %rsp
96		popq %rbp
97		ret
===========================================================
-- Finished

