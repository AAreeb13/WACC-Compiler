./valid/if/if5.wacc
calling the reference compiler on ./valid/if/if5.wacc
-- Test: if5.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Simple conditional statement with boolen expression test

# Output:
# correct
#

# Program:

begin
  bool b = true;
  bool c = false;
  if b || c
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
if5.s contents are:
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
20		movq $1, %rax
21		movq %rax, %r12
22		movq $0, %rax
23		movq %rax, %r13
24		cmpq $1, %r12
25		je .L2
26		cmpq $1, %r13
27	.L2:
28		je .L0
29		# Stack pointer unchanged, no stack allocated arguments
30		leaq .L.str0(%rip), %rax
31		pushq %rax
32		popq %rax
33		movq %rax, %rax
34		movq %rax, %rdi
35		# statement primitives do not return results (but will clobber r0/rax)
36		call _prints
37		call _println
38		jmp .L1
39	.L0:
40		# Stack pointer unchanged, no stack allocated arguments
41		leaq .L.str1(%rip), %rax
42		pushq %rax
43		popq %rax
44		movq %rax, %rax
45		movq %rax, %rdi
46		# statement primitives do not return results (but will clobber r0/rax)
47		call _prints
48		call _println
49	.L1:
50		# Stack pointer unchanged, no stack allocated variables
51		movq $0, %rax
52		# popq {%rbx, %r12, %r13}
53		movq (%rsp), %rbx
54		movq 8(%rsp), %r12
55		movq 16(%rsp), %r13
56		addq $24, %rsp
57		popq %rbp
58		ret
59	
60	.section .rodata
61	# length of .L._prints_str0
62		.int 4
63	.L._prints_str0:
64		.asciz "%.*s"
65	.text
66	_prints:
67		pushq %rbp
68		movq %rsp, %rbp
69		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
70		andq $-16, %rsp
71		movq %rdi, %rdx
72		movl -4(%rdi), %esi
73		leaq .L._prints_str0(%rip), %rdi
74		# on x86, al represents the number of SIMD registers used as variadic arguments
75		movb $0, %al
76		call printf@plt
77		movq $0, %rdi
78		call fflush@plt
79		movq %rbp, %rsp
80		popq %rbp
81		ret
82	
83	.section .rodata
84	# length of .L._println_str0
85		.int 0
86	.L._println_str0:
87		.asciz ""
88	.text
89	_println:
90		pushq %rbp
91		movq %rsp, %rbp
92		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
93		andq $-16, %rsp
94		leaq .L._println_str0(%rip), %rdi
95		call puts@plt
96		movq $0, %rdi
97		call fflush@plt
98		movq %rbp, %rsp
99		popq %rbp
100		ret
===========================================================
-- Finished

