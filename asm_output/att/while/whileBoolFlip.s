./valid/while/whileBoolFlip.wacc
calling the reference compiler on ./valid/while/whileBoolFlip.wacc
-- Test: whileBoolFlip.wacc

-- Uploaded file: 
---------------------------------------------------------------
# while loop flips bool to terminate

# Output:
# flip b!
# end of loop
#

# Program:

begin
  bool b = true ;
  while b do
    println "flip b!" ;
    b = !b
  done ;
  println "end of loop"
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
whileBoolFlip.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 7
4	.L.str0:
5		.asciz "flip b!"
6	# length of .L.str1
7		.int 11
8	.L.str1:
9		.asciz "end of loop"
10	.text
11	main:
12		pushq %rbp
13		# pushq {%rbx, %r12}
14		subq $16, %rsp
15		movq %rbx, (%rsp)
16		movq %r12, 8(%rsp)
17		movq %rsp, %rbp
18		# Stack pointer unchanged, no stack allocated variables
19		movq $1, %rax
20		movq %rax, %r12
21		jmp .L0
22	.L1:
23		# Stack pointer unchanged, no stack allocated arguments
24		leaq .L.str0(%rip), %rax
25		pushq %rax
26		popq %rax
27		movq %rax, %rax
28		movq %rax, %rdi
29		# statement primitives do not return results (but will clobber r0/rax)
30		call _prints
31		call _println
32		cmpq $1, %r12
33		setne %al
34		movsbq %al, %rax
35		pushq %rax
36		popq %rax
37		movq %rax, %rax
38		movq %rax, %r12
39	.L0:
40		cmpq $1, %r12
41		je .L1
42		# Stack pointer unchanged, no stack allocated arguments
43		leaq .L.str1(%rip), %rax
44		pushq %rax
45		popq %rax
46		movq %rax, %rax
47		movq %rax, %rdi
48		# statement primitives do not return results (but will clobber r0/rax)
49		call _prints
50		call _println
51		# Stack pointer unchanged, no stack allocated variables
52		movq $0, %rax
53		# popq {%rbx, %r12}
54		movq (%rsp), %rbx
55		movq 8(%rsp), %r12
56		addq $16, %rsp
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

