./valid/function/simple_functions/functionMultiReturns.wacc
calling the reference compiler on ./valid/function/simple_functions/functionMultiReturns.wacc
-- Test: functionMultiReturns.wacc

-- Uploaded file: 
---------------------------------------------------------------
# a simple function with multiple returns, importantly one at the end.

# Output:
# 1
#

# Program:

begin
  int returnInWhile() is
    while true do
      return 1 ;
      println "How on Earth did we get here?"
    done ;
    return 2
  end

  int x = call returnInWhile() ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
functionMultiReturns.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 29
4	.L.str0:
5		.asciz "How on Earth did we get here?"
6	.text
7	main:
8		pushq %rbp
9		# pushq {%rbx, %r12}
10		subq $16, %rsp
11		movq %rbx, (%rsp)
12		movq %r12, 8(%rsp)
13		movq %rsp, %rbp
14		# Stack pointer unchanged, no stack allocated variables
15		# Stack pointer unchanged, no stack allocated arguments
16		call wacc_returnInWhile
17		movq %rax, %r11
18		# Stack pointer unchanged, no stack allocated arguments
19		movq %r11, %rax
20		movq %rax, %r12
21		# Stack pointer unchanged, no stack allocated arguments
22		movq %r12, %rax
23		movq %rax, %rdi
24		# statement primitives do not return results (but will clobber r0/rax)
25		call _printi
26		call _println
27		# Stack pointer unchanged, no stack allocated variables
28		movq $0, %rax
29		# popq {%rbx, %r12}
30		movq (%rsp), %rbx
31		movq 8(%rsp), %r12
32		addq $16, %rsp
33		popq %rbp
34		ret
35	
36	wacc_returnInWhile:
37		pushq %rbp
38		movq %rsp, %rbp
39		jmp .L0
40	.L1:
41		movq $1, %rax
42		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
43		movq %rbp, %rsp
44		popq %rbp
45		ret
46		# Stack pointer unchanged, no stack allocated arguments
47		leaq .L.str0(%rip), %rax
48		pushq %rax
49		popq %rax
50		movq %rax, %rax
51		movq %rax, %rdi
52		# statement primitives do not return results (but will clobber r0/rax)
53		call _prints
54		call _println
55	.L0:
56		movq $1, %rax
57		cmpq $1, %rax
58		je .L1
59		movq $2, %rax
60		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
61		movq %rbp, %rsp
62		popq %rbp
63		ret
64		# 'ere be dragons: this is 100% dead code, functions always end in returns!
65	
66	.section .rodata
67	# length of .L._prints_str0
68		.int 4
69	.L._prints_str0:
70		.asciz "%.*s"
71	.text
72	_prints:
73		pushq %rbp
74		movq %rsp, %rbp
75		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
76		andq $-16, %rsp
77		movq %rdi, %rdx
78		movl -4(%rdi), %esi
79		leaq .L._prints_str0(%rip), %rdi
80		# on x86, al represents the number of SIMD registers used as variadic arguments
81		movb $0, %al
82		call printf@plt
83		movq $0, %rdi
84		call fflush@plt
85		movq %rbp, %rsp
86		popq %rbp
87		ret
88	
89	.section .rodata
90	# length of .L._printi_str0
91		.int 2
92	.L._printi_str0:
93		.asciz "%d"
94	.text
95	_printi:
96		pushq %rbp
97		movq %rsp, %rbp
98		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
99		andq $-16, %rsp
100		movl %edi, %esi
101		leaq .L._printi_str0(%rip), %rdi
102		# on x86, al represents the number of SIMD registers used as variadic arguments
103		movb $0, %al
104		call printf@plt
105		movq $0, %rdi
106		call fflush@plt
107		movq %rbp, %rsp
108		popq %rbp
109		ret
110	
111	.section .rodata
112	# length of .L._println_str0
113		.int 0
114	.L._println_str0:
115		.asciz ""
116	.text
117	_println:
118		pushq %rbp
119		movq %rsp, %rbp
120		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
121		andq $-16, %rsp
122		leaq .L._println_str0(%rip), %rdi
123		call puts@plt
124		movq $0, %rdi
125		call fflush@plt
126		movq %rbp, %rsp
127		popq %rbp
128		ret
===========================================================
-- Finished

