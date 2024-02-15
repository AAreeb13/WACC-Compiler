./valid/if/if1.wacc
calling the reference compiler on ./valid/if/if1.wacc
-- Test: if1.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Simple conditional statement with int comparison test

# Output:
# correct
#

# Program:

begin
  int a = 13;
  if a == 13
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
if1.s contents are:
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
13		# pushq {%rbx, %r12}
14		subq $16, %rsp
15		movq %rbx, (%rsp)
16		movq %r12, 8(%rsp)
17		movq %rsp, %rbp
18		# Stack pointer unchanged, no stack allocated variables
19		movq $13, %rax
20		movq %rax, %r12
21		cmpq $13, %r12
22		je .L0
23		# Stack pointer unchanged, no stack allocated arguments
24		leaq .L.str0(%rip), %rax
25		pushq %rax
26		popq %rax
27		movq %rax, %rax
28		movq %rax, %rdi
29		# statement primitives do not return results (but will clobber r0/rax)
30		call _prints
31		call _println
32		jmp .L1
33	.L0:
34		# Stack pointer unchanged, no stack allocated arguments
35		leaq .L.str1(%rip), %rax
36		pushq %rax
37		popq %rax
38		movq %rax, %rax
39		movq %rax, %rdi
40		# statement primitives do not return results (but will clobber r0/rax)
41		call _prints
42		call _println
43	.L1:
44		# Stack pointer unchanged, no stack allocated variables
45		movq $0, %rax
46		# popq {%rbx, %r12}
47		movq (%rsp), %rbx
48		movq 8(%rsp), %r12
49		addq $16, %rsp
50		popq %rbp
51		ret
52	
53	.section .rodata
54	# length of .L._prints_str0
55		.int 4
56	.L._prints_str0:
57		.asciz "%.*s"
58	.text
59	_prints:
60		pushq %rbp
61		movq %rsp, %rbp
62		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
63		andq $-16, %rsp
64		movq %rdi, %rdx
65		movl -4(%rdi), %esi
66		leaq .L._prints_str0(%rip), %rdi
67		# on x86, al represents the number of SIMD registers used as variadic arguments
68		movb $0, %al
69		call printf@plt
70		movq $0, %rdi
71		call fflush@plt
72		movq %rbp, %rsp
73		popq %rbp
74		ret
75	
76	.section .rodata
77	# length of .L._println_str0
78		.int 0
79	.L._println_str0:
80		.asciz ""
81	.text
82	_println:
83		pushq %rbp
84		movq %rsp, %rbp
85		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
86		andq $-16, %rsp
87		leaq .L._println_str0(%rip), %rdi
88		call puts@plt
89		movq $0, %rdi
90		call fflush@plt
91		movq %rbp, %rsp
92		popq %rbp
93		ret
===========================================================
-- Finished

