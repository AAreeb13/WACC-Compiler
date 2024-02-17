./valid/scope/ifNested1.wacc
calling the reference compiler on ./valid/scope/ifNested1.wacc
-- Test: ifNested1.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Nested conditional statement

# Output:
# correct
#

# Program:

begin
  int a = 13;
  if a == 13
  then
    if a > 5
    then
      println "correct"
    else
      println "incorrect"
    fi
  else
    println "incorrect"
  fi
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
ifNested1.s contents are:
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
34		cmpq $5, %r12
35		jg .L2
36		# Stack pointer unchanged, no stack allocated arguments
37		leaq .L.str0(%rip), %rax
38		pushq %rax
39		popq %rax
40		movq %rax, %rax
41		movq %rax, %rdi
42		# statement primitives do not return results (but will clobber r0/rax)
43		call _prints
44		call _println
45		jmp .L3
46	.L2:
47		# Stack pointer unchanged, no stack allocated arguments
48		leaq .L.str1(%rip), %rax
49		pushq %rax
50		popq %rax
51		movq %rax, %rax
52		movq %rax, %rdi
53		# statement primitives do not return results (but will clobber r0/rax)
54		call _prints
55		call _println
56	.L3:
57	.L1:
58		# Stack pointer unchanged, no stack allocated variables
59		movq $0, %rax
60		# popq {%rbx, %r12}
61		movq (%rsp), %rbx
62		movq 8(%rsp), %r12
63		addq $16, %rsp
64		popq %rbp
65		ret
66	
67	.section .rodata
68	# length of .L._prints_str0
69		.int 4
70	.L._prints_str0:
71		.asciz "%.*s"
72	.text
73	_prints:
74		pushq %rbp
75		movq %rsp, %rbp
76		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
77		andq $-16, %rsp
78		movq %rdi, %rdx
79		movl -4(%rdi), %esi
80		leaq .L._prints_str0(%rip), %rdi
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

