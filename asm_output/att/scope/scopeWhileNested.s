./valid/scope/scopeWhileNested.wacc
calling the reference compiler on ./valid/scope/scopeWhileNested.wacc
-- Test: scopeWhileNested.wacc

-- Uploaded file: 
---------------------------------------------------------------
# variable scoping nested within a while-loop

# Output:
# counting... 5
# counting... 4
# counting... 3
# counting... 2
# counting... 1
# 0 Boom!
#

# Program:

begin
  int x = 5 ;
  while x > 0 do
    begin
      string x = "counting... " ;
      print x
    end ;
    println x ;
    x = x - 1
  done ;
  print x ;
  println " Boom!"
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
scopeWhileNested.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 12
4	.L.str0:
5		.asciz "counting... "
6	# length of .L.str1
7		.int 6
8	.L.str1:
9		.asciz " Boom!"
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
20		movq $5, %rax
21		movq %rax, %r12
22		jmp .L0
23	.L1:
24		# Stack pointer unchanged, no stack allocated variables
25		leaq .L.str0(%rip), %rax
26		pushq %rax
27		popq %rax
28		movq %rax, %rax
29		movq %rax, %r13
30		# Stack pointer unchanged, no stack allocated arguments
31		movq %r13, %rax
32		movq %rax, %rdi
33		# statement primitives do not return results (but will clobber r0/rax)
34		call _prints
35		# Stack pointer unchanged, no stack allocated variables
36		# Stack pointer unchanged, no stack allocated arguments
37		movq %r12, %rax
38		movq %rax, %rdi
39		# statement primitives do not return results (but will clobber r0/rax)
40		call _printi
41		call _println
42		movl %r12d, %eax
43		subl $1, %eax
44		jo _errOverflow
45		movslq %eax, %rax
46		pushq %rax
47		popq %rax
48		movq %rax, %rax
49		movq %rax, %r12
50	.L0:
51		cmpq $0, %r12
52		jg .L1
53		# Stack pointer unchanged, no stack allocated arguments
54		movq %r12, %rax
55		movq %rax, %rdi
56		# statement primitives do not return results (but will clobber r0/rax)
57		call _printi
58		# Stack pointer unchanged, no stack allocated arguments
59		leaq .L.str1(%rip), %rax
60		pushq %rax
61		popq %rax
62		movq %rax, %rax
63		movq %rax, %rdi
64		# statement primitives do not return results (but will clobber r0/rax)
65		call _prints
66		call _println
67		# Stack pointer unchanged, no stack allocated variables
68		movq $0, %rax
69		# popq {%rbx, %r12, %r13}
70		movq (%rsp), %rbx
71		movq 8(%rsp), %r12
72		movq 16(%rsp), %r13
73		addq $24, %rsp
74		popq %rbp
75		ret
76	
77	.section .rodata
78	# length of .L._prints_str0
79		.int 4
80	.L._prints_str0:
81		.asciz "%.*s"
82	.text
83	_prints:
84		pushq %rbp
85		movq %rsp, %rbp
86		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
87		andq $-16, %rsp
88		movq %rdi, %rdx
89		movl -4(%rdi), %esi
90		leaq .L._prints_str0(%rip), %rdi
91		# on x86, al represents the number of SIMD registers used as variadic arguments
92		movb $0, %al
93		call printf@plt
94		movq $0, %rdi
95		call fflush@plt
96		movq %rbp, %rsp
97		popq %rbp
98		ret
99	
100	.section .rodata
101	# length of .L._printi_str0
102		.int 2
103	.L._printi_str0:
104		.asciz "%d"
105	.text
106	_printi:
107		pushq %rbp
108		movq %rsp, %rbp
109		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
110		andq $-16, %rsp
111		movl %edi, %esi
112		leaq .L._printi_str0(%rip), %rdi
113		# on x86, al represents the number of SIMD registers used as variadic arguments
114		movb $0, %al
115		call printf@plt
116		movq $0, %rdi
117		call fflush@plt
118		movq %rbp, %rsp
119		popq %rbp
120		ret
121	
122	.section .rodata
123	# length of .L._println_str0
124		.int 0
125	.L._println_str0:
126		.asciz ""
127	.text
128	_println:
129		pushq %rbp
130		movq %rsp, %rbp
131		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
132		andq $-16, %rsp
133		leaq .L._println_str0(%rip), %rdi
134		call puts@plt
135		movq $0, %rdi
136		call fflush@plt
137		movq %rbp, %rsp
138		popq %rbp
139		ret
140	
141	_exit:
142		pushq %rbp
143		movq %rsp, %rbp
144		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
145		andq $-16, %rsp
146		call exit@plt
147		movq %rbp, %rsp
148		popq %rbp
149		ret
150	
151	.section .rodata
152	# length of .L._errOverflow_str0
153		.int 52
154	.L._errOverflow_str0:
155		.asciz "fatal error: integer overflow or underflow occurred\n"
156	.text
157	_errOverflow:
158		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
159		andq $-16, %rsp
160		leaq .L._errOverflow_str0(%rip), %rdi
161		call _prints
162		movb $-1, %dil
163		call exit@plt
===========================================================
-- Finished

