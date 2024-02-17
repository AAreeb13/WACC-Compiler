./valid/scope/scopeIfRedefine.wacc
calling the reference compiler on ./valid/scope/scopeIfRedefine.wacc
-- Test: scopeIfRedefine.wacc

-- Uploaded file: 
---------------------------------------------------------------
# variable scoping test that redefines a variable within an if-statement

# Output:
# true
# 12
#

# Program:

begin
  int x = 12 ;
  if x == 12 then
    bool x = true ;
    println x
  else
    char x = 'a';
    println x
  fi ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
scopeIfRedefine.s contents are:
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
14		cmpq $12, %r12
15		je .L0
16		# Stack pointer unchanged, no stack allocated variables
17		movq $97, %rax
18		movq %rax, %r13
19		# Stack pointer unchanged, no stack allocated arguments
20		movq %r13, %rax
21		movq %rax, %rdi
22		# statement primitives do not return results (but will clobber r0/rax)
23		call _printc
24		call _println
25		# Stack pointer unchanged, no stack allocated variables
26		jmp .L1
27	.L0:
28		# Stack pointer unchanged, no stack allocated variables
29		movq $1, %rax
30		movq %rax, %r13
31		# Stack pointer unchanged, no stack allocated arguments
32		movq %r13, %rax
33		movq %rax, %rdi
34		# statement primitives do not return results (but will clobber r0/rax)
35		call _printb
36		call _println
37		# Stack pointer unchanged, no stack allocated variables
38	.L1:
39		# Stack pointer unchanged, no stack allocated arguments
40		movq %r12, %rax
41		movq %rax, %rdi
42		# statement primitives do not return results (but will clobber r0/rax)
43		call _printi
44		call _println
45		# Stack pointer unchanged, no stack allocated variables
46		movq $0, %rax
47		# popq {%rbx, %r12, %r13}
48		movq (%rsp), %rbx
49		movq 8(%rsp), %r12
50		movq 16(%rsp), %r13
51		addq $24, %rsp
52		popq %rbp
53		ret
54	
55	.section .rodata
56	# length of .L._printi_str0
57		.int 2
58	.L._printi_str0:
59		.asciz "%d"
60	.text
61	_printi:
62		pushq %rbp
63		movq %rsp, %rbp
64		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
65		andq $-16, %rsp
66		movl %edi, %esi
67		leaq .L._printi_str0(%rip), %rdi
68		# on x86, al represents the number of SIMD registers used as variadic arguments
69		movb $0, %al
70		call printf@plt
71		movq $0, %rdi
72		call fflush@plt
73		movq %rbp, %rsp
74		popq %rbp
75		ret
76	
77	.section .rodata
78	# length of .L._printb_str0
79		.int 5
80	.L._printb_str0:
81		.asciz "false"
82	# length of .L._printb_str1
83		.int 4
84	.L._printb_str1:
85		.asciz "true"
86	# length of .L._printb_str2
87		.int 4
88	.L._printb_str2:
89		.asciz "%.*s"
90	.text
91	_printb:
92		pushq %rbp
93		movq %rsp, %rbp
94		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
95		andq $-16, %rsp
96		cmpb $0, %dil
97		jne .L_printb0
98		leaq .L._printb_str0(%rip), %rdx
99		jmp .L_printb1
100	.L_printb0:
101		leaq .L._printb_str1(%rip), %rdx
102	.L_printb1:
103		movl -4(%rdx), %esi
104		leaq .L._printb_str2(%rip), %rdi
105		# on x86, al represents the number of SIMD registers used as variadic arguments
106		movb $0, %al
107		call printf@plt
108		movq $0, %rdi
109		call fflush@plt
110		movq %rbp, %rsp
111		popq %rbp
112		ret
113	
114	.section .rodata
115	# length of .L._printc_str0
116		.int 2
117	.L._printc_str0:
118		.asciz "%c"
119	.text
120	_printc:
121		pushq %rbp
122		movq %rsp, %rbp
123		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
124		andq $-16, %rsp
125		movb %dil, %sil
126		leaq .L._printc_str0(%rip), %rdi
127		# on x86, al represents the number of SIMD registers used as variadic arguments
128		movb $0, %al
129		call printf@plt
130		movq $0, %rdi
131		call fflush@plt
132		movq %rbp, %rsp
133		popq %rbp
134		ret
135	
136	.section .rodata
137	# length of .L._println_str0
138		.int 0
139	.L._println_str0:
140		.asciz ""
141	.text
142	_println:
143		pushq %rbp
144		movq %rsp, %rbp
145		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
146		andq $-16, %rsp
147		leaq .L._println_str0(%rip), %rdi
148		call puts@plt
149		movq $0, %rdi
150		call fflush@plt
151		movq %rbp, %rsp
152		popq %rbp
153		ret
===========================================================
-- Finished

