./valid/function/simple_functions/lotsOfLocals.wacc
calling the reference compiler on ./valid/function/simple_functions/lotsOfLocals.wacc
-- Test: lotsOfLocals.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Functions with lots of local arguments should work ok
# Thanks to Kristina Zimina for finding this

# Output:
# 5
# 8
#

# Program:

begin
    int f (int a, int b, int c, int d, int e, int k) is
        int x1 = 1;
        int x2 = 2;
        int x3 = 3;
        int x4 = 4;
        char x5 = '5';
        char x6 = '6';
        char x7 = '7';
        int x8 = 8;
        begin
            char y1 = 'a';
            char y2 = 'b';
            int y = 5 ;
            println y
        end ;
        return x8
    end

    int r = call f(1, 2, 3, 4, 5, 6);
    println r
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
lotsOfLocals.s contents are:
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
12		movq $1, %rax
13		movq %rax, %rdi
14		movq $2, %rax
15		movq %rax, %rsi
16		movq $3, %rax
17		movq %rax, %rdx
18		movq $4, %rax
19		movq %rax, %rcx
20		movq $5, %rax
21		movq %rax, %r8
22		movq $6, %rax
23		movq %rax, %r9
24		call wacc_f
25		movq %rax, %r11
26		# Stack pointer unchanged, no stack allocated arguments
27		movq %r11, %rax
28		movq %rax, %r12
29		# Stack pointer unchanged, no stack allocated arguments
30		movq %r12, %rax
31		movq %rax, %rdi
32		# statement primitives do not return results (but will clobber r0/rax)
33		call _printi
34		call _println
35		# Stack pointer unchanged, no stack allocated variables
36		movq $0, %rax
37		# popq {%rbx, %r12}
38		movq (%rsp), %rbx
39		movq 8(%rsp), %r12
40		addq $16, %rsp
41		popq %rbp
42		ret
43	
44	wacc_f:
45		pushq %rbp
46		# pushq {%r12, %r13, %r14, %r15}
47		subq $32, %rsp
48		movq %r12, (%rsp)
49		movq %r13, 8(%rsp)
50		movq %r14, 16(%rsp)
51		movq %r15, 24(%rsp)
52		movq %rsp, %rbp
53		subq $7, %rsp
54		movq $1, %rax
55		movq %rax, %r12
56		movq $2, %rax
57		movq %rax, %r13
58		movq $3, %rax
59		movq %rax, %r14
60		movq $4, %rax
61		movq %rax, %r15
62		movq $53, %rax
63		movb %al, -7(%rbp)
64		movq $54, %rax
65		movb %al, -6(%rbp)
66		movq $55, %rax
67		movb %al, -5(%rbp)
68		movq $8, %rax
69		movl %eax, -4(%rbp)
70		subq $6, %rsp
71		movq $97, %rax
72		movb %al, -13(%rbp)
73		movq $98, %rax
74		movb %al, -12(%rbp)
75		movq $5, %rax
76		movl %eax, -11(%rbp)
77		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
78		subq $48, %rsp
79		movq %rcx, (%rsp)
80		movq %rdx, 8(%rsp)
81		movq %rsi, 16(%rsp)
82		movq %rdi, 24(%rsp)
83		movq %r8, 32(%rsp)
84		movq %r9, 40(%rsp)
85		# Set up R11 as a temporary second base pointer for the caller saved things
86		movq %rsp, %r11
87		# Stack pointer unchanged, no stack allocated arguments
88		movslq -11(%rbp), %rax
89		movq %rax, %rax
90		movq %rax, %rdi
91		# statement primitives do not return results (but will clobber r0/rax)
92		call _printi
93		call _println
94		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
95		movq (%rsp), %rcx
96		movq 8(%rsp), %rdx
97		movq 16(%rsp), %rsi
98		movq 24(%rsp), %rdi
99		movq 32(%rsp), %r8
100		movq 40(%rsp), %r9
101		addq $48, %rsp
102		addq $6, %rsp
103		movslq -4(%rbp), %rax
104		movq %rax, %rax
105		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
106		movq %rbp, %rsp
107		# popq {%r12, %r13, %r14, %r15}
108		movq (%rsp), %r12
109		movq 8(%rsp), %r13
110		movq 16(%rsp), %r14
111		movq 24(%rsp), %r15
112		addq $32, %rsp
113		popq %rbp
114		ret
115		addq $7, %rsp
116		# 'ere be dragons: this is 100% dead code, functions always end in returns!
117	
118	.section .rodata
119	# length of .L._printi_str0
120		.int 2
121	.L._printi_str0:
122		.asciz "%d"
123	.text
124	_printi:
125		pushq %rbp
126		movq %rsp, %rbp
127		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
128		andq $-16, %rsp
129		movl %edi, %esi
130		leaq .L._printi_str0(%rip), %rdi
131		# on x86, al represents the number of SIMD registers used as variadic arguments
132		movb $0, %al
133		call printf@plt
134		movq $0, %rdi
135		call fflush@plt
136		movq %rbp, %rsp
137		popq %rbp
138		ret
139	
140	.section .rodata
141	# length of .L._println_str0
142		.int 0
143	.L._println_str0:
144		.asciz ""
145	.text
146	_println:
147		pushq %rbp
148		movq %rsp, %rbp
149		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
150		andq $-16, %rsp
151		leaq .L._println_str0(%rip), %rdi
152		call puts@plt
153		movq $0, %rdi
154		call fflush@plt
155		movq %rbp, %rsp
156		popq %rbp
157		ret
===========================================================
-- Finished

