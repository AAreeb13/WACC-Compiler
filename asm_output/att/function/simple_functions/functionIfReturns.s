./valid/function/simple_functions/functionIfReturns.wacc
calling the reference compiler on ./valid/function/simple_functions/functionIfReturns.wacc
-- Test: functionIfReturns.wacc

-- Uploaded file: 
---------------------------------------------------------------
# a simple function with nested returns inside an if-statement after a return

# Output:
# go
# 1
#

begin
    int f() is
        println "go";
        return 1;
        if true then
            println "a";
            return 2
        else
            println "b";
            return 3
        fi
    end

    int ret = call f();
    println ret
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
functionIfReturns.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 2
4	.L.str0:
5		.asciz "go"
6	# length of .L.str1
7		.int 1
8	.L.str1:
9		.asciz "b"
10	# length of .L.str2
11		.int 1
12	.L.str2:
13		.asciz "a"
14	.text
15	main:
16		pushq %rbp
17		# pushq {%rbx, %r12}
18		subq $16, %rsp
19		movq %rbx, (%rsp)
20		movq %r12, 8(%rsp)
21		movq %rsp, %rbp
22		# Stack pointer unchanged, no stack allocated variables
23		# Stack pointer unchanged, no stack allocated arguments
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
46		movq %rsp, %rbp
47		# Stack pointer unchanged, no stack allocated arguments
48		leaq .L.str0(%rip), %rax
49		pushq %rax
50		popq %rax
51		movq %rax, %rax
52		movq %rax, %rdi
53		# statement primitives do not return results (but will clobber r0/rax)
54		call _prints
55		call _println
56		movq $1, %rax
57		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
58		movq %rbp, %rsp
59		popq %rbp
60		ret
61		movq $1, %rax
62		cmpq $1, %rax
63		je .L0
64		# Stack pointer unchanged, no stack allocated arguments
65		leaq .L.str1(%rip), %rax
66		pushq %rax
67		popq %rax
68		movq %rax, %rax
69		movq %rax, %rdi
70		# statement primitives do not return results (but will clobber r0/rax)
71		call _prints
72		call _println
73		movq $3, %rax
74		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
75		movq %rbp, %rsp
76		popq %rbp
77		ret
78		jmp .L1
79	.L0:
80		# Stack pointer unchanged, no stack allocated arguments
81		leaq .L.str2(%rip), %rax
82		pushq %rax
83		popq %rax
84		movq %rax, %rax
85		movq %rax, %rdi
86		# statement primitives do not return results (but will clobber r0/rax)
87		call _prints
88		call _println
89		movq $2, %rax
90		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
91		movq %rbp, %rsp
92		popq %rbp
93		ret
94	.L1:
95		# 'ere be dragons: this is 100% dead code, functions always end in returns!
96	
97	.section .rodata
98	# length of .L._prints_str0
99		.int 4
100	.L._prints_str0:
101		.asciz "%.*s"
102	.text
103	_prints:
104		pushq %rbp
105		movq %rsp, %rbp
106		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
107		andq $-16, %rsp
108		movq %rdi, %rdx
109		movl -4(%rdi), %esi
110		leaq .L._prints_str0(%rip), %rdi
111		# on x86, al represents the number of SIMD registers used as variadic arguments
112		movb $0, %al
113		call printf@plt
114		movq $0, %rdi
115		call fflush@plt
116		movq %rbp, %rsp
117		popq %rbp
118		ret
119	
120	.section .rodata
121	# length of .L._printi_str0
122		.int 2
123	.L._printi_str0:
124		.asciz "%d"
125	.text
126	_printi:
127		pushq %rbp
128		movq %rsp, %rbp
129		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
130		andq $-16, %rsp
131		movl %edi, %esi
132		leaq .L._printi_str0(%rip), %rdi
133		# on x86, al represents the number of SIMD registers used as variadic arguments
134		movb $0, %al
135		call printf@plt
136		movq $0, %rdi
137		call fflush@plt
138		movq %rbp, %rsp
139		popq %rbp
140		ret
141	
142	.section .rodata
143	# length of .L._println_str0
144		.int 0
145	.L._println_str0:
146		.asciz ""
147	.text
148	_println:
149		pushq %rbp
150		movq %rsp, %rbp
151		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
152		andq $-16, %rsp
153		leaq .L._println_str0(%rip), %rdi
154		call puts@plt
155		movq $0, %rdi
156		call fflush@plt
157		movq %rbp, %rsp
158		popq %rbp
159		ret
===========================================================
-- Finished

