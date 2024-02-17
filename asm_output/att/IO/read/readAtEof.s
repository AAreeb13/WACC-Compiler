./valid/IO/read/readAtEof.wacc
calling the reference compiler on ./valid/IO/read/readAtEof.wacc
-- Test: readAtEof.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Read should not alter nothing if it can't read anything
# Thanks for Nixon Moony-Enraght for catching this!

# Input: X

# Output:
# XZ
#

# Program:
begin
    char c = 'Z';
    read c;
    print c;

    char c2 = 'Z';
    read c2;
    println c2
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
readAtEof.s contents are:
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
12		movq $90, %rax
13		movq %rax, %r12
14		# Stack pointer unchanged, no stack allocated arguments
15		# load the current value in the destination of the read so it supports defaults
16		movq %r12, %rax
17		movq %rax, %rdi
18		call _readc
19		movq %rax, %r11
20		movq %r11, %rax
21		movq %rax, %r12
22		# Stack pointer unchanged, no stack allocated arguments
23		movq %r12, %rax
24		movq %rax, %rdi
25		# statement primitives do not return results (but will clobber r0/rax)
26		call _printc
27		movq $90, %rax
28		movq %rax, %r13
29		# Stack pointer unchanged, no stack allocated arguments
30		# load the current value in the destination of the read so it supports defaults
31		movq %r13, %rax
32		movq %rax, %rdi
33		call _readc
34		movq %rax, %r11
35		movq %r11, %rax
36		movq %rax, %r13
37		# Stack pointer unchanged, no stack allocated arguments
38		movq %r13, %rax
39		movq %rax, %rdi
40		# statement primitives do not return results (but will clobber r0/rax)
41		call _printc
42		call _println
43		# Stack pointer unchanged, no stack allocated variables
44		movq $0, %rax
45		# popq {%rbx, %r12, %r13}
46		movq (%rsp), %rbx
47		movq 8(%rsp), %r12
48		movq 16(%rsp), %r13
49		addq $24, %rsp
50		popq %rbp
51		ret
52	
53	.section .rodata
54	# length of .L._readc_str0
55		.int 3
56	.L._readc_str0:
57		.asciz " %c"
58	.text
59	_readc:
60		pushq %rbp
61		movq %rsp, %rbp
62		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
63		andq $-16, %rsp
64		# RDI contains the "original" value of the destination of the read
65		# allocate space on the stack to store the read: preserve alignment!
66		# the passed default argument should be stored in case of EOF
67		subq $16, %rsp
68		movb %dil, (%rsp)
69		leaq (%rsp), %rsi
70		leaq .L._readc_str0(%rip), %rdi
71		# on x86, al represents the number of SIMD registers used as variadic arguments
72		movb $0, %al
73		call scanf@plt
74		movsbq (%rsp), %rax
75		addq $16, %rsp
76		movq %rbp, %rsp
77		popq %rbp
78		ret
79	
80	.section .rodata
81	# length of .L._printc_str0
82		.int 2
83	.L._printc_str0:
84		.asciz "%c"
85	.text
86	_printc:
87		pushq %rbp
88		movq %rsp, %rbp
89		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
90		andq $-16, %rsp
91		movb %dil, %sil
92		leaq .L._printc_str0(%rip), %rdi
93		# on x86, al represents the number of SIMD registers used as variadic arguments
94		movb $0, %al
95		call printf@plt
96		movq $0, %rdi
97		call fflush@plt
98		movq %rbp, %rsp
99		popq %rbp
100		ret
101	
102	.section .rodata
103	# length of .L._println_str0
104		.int 0
105	.L._println_str0:
106		.asciz ""
107	.text
108	_println:
109		pushq %rbp
110		movq %rsp, %rbp
111		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
112		andq $-16, %rsp
113		leaq .L._println_str0(%rip), %rdi
114		call puts@plt
115		movq $0, %rdi
116		call fflush@plt
117		movq %rbp, %rsp
118		popq %rbp
119		ret
===========================================================
-- Finished

