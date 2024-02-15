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
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	.text
4	main:
5		push rbp
6		# push {rbx, r12, r13}
7		sub rsp, 24
8		mov qword ptr [rsp], rbx
9		mov qword ptr [rsp + 8], r12
10		mov qword ptr [rsp + 16], r13
11		mov rbp, rsp
12		# Stack pointer unchanged, no stack allocated variables
13		mov rax, 90
14		mov r12, rax
15		# Stack pointer unchanged, no stack allocated arguments
16		# load the current value in the destination of the read so it supports defaults
17		mov rax, r12
18		mov rdi, rax
19		call _readc
20		mov r11, rax
21		mov rax, r11
22		mov r12, rax
23		# Stack pointer unchanged, no stack allocated arguments
24		mov rax, r12
25		mov rdi, rax
26		# statement primitives do not return results (but will clobber r0/rax)
27		call _printc
28		mov rax, 90
29		mov r13, rax
30		# Stack pointer unchanged, no stack allocated arguments
31		# load the current value in the destination of the read so it supports defaults
32		mov rax, r13
33		mov rdi, rax
34		call _readc
35		mov r11, rax
36		mov rax, r11
37		mov r13, rax
38		# Stack pointer unchanged, no stack allocated arguments
39		mov rax, r13
40		mov rdi, rax
41		# statement primitives do not return results (but will clobber r0/rax)
42		call _printc
43		call _println
44		# Stack pointer unchanged, no stack allocated variables
45		mov rax, 0
46		# pop {rbx, r12, r13}
47		mov rbx, qword ptr [rsp]
48		mov r12, qword ptr [rsp + 8]
49		mov r13, qword ptr [rsp + 16]
50		add rsp, 24
51		pop rbp
52		ret
53	
54	.section .rodata
55	# length of .L._readc_str0
56		.int 3
57	.L._readc_str0:
58		.asciz " %c"
59	.text
60	_readc:
61		push rbp
62		mov rbp, rsp
63		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
64		and rsp, -16
65		# RDI contains the "original" value of the destination of the read
66		# allocate space on the stack to store the read: preserve alignment!
67		# the passed default argument should be stored in case of EOF
68		sub rsp, 16
69		mov byte ptr [rsp], dil
70		lea rsi, qword ptr [rsp]
71		lea rdi, [rip + .L._readc_str0]
72		# on x86, al represents the number of SIMD registers used as variadic arguments
73		mov al, 0
74		call scanf@plt
75		movsx rax, byte ptr [rsp]
76		add rsp, 16
77		mov rsp, rbp
78		pop rbp
79		ret
80	
81	.section .rodata
82	# length of .L._printc_str0
83		.int 2
84	.L._printc_str0:
85		.asciz "%c"
86	.text
87	_printc:
88		push rbp
89		mov rbp, rsp
90		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
91		and rsp, -16
92		mov sil, dil
93		lea rdi, [rip + .L._printc_str0]
94		# on x86, al represents the number of SIMD registers used as variadic arguments
95		mov al, 0
96		call printf@plt
97		mov rdi, 0
98		call fflush@plt
99		mov rsp, rbp
100		pop rbp
101		ret
102	
103	.section .rodata
104	# length of .L._println_str0
105		.int 0
106	.L._println_str0:
107		.asciz ""
108	.text
109	_println:
110		push rbp
111		mov rbp, rsp
112		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
113		and rsp, -16
114		lea rdi, [rip + .L._println_str0]
115		call puts@plt
116		mov rdi, 0
117		call fflush@plt
118		mov rsp, rbp
119		pop rbp
120		ret
===========================================================
-- Finished

