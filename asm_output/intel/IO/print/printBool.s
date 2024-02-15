./valid/IO/print/printBool.wacc
calling the reference compiler on ./valid/IO/print/printBool.wacc
-- Test: printBool.wacc

-- Uploaded file: 
---------------------------------------------------------------
# basic Boolean printing

# Output:
# True is true
# False is false
#

# Program:

begin
  print "True is " ;
  println true ;

  print "False is " ;
  println false
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
printBool.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 8
5	.L.str0:
6		.asciz "True is "
7	# length of .L.str1
8		.int 9
9	.L.str1:
10		.asciz "False is "
11	.text
12	main:
13		push rbp
14		push rbx
15		mov rbp, rsp
16		# Stack pointer unchanged, no stack allocated arguments
17		lea rax, [rip + .L.str0]
18		push rax
19		pop rax
20		mov rax, rax
21		mov rdi, rax
22		# statement primitives do not return results (but will clobber r0/rax)
23		call _prints
24		# Stack pointer unchanged, no stack allocated arguments
25		mov rax, 1
26		mov rdi, rax
27		# statement primitives do not return results (but will clobber r0/rax)
28		call _printb
29		call _println
30		# Stack pointer unchanged, no stack allocated arguments
31		lea rax, [rip + .L.str1]
32		push rax
33		pop rax
34		mov rax, rax
35		mov rdi, rax
36		# statement primitives do not return results (but will clobber r0/rax)
37		call _prints
38		# Stack pointer unchanged, no stack allocated arguments
39		mov rax, 0
40		mov rdi, rax
41		# statement primitives do not return results (but will clobber r0/rax)
42		call _printb
43		call _println
44		mov rax, 0
45		pop rbx
46		pop rbp
47		ret
48	
49	.section .rodata
50	# length of .L._prints_str0
51		.int 4
52	.L._prints_str0:
53		.asciz "%.*s"
54	.text
55	_prints:
56		push rbp
57		mov rbp, rsp
58		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
59		and rsp, -16
60		mov rdx, rdi
61		mov esi, dword ptr [rdi - 4]
62		lea rdi, [rip + .L._prints_str0]
63		# on x86, al represents the number of SIMD registers used as variadic arguments
64		mov al, 0
65		call printf@plt
66		mov rdi, 0
67		call fflush@plt
68		mov rsp, rbp
69		pop rbp
70		ret
71	
72	.section .rodata
73	# length of .L._printb_str0
74		.int 5
75	.L._printb_str0:
76		.asciz "false"
77	# length of .L._printb_str1
78		.int 4
79	.L._printb_str1:
80		.asciz "true"
81	# length of .L._printb_str2
82		.int 4
83	.L._printb_str2:
84		.asciz "%.*s"
85	.text
86	_printb:
87		push rbp
88		mov rbp, rsp
89		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
90		and rsp, -16
91		cmp dil, 0
92		jne .L_printb0
93		lea rdx, [rip + .L._printb_str0]
94		jmp .L_printb1
95	.L_printb0:
96		lea rdx, [rip + .L._printb_str1]
97	.L_printb1:
98		mov esi, dword ptr [rdx - 4]
99		lea rdi, [rip + .L._printb_str2]
100		# on x86, al represents the number of SIMD registers used as variadic arguments
101		mov al, 0
102		call printf@plt
103		mov rdi, 0
104		call fflush@plt
105		mov rsp, rbp
106		pop rbp
107		ret
108	
109	.section .rodata
110	# length of .L._println_str0
111		.int 0
112	.L._println_str0:
113		.asciz ""
114	.text
115	_println:
116		push rbp
117		mov rbp, rsp
118		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
119		and rsp, -16
120		lea rdi, [rip + .L._println_str0]
121		call puts@plt
122		mov rdi, 0
123		call fflush@plt
124		mov rsp, rbp
125		pop rbp
126		ret
===========================================================
-- Finished

