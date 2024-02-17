./valid/IO/print/hashInProgram.wacc
calling the reference compiler on ./valid/IO/print/hashInProgram.wacc
-- Test: hashInProgram.wacc

-- Uploaded file: 
---------------------------------------------------------------
# In-line comments and printing #

# Output:
# We can print the hash character: #
# We can also print # when its in a string.
#

# Program:

begin
  int x = 0 ; # comments can be in-line
  print "We can print the hash character: " ;
  println '#' ;
  println "We can also print # when its in a string."
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
hashInProgram.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 33
5	.L.str0:
6		.asciz "We can print the hash character: "
7	# length of .L.str1
8		.int 41
9	.L.str1:
10		.asciz "We can also print # when its in a string."
11	.text
12	main:
13		push rbp
14		# push {rbx, r12}
15		sub rsp, 16
16		mov qword ptr [rsp], rbx
17		mov qword ptr [rsp + 8], r12
18		mov rbp, rsp
19		# Stack pointer unchanged, no stack allocated variables
20		mov rax, 0
21		mov r12, rax
22		# Stack pointer unchanged, no stack allocated arguments
23		lea rax, [rip + .L.str0]
24		push rax
25		pop rax
26		mov rax, rax
27		mov rdi, rax
28		# statement primitives do not return results (but will clobber r0/rax)
29		call _prints
30		# Stack pointer unchanged, no stack allocated arguments
31		mov rax, 35
32		mov rdi, rax
33		# statement primitives do not return results (but will clobber r0/rax)
34		call _printc
35		call _println
36		# Stack pointer unchanged, no stack allocated arguments
37		lea rax, [rip + .L.str1]
38		push rax
39		pop rax
40		mov rax, rax
41		mov rdi, rax
42		# statement primitives do not return results (but will clobber r0/rax)
43		call _prints
44		call _println
45		# Stack pointer unchanged, no stack allocated variables
46		mov rax, 0
47		# pop {rbx, r12}
48		mov rbx, qword ptr [rsp]
49		mov r12, qword ptr [rsp + 8]
50		add rsp, 16
51		pop rbp
52		ret
53	
54	.section .rodata
55	# length of .L._prints_str0
56		.int 4
57	.L._prints_str0:
58		.asciz "%.*s"
59	.text
60	_prints:
61		push rbp
62		mov rbp, rsp
63		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
64		and rsp, -16
65		mov rdx, rdi
66		mov esi, dword ptr [rdi - 4]
67		lea rdi, [rip + .L._prints_str0]
68		# on x86, al represents the number of SIMD registers used as variadic arguments
69		mov al, 0
70		call printf@plt
71		mov rdi, 0
72		call fflush@plt
73		mov rsp, rbp
74		pop rbp
75		ret
76	
77	.section .rodata
78	# length of .L._printc_str0
79		.int 2
80	.L._printc_str0:
81		.asciz "%c"
82	.text
83	_printc:
84		push rbp
85		mov rbp, rsp
86		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
87		and rsp, -16
88		mov sil, dil
89		lea rdi, [rip + .L._printc_str0]
90		# on x86, al represents the number of SIMD registers used as variadic arguments
91		mov al, 0
92		call printf@plt
93		mov rdi, 0
94		call fflush@plt
95		mov rsp, rbp
96		pop rbp
97		ret
98	
99	.section .rodata
100	# length of .L._println_str0
101		.int 0
102	.L._println_str0:
103		.asciz ""
104	.text
105	_println:
106		push rbp
107		mov rbp, rsp
108		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
109		and rsp, -16
110		lea rdi, [rip + .L._println_str0]
111		call puts@plt
112		mov rdi, 0
113		call fflush@plt
114		mov rsp, rbp
115		pop rbp
116		ret
===========================================================
-- Finished

