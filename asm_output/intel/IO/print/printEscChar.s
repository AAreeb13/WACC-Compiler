./valid/IO/print/printEscChar.wacc
calling the reference compiler on ./valid/IO/print/printEscChar.wacc
-- Test: printEscChar.wacc

-- Uploaded file: 
---------------------------------------------------------------
# basic escaped character printing

# Output:
# An escaped character example is "
#

# Program:

begin
  print "An escaped character example is " ;
  println '\"'
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
printEscChar.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 32
5	.L.str0:
6		.asciz "An escaped character example is "
7	.text
8	main:
9		push rbp
10		push rbx
11		mov rbp, rsp
12		# Stack pointer unchanged, no stack allocated arguments
13		lea rax, [rip + .L.str0]
14		push rax
15		pop rax
16		mov rax, rax
17		mov rdi, rax
18		# statement primitives do not return results (but will clobber r0/rax)
19		call _prints
20		# Stack pointer unchanged, no stack allocated arguments
21		mov rax, 34
22		mov rdi, rax
23		# statement primitives do not return results (but will clobber r0/rax)
24		call _printc
25		call _println
26		mov rax, 0
27		pop rbx
28		pop rbp
29		ret
30	
31	.section .rodata
32	# length of .L._prints_str0
33		.int 4
34	.L._prints_str0:
35		.asciz "%.*s"
36	.text
37	_prints:
38		push rbp
39		mov rbp, rsp
40		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
41		and rsp, -16
42		mov rdx, rdi
43		mov esi, dword ptr [rdi - 4]
44		lea rdi, [rip + .L._prints_str0]
45		# on x86, al represents the number of SIMD registers used as variadic arguments
46		mov al, 0
47		call printf@plt
48		mov rdi, 0
49		call fflush@plt
50		mov rsp, rbp
51		pop rbp
52		ret
53	
54	.section .rodata
55	# length of .L._printc_str0
56		.int 2
57	.L._printc_str0:
58		.asciz "%c"
59	.text
60	_printc:
61		push rbp
62		mov rbp, rsp
63		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
64		and rsp, -16
65		mov sil, dil
66		lea rdi, [rip + .L._printc_str0]
67		# on x86, al represents the number of SIMD registers used as variadic arguments
68		mov al, 0
69		call printf@plt
70		mov rdi, 0
71		call fflush@plt
72		mov rsp, rbp
73		pop rbp
74		ret
75	
76	.section .rodata
77	# length of .L._println_str0
78		.int 0
79	.L._println_str0:
80		.asciz ""
81	.text
82	_println:
83		push rbp
84		mov rbp, rsp
85		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
86		and rsp, -16
87		lea rdi, [rip + .L._println_str0]
88		call puts@plt
89		mov rdi, 0
90		call fflush@plt
91		mov rsp, rbp
92		pop rbp
93		ret
===========================================================
-- Finished

