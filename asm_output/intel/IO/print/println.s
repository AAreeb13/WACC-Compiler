./valid/IO/print/println.wacc
calling the reference compiler on ./valid/IO/print/println.wacc
-- Test: println.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple println statement

# Output:
# Hello World!
#

# Program:

begin
  println "Hello World!"
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
println.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 12
5	.L.str0:
6		.asciz "Hello World!"
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
20		call _println
21		mov rax, 0
22		pop rbx
23		pop rbp
24		ret
25	
26	.section .rodata
27	# length of .L._prints_str0
28		.int 4
29	.L._prints_str0:
30		.asciz "%.*s"
31	.text
32	_prints:
33		push rbp
34		mov rbp, rsp
35		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
36		and rsp, -16
37		mov rdx, rdi
38		mov esi, dword ptr [rdi - 4]
39		lea rdi, [rip + .L._prints_str0]
40		# on x86, al represents the number of SIMD registers used as variadic arguments
41		mov al, 0
42		call printf@plt
43		mov rdi, 0
44		call fflush@plt
45		mov rsp, rbp
46		pop rbp
47		ret
48	
49	.section .rodata
50	# length of .L._println_str0
51		.int 0
52	.L._println_str0:
53		.asciz ""
54	.text
55	_println:
56		push rbp
57		mov rbp, rsp
58		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
59		and rsp, -16
60		lea rdi, [rip + .L._println_str0]
61		call puts@plt
62		mov rdi, 0
63		call fflush@plt
64		mov rsp, rbp
65		pop rbp
66		ret
===========================================================
-- Finished

