./valid/IO/print/print.wacc
calling the reference compiler on ./valid/IO/print/print.wacc
-- Test: print.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple print statement

# Output:
# Hello World!

# Program:

begin
  print "Hello World!"
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
print.s contents are:
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
20		mov rax, 0
21		pop rbx
22		pop rbp
23		ret
24	
25	.section .rodata
26	# length of .L._prints_str0
27		.int 4
28	.L._prints_str0:
29		.asciz "%.*s"
30	.text
31	_prints:
32		push rbp
33		mov rbp, rsp
34		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
35		and rsp, -16
36		mov rdx, rdi
37		mov esi, dword ptr [rdi - 4]
38		lea rdi, [rip + .L._prints_str0]
39		# on x86, al represents the number of SIMD registers used as variadic arguments
40		mov al, 0
41		call printf@plt
42		mov rdi, 0
43		call fflush@plt
44		mov rsp, rbp
45		pop rbp
46		ret
===========================================================
-- Finished

