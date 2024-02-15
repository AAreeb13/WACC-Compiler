./valid/pairs/printNull.wacc
calling the reference compiler on ./valid/pairs/printNull.wacc
-- Test: printNull.wacc

-- Uploaded file: 
---------------------------------------------------------------
# print the null reference

# Output:
# (nil)
#

# Program:

begin
  println null
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
printNull.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	.text
4	main:
5		push rbp
6		push rbx
7		mov rbp, rsp
8		# Stack pointer unchanged, no stack allocated arguments
9		mov rax, 0
10		mov rdi, rax
11		# statement primitives do not return results (but will clobber r0/rax)
12		call _printp
13		call _println
14		mov rax, 0
15		pop rbx
16		pop rbp
17		ret
18	
19	.section .rodata
20	# length of .L._printp_str0
21		.int 2
22	.L._printp_str0:
23		.asciz "%p"
24	.text
25	_printp:
26		push rbp
27		mov rbp, rsp
28		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
29		and rsp, -16
30		mov rsi, rdi
31		lea rdi, [rip + .L._printp_str0]
32		# on x86, al represents the number of SIMD registers used as variadic arguments
33		mov al, 0
34		call printf@plt
35		mov rdi, 0
36		call fflush@plt
37		mov rsp, rbp
38		pop rbp
39		ret
40	
41	.section .rodata
42	# length of .L._println_str0
43		.int 0
44	.L._println_str0:
45		.asciz ""
46	.text
47	_println:
48		push rbp
49		mov rbp, rsp
50		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
51		and rsp, -16
52		lea rdi, [rip + .L._println_str0]
53		call puts@plt
54		mov rdi, 0
55		call fflush@plt
56		mov rsp, rbp
57		pop rbp
58		ret
===========================================================
-- Finished

