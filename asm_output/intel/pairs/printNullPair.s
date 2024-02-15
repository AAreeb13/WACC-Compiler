./valid/pairs/printNullPair.wacc
calling the reference compiler on ./valid/pairs/printNullPair.wacc
-- Test: printNullPair.wacc

-- Uploaded file: 
---------------------------------------------------------------
# print pair a null pair

# Output:
# (nil)
#

# Program:

begin
  pair(pair, pair) p = null ;
  println p
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
printNullPair.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	.text
4	main:
5		push rbp
6		# push {rbx, r12}
7		sub rsp, 16
8		mov qword ptr [rsp], rbx
9		mov qword ptr [rsp + 8], r12
10		mov rbp, rsp
11		# Stack pointer unchanged, no stack allocated variables
12		mov rax, 0
13		mov r12, rax
14		# Stack pointer unchanged, no stack allocated arguments
15		mov rax, r12
16		mov rdi, rax
17		# statement primitives do not return results (but will clobber r0/rax)
18		call _printp
19		call _println
20		# Stack pointer unchanged, no stack allocated variables
21		mov rax, 0
22		# pop {rbx, r12}
23		mov rbx, qword ptr [rsp]
24		mov r12, qword ptr [rsp + 8]
25		add rsp, 16
26		pop rbp
27		ret
28	
29	.section .rodata
30	# length of .L._printp_str0
31		.int 2
32	.L._printp_str0:
33		.asciz "%p"
34	.text
35	_printp:
36		push rbp
37		mov rbp, rsp
38		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
39		and rsp, -16
40		mov rsi, rdi
41		lea rdi, [rip + .L._printp_str0]
42		# on x86, al represents the number of SIMD registers used as variadic arguments
43		mov al, 0
44		call printf@plt
45		mov rdi, 0
46		call fflush@plt
47		mov rsp, rbp
48		pop rbp
49		ret
50	
51	.section .rodata
52	# length of .L._println_str0
53		.int 0
54	.L._println_str0:
55		.asciz ""
56	.text
57	_println:
58		push rbp
59		mov rbp, rsp
60		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
61		and rsp, -16
62		lea rdi, [rip + .L._println_str0]
63		call puts@plt
64		mov rdi, 0
65		call fflush@plt
66		mov rsp, rbp
67		pop rbp
68		ret
===========================================================
-- Finished

