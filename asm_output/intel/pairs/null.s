./valid/pairs/null.wacc
calling the reference compiler on ./valid/pairs/null.wacc
-- Test: null.wacc

-- Uploaded file: 
---------------------------------------------------------------
# null pair assignment

# Output:
# (nil)
# (nil)
#

# Program:

begin
  pair(pair, pair) p = null ;
  println p ;
  p = null ;
  println p
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
null.s contents are:
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
20		mov rax, 0
21		mov r12, rax
22		# Stack pointer unchanged, no stack allocated arguments
23		mov rax, r12
24		mov rdi, rax
25		# statement primitives do not return results (but will clobber r0/rax)
26		call _printp
27		call _println
28		# Stack pointer unchanged, no stack allocated variables
29		mov rax, 0
30		# pop {rbx, r12}
31		mov rbx, qword ptr [rsp]
32		mov r12, qword ptr [rsp + 8]
33		add rsp, 16
34		pop rbp
35		ret
36	
37	.section .rodata
38	# length of .L._printp_str0
39		.int 2
40	.L._printp_str0:
41		.asciz "%p"
42	.text
43	_printp:
44		push rbp
45		mov rbp, rsp
46		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
47		and rsp, -16
48		mov rsi, rdi
49		lea rdi, [rip + .L._printp_str0]
50		# on x86, al represents the number of SIMD registers used as variadic arguments
51		mov al, 0
52		call printf@plt
53		mov rdi, 0
54		call fflush@plt
55		mov rsp, rbp
56		pop rbp
57		ret
58	
59	.section .rodata
60	# length of .L._println_str0
61		.int 0
62	.L._println_str0:
63		.asciz ""
64	.text
65	_println:
66		push rbp
67		mov rbp, rsp
68		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
69		and rsp, -16
70		lea rdi, [rip + .L._println_str0]
71		call puts@plt
72		mov rdi, 0
73		call fflush@plt
74		mov rsp, rbp
75		pop rbp
76		ret
===========================================================
-- Finished

