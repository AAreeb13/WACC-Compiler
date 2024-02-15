./valid/if/whitespace.wacc
calling the reference compiler on ./valid/if/whitespace.wacc
-- Test: whitespace.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Whitespace only important between keyword and variable tokens

# Output:
# 1
#

# Program:

begin
	int a=13;
  if a==13then a=1else a=0fi;
  println a
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
whitespace.s contents are:
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
12		mov rax, 13
13		mov r12, rax
14		cmp r12, 13
15		je .L0
16		mov rax, 0
17		mov r12, rax
18		jmp .L1
19	.L0:
20		mov rax, 1
21		mov r12, rax
22	.L1:
23		# Stack pointer unchanged, no stack allocated arguments
24		mov rax, r12
25		mov rdi, rax
26		# statement primitives do not return results (but will clobber r0/rax)
27		call _printi
28		call _println
29		# Stack pointer unchanged, no stack allocated variables
30		mov rax, 0
31		# pop {rbx, r12}
32		mov rbx, qword ptr [rsp]
33		mov r12, qword ptr [rsp + 8]
34		add rsp, 16
35		pop rbp
36		ret
37	
38	.section .rodata
39	# length of .L._printi_str0
40		.int 2
41	.L._printi_str0:
42		.asciz "%d"
43	.text
44	_printi:
45		push rbp
46		mov rbp, rsp
47		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
48		and rsp, -16
49		mov esi, edi
50		lea rdi, [rip + .L._printi_str0]
51		# on x86, al represents the number of SIMD registers used as variadic arguments
52		mov al, 0
53		call printf@plt
54		mov rdi, 0
55		call fflush@plt
56		mov rsp, rbp
57		pop rbp
58		ret
59	
60	.section .rodata
61	# length of .L._println_str0
62		.int 0
63	.L._println_str0:
64		.asciz ""
65	.text
66	_println:
67		push rbp
68		mov rbp, rsp
69		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
70		and rsp, -16
71		lea rdi, [rip + .L._println_str0]
72		call puts@plt
73		mov rdi, 0
74		call fflush@plt
75		mov rsp, rbp
76		pop rbp
77		ret
===========================================================
-- Finished

