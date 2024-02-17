./valid/runtimeErr/nullDereference/freeNull.wacc
calling the reference compiler on ./valid/runtimeErr/nullDereference/freeNull.wacc
-- Test: freeNull.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Create and free a null pair

# Output:
# #runtime_error#

# Exit:
# 255

# Program:

begin
  pair(pair, pair) a = null ;
  free a
end

---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
freeNull.s contents are:
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
18		call _freepair
19		# Stack pointer unchanged, no stack allocated variables
20		mov rax, 0
21		# pop {rbx, r12}
22		mov rbx, qword ptr [rsp]
23		mov r12, qword ptr [rsp + 8]
24		add rsp, 16
25		pop rbp
26		ret
27	
28	_freepair:
29		push rbp
30		mov rbp, rsp
31		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
32		and rsp, -16
33		cmp rdi, 0
34		je _errNull
35		call free@plt
36		mov rsp, rbp
37		pop rbp
38		ret
39	
40	.section .rodata
41	# length of .L._prints_str0
42		.int 4
43	.L._prints_str0:
44		.asciz "%.*s"
45	.text
46	_prints:
47		push rbp
48		mov rbp, rsp
49		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
50		and rsp, -16
51		mov rdx, rdi
52		mov esi, dword ptr [rdi - 4]
53		lea rdi, [rip + .L._prints_str0]
54		# on x86, al represents the number of SIMD registers used as variadic arguments
55		mov al, 0
56		call printf@plt
57		mov rdi, 0
58		call fflush@plt
59		mov rsp, rbp
60		pop rbp
61		ret
62	
63	_exit:
64		push rbp
65		mov rbp, rsp
66		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
67		and rsp, -16
68		call exit@plt
69		mov rsp, rbp
70		pop rbp
71		ret
72	
73	.section .rodata
74	# length of .L._errNull_str0
75		.int 45
76	.L._errNull_str0:
77		.asciz "fatal error: null pair dereferenced or freed\n"
78	.text
79	_errNull:
80		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
81		and rsp, -16
82		lea rdi, [rip + .L._errNull_str0]
83		call _prints
84		mov dil, -1
85		call exit@plt
===========================================================
-- Finished

