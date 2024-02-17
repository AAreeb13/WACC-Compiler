./valid/scope/splitScope.wacc
calling the reference compiler on ./valid/scope/splitScope.wacc
-- Test: splitScope.wacc

-- Uploaded file: 
---------------------------------------------------------------
# splits the first appearances of variables by a new scope to ensure proper grouping

# Output:
# 3
# 2
#

# Program:

begin
  int x = 1 ;
  begin
    x = 3 ;
    bool z = true
  end ;
  int y = 2 ;
  println x ;
  println y
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
splitScope.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	.text
4	main:
5		push rbp
6		# push {rbx, r12, r13, r14}
7		sub rsp, 32
8		mov qword ptr [rsp], rbx
9		mov qword ptr [rsp + 8], r12
10		mov qword ptr [rsp + 16], r13
11		mov qword ptr [rsp + 24], r14
12		mov rbp, rsp
13		# Stack pointer unchanged, no stack allocated variables
14		mov rax, 1
15		mov r12, rax
16		# Stack pointer unchanged, no stack allocated variables
17		mov rax, 3
18		mov r12, rax
19		mov rax, 1
20		mov r14, rax
21		# Stack pointer unchanged, no stack allocated variables
22		mov rax, 2
23		mov r13, rax
24		# Stack pointer unchanged, no stack allocated arguments
25		mov rax, r12
26		mov rdi, rax
27		# statement primitives do not return results (but will clobber r0/rax)
28		call _printi
29		call _println
30		# Stack pointer unchanged, no stack allocated arguments
31		mov rax, r13
32		mov rdi, rax
33		# statement primitives do not return results (but will clobber r0/rax)
34		call _printi
35		call _println
36		# Stack pointer unchanged, no stack allocated variables
37		mov rax, 0
38		# pop {rbx, r12, r13, r14}
39		mov rbx, qword ptr [rsp]
40		mov r12, qword ptr [rsp + 8]
41		mov r13, qword ptr [rsp + 16]
42		mov r14, qword ptr [rsp + 24]
43		add rsp, 32
44		pop rbp
45		ret
46	
47	.section .rodata
48	# length of .L._printi_str0
49		.int 2
50	.L._printi_str0:
51		.asciz "%d"
52	.text
53	_printi:
54		push rbp
55		mov rbp, rsp
56		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
57		and rsp, -16
58		mov esi, edi
59		lea rdi, [rip + .L._printi_str0]
60		# on x86, al represents the number of SIMD registers used as variadic arguments
61		mov al, 0
62		call printf@plt
63		mov rdi, 0
64		call fflush@plt
65		mov rsp, rbp
66		pop rbp
67		ret
68	
69	.section .rodata
70	# length of .L._println_str0
71		.int 0
72	.L._println_str0:
73		.asciz ""
74	.text
75	_println:
76		push rbp
77		mov rbp, rsp
78		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
79		and rsp, -16
80		lea rdi, [rip + .L._println_str0]
81		call puts@plt
82		mov rdi, 0
83		call fflush@plt
84		mov rsp, rbp
85		pop rbp
86		ret
===========================================================
-- Finished

