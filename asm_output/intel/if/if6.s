./valid/if/if6.wacc
calling the reference compiler on ./valid/if/if6.wacc
-- Test: if6.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Simple conditional statement with character comparison test

# Output:
# correct
#

# Program:

begin
  char c1 = 'f';
  char c2 = 'F';
  if c1 == c2
  then
    println "incorrect"
  else
    println "correct"
  fi
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
if6.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 7
5	.L.str0:
6		.asciz "correct"
7	# length of .L.str1
8		.int 9
9	.L.str1:
10		.asciz "incorrect"
11	.text
12	main:
13		push rbp
14		# push {rbx, r12, r13}
15		sub rsp, 24
16		mov qword ptr [rsp], rbx
17		mov qword ptr [rsp + 8], r12
18		mov qword ptr [rsp + 16], r13
19		mov rbp, rsp
20		# Stack pointer unchanged, no stack allocated variables
21		mov rax, 102
22		mov r12, rax
23		mov rax, 70
24		mov r13, rax
25		cmp r12, r13
26		je .L0
27		# Stack pointer unchanged, no stack allocated arguments
28		lea rax, [rip + .L.str0]
29		push rax
30		pop rax
31		mov rax, rax
32		mov rdi, rax
33		# statement primitives do not return results (but will clobber r0/rax)
34		call _prints
35		call _println
36		jmp .L1
37	.L0:
38		# Stack pointer unchanged, no stack allocated arguments
39		lea rax, [rip + .L.str1]
40		push rax
41		pop rax
42		mov rax, rax
43		mov rdi, rax
44		# statement primitives do not return results (but will clobber r0/rax)
45		call _prints
46		call _println
47	.L1:
48		# Stack pointer unchanged, no stack allocated variables
49		mov rax, 0
50		# pop {rbx, r12, r13}
51		mov rbx, qword ptr [rsp]
52		mov r12, qword ptr [rsp + 8]
53		mov r13, qword ptr [rsp + 16]
54		add rsp, 24
55		pop rbp
56		ret
57	
58	.section .rodata
59	# length of .L._prints_str0
60		.int 4
61	.L._prints_str0:
62		.asciz "%.*s"
63	.text
64	_prints:
65		push rbp
66		mov rbp, rsp
67		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
68		and rsp, -16
69		mov rdx, rdi
70		mov esi, dword ptr [rdi - 4]
71		lea rdi, [rip + .L._prints_str0]
72		# on x86, al represents the number of SIMD registers used as variadic arguments
73		mov al, 0
74		call printf@plt
75		mov rdi, 0
76		call fflush@plt
77		mov rsp, rbp
78		pop rbp
79		ret
80	
81	.section .rodata
82	# length of .L._println_str0
83		.int 0
84	.L._println_str0:
85		.asciz ""
86	.text
87	_println:
88		push rbp
89		mov rbp, rsp
90		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
91		and rsp, -16
92		lea rdi, [rip + .L._println_str0]
93		call puts@plt
94		mov rdi, 0
95		call fflush@plt
96		mov rsp, rbp
97		pop rbp
98		ret
===========================================================
-- Finished

