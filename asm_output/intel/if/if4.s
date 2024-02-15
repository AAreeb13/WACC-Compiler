./valid/if/if4.wacc
calling the reference compiler on ./valid/if/if4.wacc
-- Test: if4.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Simple conditional statement with boolen expression test

# Output:
# correct
#

# Program:

begin
  bool b = true;
  bool c = false;
  if b && c
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
if4.s contents are:
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
21		mov rax, 1
22		mov r12, rax
23		mov rax, 0
24		mov r13, rax
25		cmp r12, 1
26		jne .L2
27		cmp r13, 1
28	.L2:
29		je .L0
30		# Stack pointer unchanged, no stack allocated arguments
31		lea rax, [rip + .L.str0]
32		push rax
33		pop rax
34		mov rax, rax
35		mov rdi, rax
36		# statement primitives do not return results (but will clobber r0/rax)
37		call _prints
38		call _println
39		jmp .L1
40	.L0:
41		# Stack pointer unchanged, no stack allocated arguments
42		lea rax, [rip + .L.str1]
43		push rax
44		pop rax
45		mov rax, rax
46		mov rdi, rax
47		# statement primitives do not return results (but will clobber r0/rax)
48		call _prints
49		call _println
50	.L1:
51		# Stack pointer unchanged, no stack allocated variables
52		mov rax, 0
53		# pop {rbx, r12, r13}
54		mov rbx, qword ptr [rsp]
55		mov r12, qword ptr [rsp + 8]
56		mov r13, qword ptr [rsp + 16]
57		add rsp, 24
58		pop rbp
59		ret
60	
61	.section .rodata
62	# length of .L._prints_str0
63		.int 4
64	.L._prints_str0:
65		.asciz "%.*s"
66	.text
67	_prints:
68		push rbp
69		mov rbp, rsp
70		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
71		and rsp, -16
72		mov rdx, rdi
73		mov esi, dword ptr [rdi - 4]
74		lea rdi, [rip + .L._prints_str0]
75		# on x86, al represents the number of SIMD registers used as variadic arguments
76		mov al, 0
77		call printf@plt
78		mov rdi, 0
79		call fflush@plt
80		mov rsp, rbp
81		pop rbp
82		ret
83	
84	.section .rodata
85	# length of .L._println_str0
86		.int 0
87	.L._println_str0:
88		.asciz ""
89	.text
90	_println:
91		push rbp
92		mov rbp, rsp
93		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
94		and rsp, -16
95		lea rdi, [rip + .L._println_str0]
96		call puts@plt
97		mov rdi, 0
98		call fflush@plt
99		mov rsp, rbp
100		pop rbp
101		ret
===========================================================
-- Finished

