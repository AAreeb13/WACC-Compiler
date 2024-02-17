./valid/while/whileBoolFlip.wacc
calling the reference compiler on ./valid/while/whileBoolFlip.wacc
-- Test: whileBoolFlip.wacc

-- Uploaded file: 
---------------------------------------------------------------
# while loop flips bool to terminate

# Output:
# flip b!
# end of loop
#

# Program:

begin
  bool b = true ;
  while b do
    println "flip b!" ;
    b = !b
  done ;
  println "end of loop"
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
whileBoolFlip.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 7
5	.L.str0:
6		.asciz "flip b!"
7	# length of .L.str1
8		.int 11
9	.L.str1:
10		.asciz "end of loop"
11	.text
12	main:
13		push rbp
14		# push {rbx, r12}
15		sub rsp, 16
16		mov qword ptr [rsp], rbx
17		mov qword ptr [rsp + 8], r12
18		mov rbp, rsp
19		# Stack pointer unchanged, no stack allocated variables
20		mov rax, 1
21		mov r12, rax
22		jmp .L0
23	.L1:
24		# Stack pointer unchanged, no stack allocated arguments
25		lea rax, [rip + .L.str0]
26		push rax
27		pop rax
28		mov rax, rax
29		mov rdi, rax
30		# statement primitives do not return results (but will clobber r0/rax)
31		call _prints
32		call _println
33		cmp r12, 1
34		setne al
35		movsx rax, al
36		push rax
37		pop rax
38		mov rax, rax
39		mov r12, rax
40	.L0:
41		cmp r12, 1
42		je .L1
43		# Stack pointer unchanged, no stack allocated arguments
44		lea rax, [rip + .L.str1]
45		push rax
46		pop rax
47		mov rax, rax
48		mov rdi, rax
49		# statement primitives do not return results (but will clobber r0/rax)
50		call _prints
51		call _println
52		# Stack pointer unchanged, no stack allocated variables
53		mov rax, 0
54		# pop {rbx, r12}
55		mov rbx, qword ptr [rsp]
56		mov r12, qword ptr [rsp + 8]
57		add rsp, 16
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

