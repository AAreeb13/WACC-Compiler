./valid/function/nested_functions/simpleRecursion.wacc
calling the reference compiler on ./valid/function/nested_functions/simpleRecursion.wacc
-- Test: simpleRecursion.wacc

-- Uploaded file: 
---------------------------------------------------------------
# a simple recursive function

# Output:

# Program:

begin
  int rec(int x) is
    if x == 0
    then
      skip
    else
      int y = call rec(x - 1)
    fi ;
    return 42
  end

  int x = 0 ;
  x = call rec(8)
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
simpleRecursion.s contents are:
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
15		mov rax, 8
16		mov rdi, rax
17		call wacc_rec
18		mov r11, rax
19		# Stack pointer unchanged, no stack allocated arguments
20		mov rax, r11
21		mov r12, rax
22		# Stack pointer unchanged, no stack allocated variables
23		mov rax, 0
24		# pop {rbx, r12}
25		mov rbx, qword ptr [rsp]
26		mov r12, qword ptr [rsp + 8]
27		add rsp, 16
28		pop rbp
29		ret
30	
31	wacc_rec:
32		push rbp
33		push r12
34		mov rbp, rsp
35		cmp rdi, 0
36		je .L0
37		# Stack pointer unchanged, no stack allocated variables
38		push rdi
39		# Set up R11 as a temporary second base pointer for the caller saved things
40		mov r11, rsp
41		# Stack pointer unchanged, no stack allocated arguments
42		mov eax, edi
43		sub eax, 1
44		jo _errOverflow
45		movsx rax, eax
46		push rax
47		pop rax
48		mov rax, rax
49		mov rdi, rax
50		call wacc_rec
51		mov r11, rax
52		# Stack pointer unchanged, no stack allocated arguments
53		pop rdi
54		mov rax, r11
55		mov r12, rax
56		# Stack pointer unchanged, no stack allocated variables
57		jmp .L1
58	.L0:
59	.L1:
60		mov rax, 42
61		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
62		mov rsp, rbp
63		pop r12
64		pop rbp
65		ret
66		# 'ere be dragons: this is 100% dead code, functions always end in returns!
67	
68	.section .rodata
69	# length of .L._prints_str0
70		.int 4
71	.L._prints_str0:
72		.asciz "%.*s"
73	.text
74	_prints:
75		push rbp
76		mov rbp, rsp
77		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
78		and rsp, -16
79		mov rdx, rdi
80		mov esi, dword ptr [rdi - 4]
81		lea rdi, [rip + .L._prints_str0]
82		# on x86, al represents the number of SIMD registers used as variadic arguments
83		mov al, 0
84		call printf@plt
85		mov rdi, 0
86		call fflush@plt
87		mov rsp, rbp
88		pop rbp
89		ret
90	
91	_exit:
92		push rbp
93		mov rbp, rsp
94		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
95		and rsp, -16
96		call exit@plt
97		mov rsp, rbp
98		pop rbp
99		ret
100	
101	.section .rodata
102	# length of .L._errOverflow_str0
103		.int 52
104	.L._errOverflow_str0:
105		.asciz "fatal error: integer overflow or underflow occurred\n"
106	.text
107	_errOverflow:
108		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
109		and rsp, -16
110		lea rdi, [rip + .L._errOverflow_str0]
111		call _prints
112		mov dil, -1
113		call exit@plt
===========================================================
-- Finished

