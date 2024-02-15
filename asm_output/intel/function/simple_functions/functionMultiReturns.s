./valid/function/simple_functions/functionMultiReturns.wacc
calling the reference compiler on ./valid/function/simple_functions/functionMultiReturns.wacc
-- Test: functionMultiReturns.wacc

-- Uploaded file: 
---------------------------------------------------------------
# a simple function with multiple returns, importantly one at the end.

# Output:
# 1
#

# Program:

begin
  int returnInWhile() is
    while true do
      return 1 ;
      println "How on Earth did we get here?"
    done ;
    return 2
  end

  int x = call returnInWhile() ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
functionMultiReturns.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 29
5	.L.str0:
6		.asciz "How on Earth did we get here?"
7	.text
8	main:
9		push rbp
10		# push {rbx, r12}
11		sub rsp, 16
12		mov qword ptr [rsp], rbx
13		mov qword ptr [rsp + 8], r12
14		mov rbp, rsp
15		# Stack pointer unchanged, no stack allocated variables
16		# Stack pointer unchanged, no stack allocated arguments
17		call wacc_returnInWhile
18		mov r11, rax
19		# Stack pointer unchanged, no stack allocated arguments
20		mov rax, r11
21		mov r12, rax
22		# Stack pointer unchanged, no stack allocated arguments
23		mov rax, r12
24		mov rdi, rax
25		# statement primitives do not return results (but will clobber r0/rax)
26		call _printi
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
37	wacc_returnInWhile:
38		push rbp
39		mov rbp, rsp
40		jmp .L0
41	.L1:
42		mov rax, 1
43		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
44		mov rsp, rbp
45		pop rbp
46		ret
47		# Stack pointer unchanged, no stack allocated arguments
48		lea rax, [rip + .L.str0]
49		push rax
50		pop rax
51		mov rax, rax
52		mov rdi, rax
53		# statement primitives do not return results (but will clobber r0/rax)
54		call _prints
55		call _println
56	.L0:
57		mov rax, 1
58		cmp rax, 1
59		je .L1
60		mov rax, 2
61		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
62		mov rsp, rbp
63		pop rbp
64		ret
65		# 'ere be dragons: this is 100% dead code, functions always end in returns!
66	
67	.section .rodata
68	# length of .L._prints_str0
69		.int 4
70	.L._prints_str0:
71		.asciz "%.*s"
72	.text
73	_prints:
74		push rbp
75		mov rbp, rsp
76		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
77		and rsp, -16
78		mov rdx, rdi
79		mov esi, dword ptr [rdi - 4]
80		lea rdi, [rip + .L._prints_str0]
81		# on x86, al represents the number of SIMD registers used as variadic arguments
82		mov al, 0
83		call printf@plt
84		mov rdi, 0
85		call fflush@plt
86		mov rsp, rbp
87		pop rbp
88		ret
89	
90	.section .rodata
91	# length of .L._printi_str0
92		.int 2
93	.L._printi_str0:
94		.asciz "%d"
95	.text
96	_printi:
97		push rbp
98		mov rbp, rsp
99		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
100		and rsp, -16
101		mov esi, edi
102		lea rdi, [rip + .L._printi_str0]
103		# on x86, al represents the number of SIMD registers used as variadic arguments
104		mov al, 0
105		call printf@plt
106		mov rdi, 0
107		call fflush@plt
108		mov rsp, rbp
109		pop rbp
110		ret
111	
112	.section .rodata
113	# length of .L._println_str0
114		.int 0
115	.L._println_str0:
116		.asciz ""
117	.text
118	_println:
119		push rbp
120		mov rbp, rsp
121		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
122		and rsp, -16
123		lea rdi, [rip + .L._println_str0]
124		call puts@plt
125		mov rdi, 0
126		call fflush@plt
127		mov rsp, rbp
128		pop rbp
129		ret
===========================================================
-- Finished

