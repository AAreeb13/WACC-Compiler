./valid/IO/print/printCharArray.wacc
calling the reference compiler on ./valid/IO/print/printCharArray.wacc
-- Test: printCharArray.wacc

-- Uploaded file: 
---------------------------------------------------------------
# printing the contents of a char[] is possible via an intermediate variable

# Output:
# hi!
#

# Exit:
# 0

# Program:

begin
  char[] s = ['h','i','!'];
  println s
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
printCharArray.s contents are:
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
12		# 3 element array
13		mov edi, 7
14		call _malloc
15		mov r11, rax
16		# array pointers are shifted forwards by 4 bytes (to account for size)
17		mov r11, r11
18		add r11, 4
19		mov rax, 3
20		mov dword ptr [r11 - 4], eax
21		mov rax, 104
22		mov byte ptr [r11], al
23		mov rax, 105
24		mov byte ptr [r11 + 1], al
25		mov rax, 33
26		mov byte ptr [r11 + 2], al
27		mov rax, r11
28		mov r12, rax
29		# Stack pointer unchanged, no stack allocated arguments
30		mov rax, r12
31		mov rdi, rax
32		# statement primitives do not return results (but will clobber r0/rax)
33		call _prints
34		call _println
35		# Stack pointer unchanged, no stack allocated variables
36		mov rax, 0
37		# pop {rbx, r12}
38		mov rbx, qword ptr [rsp]
39		mov r12, qword ptr [rsp + 8]
40		add rsp, 16
41		pop rbp
42		ret
43	
44	.section .rodata
45	# length of .L._prints_str0
46		.int 4
47	.L._prints_str0:
48		.asciz "%.*s"
49	.text
50	_prints:
51		push rbp
52		mov rbp, rsp
53		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
54		and rsp, -16
55		mov rdx, rdi
56		mov esi, dword ptr [rdi - 4]
57		lea rdi, [rip + .L._prints_str0]
58		# on x86, al represents the number of SIMD registers used as variadic arguments
59		mov al, 0
60		call printf@plt
61		mov rdi, 0
62		call fflush@plt
63		mov rsp, rbp
64		pop rbp
65		ret
66	
67	_malloc:
68		push rbp
69		mov rbp, rsp
70		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
71		and rsp, -16
72		call malloc@plt
73		cmp rax, 0
74		je _errOutOfMemory
75		mov rsp, rbp
76		pop rbp
77		ret
78	
79	.section .rodata
80	# length of .L._println_str0
81		.int 0
82	.L._println_str0:
83		.asciz ""
84	.text
85	_println:
86		push rbp
87		mov rbp, rsp
88		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
89		and rsp, -16
90		lea rdi, [rip + .L._println_str0]
91		call puts@plt
92		mov rdi, 0
93		call fflush@plt
94		mov rsp, rbp
95		pop rbp
96		ret
97	
98	_exit:
99		push rbp
100		mov rbp, rsp
101		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
102		and rsp, -16
103		call exit@plt
104		mov rsp, rbp
105		pop rbp
106		ret
107	
108	.section .rodata
109	# length of .L._errOutOfMemory_str0
110		.int 27
111	.L._errOutOfMemory_str0:
112		.asciz "fatal error: out of memory\n"
113	.text
114	_errOutOfMemory:
115		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
116		and rsp, -16
117		lea rdi, [rip + .L._errOutOfMemory_str0]
118		call _prints
119		mov dil, -1
120		call exit@plt
===========================================================
-- Finished

