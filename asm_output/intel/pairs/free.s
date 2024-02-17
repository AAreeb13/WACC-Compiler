./valid/pairs/free.wacc
calling the reference compiler on ./valid/pairs/free.wacc
-- Test: free.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Create and free a simple pair

# Output:

# Program:

begin
  pair(int, char) a = newpair(10, 'a') ;
  free a
end

---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
free.s contents are:
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
12		mov edi, 16
13		call _malloc
14		mov r11, rax
15		mov rax, 10
16		mov qword ptr [r11], rax
17		mov rax, 97
18		mov qword ptr [r11 + 8], rax
19		mov rax, r11
20		mov r12, rax
21		# Stack pointer unchanged, no stack allocated arguments
22		mov rax, r12
23		mov rdi, rax
24		# statement primitives do not return results (but will clobber r0/rax)
25		call _freepair
26		# Stack pointer unchanged, no stack allocated variables
27		mov rax, 0
28		# pop {rbx, r12}
29		mov rbx, qword ptr [rsp]
30		mov r12, qword ptr [rsp + 8]
31		add rsp, 16
32		pop rbp
33		ret
34	
35	_freepair:
36		push rbp
37		mov rbp, rsp
38		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
39		and rsp, -16
40		cmp rdi, 0
41		je _errNull
42		call free@plt
43		mov rsp, rbp
44		pop rbp
45		ret
46	
47	.section .rodata
48	# length of .L._prints_str0
49		.int 4
50	.L._prints_str0:
51		.asciz "%.*s"
52	.text
53	_prints:
54		push rbp
55		mov rbp, rsp
56		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
57		and rsp, -16
58		mov rdx, rdi
59		mov esi, dword ptr [rdi - 4]
60		lea rdi, [rip + .L._prints_str0]
61		# on x86, al represents the number of SIMD registers used as variadic arguments
62		mov al, 0
63		call printf@plt
64		mov rdi, 0
65		call fflush@plt
66		mov rsp, rbp
67		pop rbp
68		ret
69	
70	_malloc:
71		push rbp
72		mov rbp, rsp
73		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
74		and rsp, -16
75		call malloc@plt
76		cmp rax, 0
77		je _errOutOfMemory
78		mov rsp, rbp
79		pop rbp
80		ret
81	
82	_exit:
83		push rbp
84		mov rbp, rsp
85		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
86		and rsp, -16
87		call exit@plt
88		mov rsp, rbp
89		pop rbp
90		ret
91	
92	.section .rodata
93	# length of .L._errOutOfMemory_str0
94		.int 27
95	.L._errOutOfMemory_str0:
96		.asciz "fatal error: out of memory\n"
97	.text
98	_errOutOfMemory:
99		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
100		and rsp, -16
101		lea rdi, [rip + .L._errOutOfMemory_str0]
102		call _prints
103		mov dil, -1
104		call exit@plt
105	
106	.section .rodata
107	# length of .L._errNull_str0
108		.int 45
109	.L._errNull_str0:
110		.asciz "fatal error: null pair dereferenced or freed\n"
111	.text
112	_errNull:
113		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
114		and rsp, -16
115		lea rdi, [rip + .L._errNull_str0]
116		call _prints
117		mov dil, -1
118		call exit@plt
===========================================================
-- Finished

