./valid/if/if1.wacc
calling the reference compiler on ./valid/if/if1.wacc
-- Test: if1.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Simple conditional statement with int comparison test

# Output:
# correct
#

# Program:

begin
  int a = 13;
  if a == 13
  then
    println "correct"
  else
    println "incorrect"
  fi
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
if1.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 9
5	.L.str0:
6		.asciz "incorrect"
7	# length of .L.str1
8		.int 7
9	.L.str1:
10		.asciz "correct"
11	.text
12	main:
13		push rbp
14		# push {rbx, r12}
15		sub rsp, 16
16		mov qword ptr [rsp], rbx
17		mov qword ptr [rsp + 8], r12
18		mov rbp, rsp
19		# Stack pointer unchanged, no stack allocated variables
20		mov rax, 13
21		mov r12, rax
22		cmp r12, 13
23		je .L0
24		# Stack pointer unchanged, no stack allocated arguments
25		lea rax, [rip + .L.str0]
26		push rax
27		pop rax
28		mov rax, rax
29		mov rdi, rax
30		# statement primitives do not return results (but will clobber r0/rax)
31		call _prints
32		call _println
33		jmp .L1
34	.L0:
35		# Stack pointer unchanged, no stack allocated arguments
36		lea rax, [rip + .L.str1]
37		push rax
38		pop rax
39		mov rax, rax
40		mov rdi, rax
41		# statement primitives do not return results (but will clobber r0/rax)
42		call _prints
43		call _println
44	.L1:
45		# Stack pointer unchanged, no stack allocated variables
46		mov rax, 0
47		# pop {rbx, r12}
48		mov rbx, qword ptr [rsp]
49		mov r12, qword ptr [rsp + 8]
50		add rsp, 16
51		pop rbp
52		ret
53	
54	.section .rodata
55	# length of .L._prints_str0
56		.int 4
57	.L._prints_str0:
58		.asciz "%.*s"
59	.text
60	_prints:
61		push rbp
62		mov rbp, rsp
63		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
64		and rsp, -16
65		mov rdx, rdi
66		mov esi, dword ptr [rdi - 4]
67		lea rdi, [rip + .L._prints_str0]
68		# on x86, al represents the number of SIMD registers used as variadic arguments
69		mov al, 0
70		call printf@plt
71		mov rdi, 0
72		call fflush@plt
73		mov rsp, rbp
74		pop rbp
75		ret
76	
77	.section .rodata
78	# length of .L._println_str0
79		.int 0
80	.L._println_str0:
81		.asciz ""
82	.text
83	_println:
84		push rbp
85		mov rbp, rsp
86		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
87		and rsp, -16
88		lea rdi, [rip + .L._println_str0]
89		call puts@plt
90		mov rdi, 0
91		call fflush@plt
92		mov rsp, rbp
93		pop rbp
94		ret
===========================================================
-- Finished

