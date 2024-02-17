./valid/scope/ifNested1.wacc
calling the reference compiler on ./valid/scope/ifNested1.wacc
-- Test: ifNested1.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Nested conditional statement

# Output:
# correct
#

# Program:

begin
  int a = 13;
  if a == 13
  then
    if a > 5
    then
      println "correct"
    else
      println "incorrect"
    fi
  else
    println "incorrect"
  fi
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
ifNested1.s contents are:
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
35		cmp r12, 5
36		jg .L2
37		# Stack pointer unchanged, no stack allocated arguments
38		lea rax, [rip + .L.str0]
39		push rax
40		pop rax
41		mov rax, rax
42		mov rdi, rax
43		# statement primitives do not return results (but will clobber r0/rax)
44		call _prints
45		call _println
46		jmp .L3
47	.L2:
48		# Stack pointer unchanged, no stack allocated arguments
49		lea rax, [rip + .L.str1]
50		push rax
51		pop rax
52		mov rax, rax
53		mov rdi, rax
54		# statement primitives do not return results (but will clobber r0/rax)
55		call _prints
56		call _println
57	.L3:
58	.L1:
59		# Stack pointer unchanged, no stack allocated variables
60		mov rax, 0
61		# pop {rbx, r12}
62		mov rbx, qword ptr [rsp]
63		mov r12, qword ptr [rsp + 8]
64		add rsp, 16
65		pop rbp
66		ret
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
91	.section .rodata
92	# length of .L._println_str0
93		.int 0
94	.L._println_str0:
95		.asciz ""
96	.text
97	_println:
98		push rbp
99		mov rbp, rsp
100		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
101		and rsp, -16
102		lea rdi, [rip + .L._println_str0]
103		call puts@plt
104		mov rdi, 0
105		call fflush@plt
106		mov rsp, rbp
107		pop rbp
108		ret
===========================================================
-- Finished

