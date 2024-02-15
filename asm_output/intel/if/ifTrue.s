./valid/if/ifTrue.wacc
calling the reference compiler on ./valid/if/ifTrue.wacc
-- Test: ifTrue.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple true if statement

# Output:
# here
#

# Program:

begin
  if true
  then
    println "here"
  else
    println "not here"
  fi
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
ifTrue.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 8
5	.L.str0:
6		.asciz "not here"
7	# length of .L.str1
8		.int 4
9	.L.str1:
10		.asciz "here"
11	.text
12	main:
13		push rbp
14		push rbx
15		mov rbp, rsp
16		mov rax, 1
17		cmp rax, 1
18		je .L0
19		# Stack pointer unchanged, no stack allocated arguments
20		lea rax, [rip + .L.str0]
21		push rax
22		pop rax
23		mov rax, rax
24		mov rdi, rax
25		# statement primitives do not return results (but will clobber r0/rax)
26		call _prints
27		call _println
28		jmp .L1
29	.L0:
30		# Stack pointer unchanged, no stack allocated arguments
31		lea rax, [rip + .L.str1]
32		push rax
33		pop rax
34		mov rax, rax
35		mov rdi, rax
36		# statement primitives do not return results (but will clobber r0/rax)
37		call _prints
38		call _println
39	.L1:
40		mov rax, 0
41		pop rbx
42		pop rbp
43		ret
44	
45	.section .rodata
46	# length of .L._prints_str0
47		.int 4
48	.L._prints_str0:
49		.asciz "%.*s"
50	.text
51	_prints:
52		push rbp
53		mov rbp, rsp
54		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
55		and rsp, -16
56		mov rdx, rdi
57		mov esi, dword ptr [rdi - 4]
58		lea rdi, [rip + .L._prints_str0]
59		# on x86, al represents the number of SIMD registers used as variadic arguments
60		mov al, 0
61		call printf@plt
62		mov rdi, 0
63		call fflush@plt
64		mov rsp, rbp
65		pop rbp
66		ret
67	
68	.section .rodata
69	# length of .L._println_str0
70		.int 0
71	.L._println_str0:
72		.asciz ""
73	.text
74	_println:
75		push rbp
76		mov rbp, rsp
77		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
78		and rsp, -16
79		lea rdi, [rip + .L._println_str0]
80		call puts@plt
81		mov rdi, 0
82		call fflush@plt
83		mov rsp, rbp
84		pop rbp
85		ret
===========================================================
-- Finished

