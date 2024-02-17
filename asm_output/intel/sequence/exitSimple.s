./valid/sequence/exitSimple.wacc
calling the reference compiler on ./valid/sequence/exitSimple.wacc
-- Test: exitSimple.wacc

-- Uploaded file: 
---------------------------------------------------------------
# exit with unreachable print

# Output:

# Exit:
# 42

# Program:

begin
  exit 42 ;
  println "Should not print this."
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
exitSimple.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 22
5	.L.str0:
6		.asciz "Should not print this."
7	.text
8	main:
9		push rbp
10		push rbx
11		mov rbp, rsp
12		# Stack pointer unchanged, no stack allocated arguments
13		mov rax, 42
14		mov rdi, rax
15		# statement primitives do not return results (but will clobber r0/rax)
16		call _exit
17		# Stack pointer unchanged, no stack allocated arguments
18		lea rax, [rip + .L.str0]
19		push rax
20		pop rax
21		mov rax, rax
22		mov rdi, rax
23		# statement primitives do not return results (but will clobber r0/rax)
24		call _prints
25		call _println
26		mov rax, 0
27		pop rbx
28		pop rbp
29		ret
30	
31	.section .rodata
32	# length of .L._prints_str0
33		.int 4
34	.L._prints_str0:
35		.asciz "%.*s"
36	.text
37	_prints:
38		push rbp
39		mov rbp, rsp
40		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
41		and rsp, -16
42		mov rdx, rdi
43		mov esi, dword ptr [rdi - 4]
44		lea rdi, [rip + .L._prints_str0]
45		# on x86, al represents the number of SIMD registers used as variadic arguments
46		mov al, 0
47		call printf@plt
48		mov rdi, 0
49		call fflush@plt
50		mov rsp, rbp
51		pop rbp
52		ret
53	
54	.section .rodata
55	# length of .L._println_str0
56		.int 0
57	.L._println_str0:
58		.asciz ""
59	.text
60	_println:
61		push rbp
62		mov rbp, rsp
63		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
64		and rsp, -16
65		lea rdi, [rip + .L._println_str0]
66		call puts@plt
67		mov rdi, 0
68		call fflush@plt
69		mov rsp, rbp
70		pop rbp
71		ret
72	
73	_exit:
74		push rbp
75		mov rbp, rsp
76		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
77		and rsp, -16
78		call exit@plt
79		mov rsp, rbp
80		pop rbp
81		ret
===========================================================
-- Finished

