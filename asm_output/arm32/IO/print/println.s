./valid/IO/print/println.wacc
calling the reference compiler on ./valid/IO/print/println.wacc
-- Test: println.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple println statement

# Output:
# Hello World!
#

# Program:

begin
  println "Hello World!"
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
println.s contents are:
===========================================================
0	.data
1	@ length of .L.str0
2		.word 12
3	.L.str0:
4		.asciz "Hello World!"
5	.align 4
6	.text
7	.global main
8	main:
9		push {fp, lr}
10		push {r8, r10, r12}
11		mov fp, sp
12		@ Stack pointer unchanged, no stack allocated arguments
13		ldr r8, =.L.str0
14		push {r8}
15		pop {r8}
16		mov r8, r8
17		mov r0, r8
18		@ statement primitives do not return results (but will clobber r0/rax)
19		bl _prints
20		bl _println
21		mov r0, #0
22		pop {r8, r10, r12}
23		pop {fp, pc}
24	
25	@ length of .L._prints_str0
26		.word 4
27	.L._prints_str0:
28		.asciz "%.*s"
29	.align 4
30	_prints:
31		push {fp, lr}
32		mov fp, sp
33		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
34		bic sp, sp, #0x7
35		mov r2, r0
36		ldr r1, [r0, #-4]
37		adr r0, .L._prints_str0
38		bl printf
39		mov r0, #0
40		bl fflush
41		mov sp, fp
42		pop {fp, pc}
43	
44	@ length of .L._println_str0
45		.word 0
46	.L._println_str0:
47		.asciz ""
48	.align 4
49	_println:
50		push {fp, lr}
51		mov fp, sp
52		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
53		bic sp, sp, #0x7
54		adr r0, .L._println_str0
55		bl puts
56		mov r0, #0
57		bl fflush
58		mov sp, fp
59		pop {fp, pc}
===========================================================
-- Finished

