./valid/IO/print/print-backspace.wacc
calling the reference compiler on ./valid/IO/print/print-backspace.wacc
-- Test: print-backspace.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple print statement off a string with a backspace character

# Output:
# Hello World!
#

# Program:

begin
  print "Hello\b World!\n"
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
print-backspace.s contents are:
===========================================================
0	.data
1	@ length of .L.str0
2		.word 14
3	.L.str0:
4		.asciz "Hello\b World!\n"
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
20		mov r0, #0
21		pop {r8, r10, r12}
22		pop {fp, pc}
23	
24	@ length of .L._prints_str0
25		.word 4
26	.L._prints_str0:
27		.asciz "%.*s"
28	.align 4
29	_prints:
30		push {fp, lr}
31		mov fp, sp
32		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
33		bic sp, sp, #0x7
34		mov r2, r0
35		ldr r1, [r0, #-4]
36		adr r0, .L._prints_str0
37		bl printf
38		mov r0, #0
39		bl fflush
40		mov sp, fp
41		pop {fp, pc}
===========================================================
-- Finished

