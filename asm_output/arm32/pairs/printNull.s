./valid/pairs/printNull.wacc
calling the reference compiler on ./valid/pairs/printNull.wacc
-- Test: printNull.wacc

-- Uploaded file: 
---------------------------------------------------------------
# print the null reference

# Output:
# (nil)
#

# Program:

begin
  println null
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
printNull.s contents are:
===========================================================
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		push {fp, lr}
6		push {r8, r10, r12}
7		mov fp, sp
8		@ Stack pointer unchanged, no stack allocated arguments
9		mov r8, #0
10		mov r0, r8
11		@ statement primitives do not return results (but will clobber r0/rax)
12		bl _printp
13		bl _println
14		mov r0, #0
15		pop {r8, r10, r12}
16		pop {fp, pc}
17	
18	@ length of .L._printp_str0
19		.word 2
20	.L._printp_str0:
21		.asciz "%p"
22	.align 4
23	_printp:
24		push {fp, lr}
25		mov fp, sp
26		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
27		bic sp, sp, #0x7
28		mov r1, r0
29		adr r0, .L._printp_str0
30		bl printf
31		mov r0, #0
32		bl fflush
33		mov sp, fp
34		pop {fp, pc}
35	
36	@ length of .L._println_str0
37		.word 0
38	.L._println_str0:
39		.asciz ""
40	.align 4
41	_println:
42		push {fp, lr}
43		mov fp, sp
44		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
45		bic sp, sp, #0x7
46		adr r0, .L._println_str0
47		bl puts
48		mov r0, #0
49		bl fflush
50		mov sp, fp
51		pop {fp, pc}
===========================================================
-- Finished

