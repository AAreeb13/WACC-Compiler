./valid/pairs/printNullPair.wacc
calling the reference compiler on ./valid/pairs/printNullPair.wacc
-- Test: printNullPair.wacc

-- Uploaded file: 
---------------------------------------------------------------
# print pair a null pair

# Output:
# (nil)
#

# Program:

begin
  pair(pair, pair) p = null ;
  println p
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
printNullPair.s contents are:
===========================================================
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		push {fp, lr}
6		push {r4, r8, r10, r12}
7		mov fp, sp
8		@ Stack pointer unchanged, no stack allocated variables
9		mov r8, #0
10		mov r4, r8
11		@ Stack pointer unchanged, no stack allocated arguments
12		mov r8, r4
13		mov r0, r8
14		@ statement primitives do not return results (but will clobber r0/rax)
15		bl _printp
16		bl _println
17		@ Stack pointer unchanged, no stack allocated variables
18		mov r0, #0
19		pop {r4, r8, r10, r12}
20		pop {fp, pc}
21	
22	@ length of .L._printp_str0
23		.word 2
24	.L._printp_str0:
25		.asciz "%p"
26	.align 4
27	_printp:
28		push {fp, lr}
29		mov fp, sp
30		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
31		bic sp, sp, #0x7
32		mov r1, r0
33		adr r0, .L._printp_str0
34		bl printf
35		mov r0, #0
36		bl fflush
37		mov sp, fp
38		pop {fp, pc}
39	
40	@ length of .L._println_str0
41		.word 0
42	.L._println_str0:
43		.asciz ""
44	.align 4
45	_println:
46		push {fp, lr}
47		mov fp, sp
48		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
49		bic sp, sp, #0x7
50		adr r0, .L._println_str0
51		bl puts
52		mov r0, #0
53		bl fflush
54		mov sp, fp
55		pop {fp, pc}
===========================================================
-- Finished

