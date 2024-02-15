./valid/sequence/intLeadingZeros.wacc
calling the reference compiler on ./valid/sequence/intLeadingZeros.wacc
-- Test: intLeadingZeros.wacc

-- Uploaded file: 
---------------------------------------------------------------
# integer variable declaration with leading zeroes

# Output:
# 42
# 0
#

# Program:

begin
  int x = 0000000000000000000000000000000000000042 ;
  int y = 0000000000000000000000000000000000000000 ;
  println x ;
  println y
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
intLeadingZeros.s contents are:
===========================================================
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		push {fp, lr}
6		push {r4, r5, r8, r10, r12}
7		mov fp, sp
8		@ Stack pointer unchanged, no stack allocated variables
9		mov r8, #42
10		mov r4, r8
11		mov r8, #0
12		mov r5, r8
13		@ Stack pointer unchanged, no stack allocated arguments
14		mov r8, r4
15		mov r0, r8
16		@ statement primitives do not return results (but will clobber r0/rax)
17		bl _printi
18		bl _println
19		@ Stack pointer unchanged, no stack allocated arguments
20		mov r8, r5
21		mov r0, r8
22		@ statement primitives do not return results (but will clobber r0/rax)
23		bl _printi
24		bl _println
25		@ Stack pointer unchanged, no stack allocated variables
26		mov r0, #0
27		pop {r4, r5, r8, r10, r12}
28		pop {fp, pc}
29	
30	@ length of .L._printi_str0
31		.word 2
32	.L._printi_str0:
33		.asciz "%d"
34	.align 4
35	_printi:
36		push {fp, lr}
37		mov fp, sp
38		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
39		bic sp, sp, #0x7
40		mov r1, r0
41		adr r0, .L._printi_str0
42		bl printf
43		mov r0, #0
44		bl fflush
45		mov sp, fp
46		pop {fp, pc}
47	
48	@ length of .L._println_str0
49		.word 0
50	.L._println_str0:
51		.asciz ""
52	.align 4
53	_println:
54		push {fp, lr}
55		mov fp, sp
56		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
57		bic sp, sp, #0x7
58		adr r0, .L._println_str0
59		bl puts
60		mov r0, #0
61		bl fflush
62		mov sp, fp
63		pop {fp, pc}
===========================================================
-- Finished

