./valid/scope/scopeVars.wacc
calling the reference compiler on ./valid/scope/scopeVars.wacc
-- Test: scopeVars.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple variable scoping test

# Output:
# 2
# 4
# 2
#

# Program:

begin
  int x = 2 ;
  println x ;
  begin
    int x = 4 ;
    println x
  end ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
scopeVars.s contents are:
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
9		mov r8, #2
10		mov r4, r8
11		@ Stack pointer unchanged, no stack allocated arguments
12		mov r8, r4
13		mov r0, r8
14		@ statement primitives do not return results (but will clobber r0/rax)
15		bl _printi
16		bl _println
17		@ Stack pointer unchanged, no stack allocated variables
18		mov r8, #4
19		mov r5, r8
20		@ Stack pointer unchanged, no stack allocated arguments
21		mov r8, r5
22		mov r0, r8
23		@ statement primitives do not return results (but will clobber r0/rax)
24		bl _printi
25		bl _println
26		@ Stack pointer unchanged, no stack allocated variables
27		@ Stack pointer unchanged, no stack allocated arguments
28		mov r8, r4
29		mov r0, r8
30		@ statement primitives do not return results (but will clobber r0/rax)
31		bl _printi
32		bl _println
33		@ Stack pointer unchanged, no stack allocated variables
34		mov r0, #0
35		pop {r4, r5, r8, r10, r12}
36		pop {fp, pc}
37	
38	@ length of .L._printi_str0
39		.word 2
40	.L._printi_str0:
41		.asciz "%d"
42	.align 4
43	_printi:
44		push {fp, lr}
45		mov fp, sp
46		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
47		bic sp, sp, #0x7
48		mov r1, r0
49		adr r0, .L._printi_str0
50		bl printf
51		mov r0, #0
52		bl fflush
53		mov sp, fp
54		pop {fp, pc}
55	
56	@ length of .L._println_str0
57		.word 0
58	.L._println_str0:
59		.asciz ""
60	.align 4
61	_println:
62		push {fp, lr}
63		mov fp, sp
64		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
65		bic sp, sp, #0x7
66		adr r0, .L._println_str0
67		bl puts
68		mov r0, #0
69		bl fflush
70		mov sp, fp
71		pop {fp, pc}
===========================================================
-- Finished

