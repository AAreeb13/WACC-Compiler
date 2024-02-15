./valid/scope/splitScope.wacc
calling the reference compiler on ./valid/scope/splitScope.wacc
-- Test: splitScope.wacc

-- Uploaded file: 
---------------------------------------------------------------
# splits the first appearances of variables by a new scope to ensure proper grouping

# Output:
# 3
# 2
#

# Program:

begin
  int x = 1 ;
  begin
    x = 3 ;
    bool z = true
  end ;
  int y = 2 ;
  println x ;
  println y
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
splitScope.s contents are:
===========================================================
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		push {fp, lr}
6		push {r4, r5, r6, r8, r10, r12}
7		mov fp, sp
8		@ Stack pointer unchanged, no stack allocated variables
9		mov r8, #1
10		mov r4, r8
11		@ Stack pointer unchanged, no stack allocated variables
12		mov r8, #3
13		mov r4, r8
14		mov r8, #1
15		mov r6, r8
16		@ Stack pointer unchanged, no stack allocated variables
17		mov r8, #2
18		mov r5, r8
19		@ Stack pointer unchanged, no stack allocated arguments
20		mov r8, r4
21		mov r0, r8
22		@ statement primitives do not return results (but will clobber r0/rax)
23		bl _printi
24		bl _println
25		@ Stack pointer unchanged, no stack allocated arguments
26		mov r8, r5
27		mov r0, r8
28		@ statement primitives do not return results (but will clobber r0/rax)
29		bl _printi
30		bl _println
31		@ Stack pointer unchanged, no stack allocated variables
32		mov r0, #0
33		pop {r4, r5, r6, r8, r10, r12}
34		pop {fp, pc}
35	
36	@ length of .L._printi_str0
37		.word 2
38	.L._printi_str0:
39		.asciz "%d"
40	.align 4
41	_printi:
42		push {fp, lr}
43		mov fp, sp
44		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
45		bic sp, sp, #0x7
46		mov r1, r0
47		adr r0, .L._printi_str0
48		bl printf
49		mov r0, #0
50		bl fflush
51		mov sp, fp
52		pop {fp, pc}
53	
54	@ length of .L._println_str0
55		.word 0
56	.L._println_str0:
57		.asciz ""
58	.align 4
59	_println:
60		push {fp, lr}
61		mov fp, sp
62		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
63		bic sp, sp, #0x7
64		adr r0, .L._println_str0
65		bl puts
66		mov r0, #0
67		bl fflush
68		mov sp, fp
69		pop {fp, pc}
===========================================================
-- Finished

