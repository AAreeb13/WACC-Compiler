./valid/if/whitespace.wacc
calling the reference compiler on ./valid/if/whitespace.wacc
-- Test: whitespace.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Whitespace only important between keyword and variable tokens

# Output:
# 1
#

# Program:

begin
	int a=13;
  if a==13then a=1else a=0fi;
  println a
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
whitespace.s contents are:
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
9		mov r8, #13
10		mov r4, r8
11		cmp r4, #13
12		beq .L0
13		mov r8, #0
14		mov r4, r8
15		b .L1
16	.L0:
17		mov r8, #1
18		mov r4, r8
19	.L1:
20		@ Stack pointer unchanged, no stack allocated arguments
21		mov r8, r4
22		mov r0, r8
23		@ statement primitives do not return results (but will clobber r0/rax)
24		bl _printi
25		bl _println
26		@ Stack pointer unchanged, no stack allocated variables
27		mov r0, #0
28		pop {r4, r8, r10, r12}
29		pop {fp, pc}
30	
31	@ length of .L._printi_str0
32		.word 2
33	.L._printi_str0:
34		.asciz "%d"
35	.align 4
36	_printi:
37		push {fp, lr}
38		mov fp, sp
39		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
40		bic sp, sp, #0x7
41		mov r1, r0
42		adr r0, .L._printi_str0
43		bl printf
44		mov r0, #0
45		bl fflush
46		mov sp, fp
47		pop {fp, pc}
48	
49	@ length of .L._println_str0
50		.word 0
51	.L._println_str0:
52		.asciz ""
53	.align 4
54	_println:
55		push {fp, lr}
56		mov fp, sp
57		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
58		bic sp, sp, #0x7
59		adr r0, .L._println_str0
60		bl puts
61		mov r0, #0
62		bl fflush
63		mov sp, fp
64		pop {fp, pc}
===========================================================
-- Finished

