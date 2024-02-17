./valid/function/simple_functions/sameArgName2.wacc
calling the reference compiler on ./valid/function/simple_functions/sameArgName2.wacc
-- Test: sameArgName2.wacc

-- Uploaded file: 
---------------------------------------------------------------
# program with function that has same parameter name as function
# and also returns to same named variable

# Output:
# 99
#

# Program:

begin
  int f(int f) is
    return f
  end

  int f = call f(99);
  println f
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
sameArgName2.s contents are:
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
9		@ Stack pointer unchanged, no stack allocated arguments
10		mov r8, #99
11		mov r0, r8
12		bl wacc_f
13		mov r12, r0
14		@ Stack pointer unchanged, no stack allocated arguments
15		mov r8, r12
16		mov r4, r8
17		@ Stack pointer unchanged, no stack allocated arguments
18		mov r8, r4
19		mov r0, r8
20		@ statement primitives do not return results (but will clobber r0/rax)
21		bl _printi
22		bl _println
23		@ Stack pointer unchanged, no stack allocated variables
24		mov r0, #0
25		pop {r4, r8, r10, r12}
26		pop {fp, pc}
27	
28	wacc_f:
29		push {fp, lr}
30		mov fp, sp
31		mov r0, r0
32		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
33		mov sp, fp
34		pop {fp, pc}
35		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
36		@ the .ltorg here is used to generate the constant pool after every function
37		@ this is important for ensuring string literals and large integers are accessible!
38		.ltorg
39	
40	@ length of .L._printi_str0
41		.word 2
42	.L._printi_str0:
43		.asciz "%d"
44	.align 4
45	_printi:
46		push {fp, lr}
47		mov fp, sp
48		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
49		bic sp, sp, #0x7
50		mov r1, r0
51		adr r0, .L._printi_str0
52		bl printf
53		mov r0, #0
54		bl fflush
55		mov sp, fp
56		pop {fp, pc}
57	
58	@ length of .L._println_str0
59		.word 0
60	.L._println_str0:
61		.asciz ""
62	.align 4
63	_println:
64		push {fp, lr}
65		mov fp, sp
66		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
67		bic sp, sp, #0x7
68		adr r0, .L._println_str0
69		bl puts
70		mov r0, #0
71		bl fflush
72		mov sp, fp
73		pop {fp, pc}
===========================================================
-- Finished

