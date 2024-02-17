./valid/if/if1.wacc
calling the reference compiler on ./valid/if/if1.wacc
-- Test: if1.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Simple conditional statement with int comparison test

# Output:
# correct
#

# Program:

begin
  int a = 13;
  if a == 13
  then
    println "correct"
  else
    println "incorrect"
  fi
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
if1.s contents are:
===========================================================
0	.data
1	@ length of .L.str0
2		.word 9
3	.L.str0:
4		.asciz "incorrect"
5	@ length of .L.str1
6		.word 7
7	.L.str1:
8		.asciz "correct"
9	.align 4
10	.text
11	.global main
12	main:
13		push {fp, lr}
14		push {r4, r8, r10, r12}
15		mov fp, sp
16		@ Stack pointer unchanged, no stack allocated variables
17		mov r8, #13
18		mov r4, r8
19		cmp r4, #13
20		beq .L0
21		@ Stack pointer unchanged, no stack allocated arguments
22		ldr r8, =.L.str0
23		push {r8}
24		pop {r8}
25		mov r8, r8
26		mov r0, r8
27		@ statement primitives do not return results (but will clobber r0/rax)
28		bl _prints
29		bl _println
30		b .L1
31	.L0:
32		@ Stack pointer unchanged, no stack allocated arguments
33		ldr r8, =.L.str1
34		push {r8}
35		pop {r8}
36		mov r8, r8
37		mov r0, r8
38		@ statement primitives do not return results (but will clobber r0/rax)
39		bl _prints
40		bl _println
41	.L1:
42		@ Stack pointer unchanged, no stack allocated variables
43		mov r0, #0
44		pop {r4, r8, r10, r12}
45		pop {fp, pc}
46	
47	@ length of .L._prints_str0
48		.word 4
49	.L._prints_str0:
50		.asciz "%.*s"
51	.align 4
52	_prints:
53		push {fp, lr}
54		mov fp, sp
55		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
56		bic sp, sp, #0x7
57		mov r2, r0
58		ldr r1, [r0, #-4]
59		adr r0, .L._prints_str0
60		bl printf
61		mov r0, #0
62		bl fflush
63		mov sp, fp
64		pop {fp, pc}
65	
66	@ length of .L._println_str0
67		.word 0
68	.L._println_str0:
69		.asciz ""
70	.align 4
71	_println:
72		push {fp, lr}
73		mov fp, sp
74		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
75		bic sp, sp, #0x7
76		adr r0, .L._println_str0
77		bl puts
78		mov r0, #0
79		bl fflush
80		mov sp, fp
81		pop {fp, pc}
===========================================================
-- Finished

