./valid/if/ifTrue.wacc
calling the reference compiler on ./valid/if/ifTrue.wacc
-- Test: ifTrue.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple true if statement

# Output:
# here
#

# Program:

begin
  if true
  then
    println "here"
  else
    println "not here"
  fi
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
ifTrue.s contents are:
===========================================================
0	.data
1	@ length of .L.str0
2		.word 8
3	.L.str0:
4		.asciz "not here"
5	@ length of .L.str1
6		.word 4
7	.L.str1:
8		.asciz "here"
9	.align 4
10	.text
11	.global main
12	main:
13		push {fp, lr}
14		push {r8, r10, r12}
15		mov fp, sp
16		mov r8, #1
17		cmp r8, #1
18		beq .L0
19		@ Stack pointer unchanged, no stack allocated arguments
20		ldr r8, =.L.str0
21		push {r8}
22		pop {r8}
23		mov r8, r8
24		mov r0, r8
25		@ statement primitives do not return results (but will clobber r0/rax)
26		bl _prints
27		bl _println
28		b .L1
29	.L0:
30		@ Stack pointer unchanged, no stack allocated arguments
31		ldr r8, =.L.str1
32		push {r8}
33		pop {r8}
34		mov r8, r8
35		mov r0, r8
36		@ statement primitives do not return results (but will clobber r0/rax)
37		bl _prints
38		bl _println
39	.L1:
40		mov r0, #0
41		pop {r8, r10, r12}
42		pop {fp, pc}
43	
44	@ length of .L._prints_str0
45		.word 4
46	.L._prints_str0:
47		.asciz "%.*s"
48	.align 4
49	_prints:
50		push {fp, lr}
51		mov fp, sp
52		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
53		bic sp, sp, #0x7
54		mov r2, r0
55		ldr r1, [r0, #-4]
56		adr r0, .L._prints_str0
57		bl printf
58		mov r0, #0
59		bl fflush
60		mov sp, fp
61		pop {fp, pc}
62	
63	@ length of .L._println_str0
64		.word 0
65	.L._println_str0:
66		.asciz ""
67	.align 4
68	_println:
69		push {fp, lr}
70		mov fp, sp
71		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
72		bic sp, sp, #0x7
73		adr r0, .L._println_str0
74		bl puts
75		mov r0, #0
76		bl fflush
77		mov sp, fp
78		pop {fp, pc}
===========================================================
-- Finished

