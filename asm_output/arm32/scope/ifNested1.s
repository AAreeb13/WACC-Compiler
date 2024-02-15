./valid/scope/ifNested1.wacc
calling the reference compiler on ./valid/scope/ifNested1.wacc
-- Test: ifNested1.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Nested conditional statement

# Output:
# correct
#

# Program:

begin
  int a = 13;
  if a == 13
  then
    if a > 5
    then
      println "correct"
    else
      println "incorrect"
    fi
  else
    println "incorrect"
  fi
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
ifNested1.s contents are:
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
32		cmp r4, #5
33		bgt .L2
34		@ Stack pointer unchanged, no stack allocated arguments
35		ldr r8, =.L.str0
36		push {r8}
37		pop {r8}
38		mov r8, r8
39		mov r0, r8
40		@ statement primitives do not return results (but will clobber r0/rax)
41		bl _prints
42		bl _println
43		b .L3
44	.L2:
45		@ Stack pointer unchanged, no stack allocated arguments
46		ldr r8, =.L.str1
47		push {r8}
48		pop {r8}
49		mov r8, r8
50		mov r0, r8
51		@ statement primitives do not return results (but will clobber r0/rax)
52		bl _prints
53		bl _println
54	.L3:
55	.L1:
56		@ Stack pointer unchanged, no stack allocated variables
57		mov r0, #0
58		pop {r4, r8, r10, r12}
59		pop {fp, pc}
60	
61	@ length of .L._prints_str0
62		.word 4
63	.L._prints_str0:
64		.asciz "%.*s"
65	.align 4
66	_prints:
67		push {fp, lr}
68		mov fp, sp
69		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
70		bic sp, sp, #0x7
71		mov r2, r0
72		ldr r1, [r0, #-4]
73		adr r0, .L._prints_str0
74		bl printf
75		mov r0, #0
76		bl fflush
77		mov sp, fp
78		pop {fp, pc}
79	
80	@ length of .L._println_str0
81		.word 0
82	.L._println_str0:
83		.asciz ""
84	.align 4
85	_println:
86		push {fp, lr}
87		mov fp, sp
88		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
89		bic sp, sp, #0x7
90		adr r0, .L._println_str0
91		bl puts
92		mov r0, #0
93		bl fflush
94		mov sp, fp
95		pop {fp, pc}
===========================================================
-- Finished

