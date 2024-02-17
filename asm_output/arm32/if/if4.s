./valid/if/if4.wacc
calling the reference compiler on ./valid/if/if4.wacc
-- Test: if4.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Simple conditional statement with boolen expression test

# Output:
# correct
#

# Program:

begin
  bool b = true;
  bool c = false;
  if b && c
  then
    println "incorrect"
  else
    println "correct"
  fi
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
if4.s contents are:
===========================================================
0	.data
1	@ length of .L.str0
2		.word 7
3	.L.str0:
4		.asciz "correct"
5	@ length of .L.str1
6		.word 9
7	.L.str1:
8		.asciz "incorrect"
9	.align 4
10	.text
11	.global main
12	main:
13		push {fp, lr}
14		push {r4, r5, r8, r10, r12}
15		mov fp, sp
16		@ Stack pointer unchanged, no stack allocated variables
17		mov r8, #1
18		mov r4, r8
19		mov r8, #0
20		mov r5, r8
21		cmp r4, #1
22		bne .L2
23		cmp r5, #1
24	.L2:
25		beq .L0
26		@ Stack pointer unchanged, no stack allocated arguments
27		ldr r8, =.L.str0
28		push {r8}
29		pop {r8}
30		mov r8, r8
31		mov r0, r8
32		@ statement primitives do not return results (but will clobber r0/rax)
33		bl _prints
34		bl _println
35		b .L1
36	.L0:
37		@ Stack pointer unchanged, no stack allocated arguments
38		ldr r8, =.L.str1
39		push {r8}
40		pop {r8}
41		mov r8, r8
42		mov r0, r8
43		@ statement primitives do not return results (but will clobber r0/rax)
44		bl _prints
45		bl _println
46	.L1:
47		@ Stack pointer unchanged, no stack allocated variables
48		mov r0, #0
49		pop {r4, r5, r8, r10, r12}
50		pop {fp, pc}
51	
52	@ length of .L._prints_str0
53		.word 4
54	.L._prints_str0:
55		.asciz "%.*s"
56	.align 4
57	_prints:
58		push {fp, lr}
59		mov fp, sp
60		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
61		bic sp, sp, #0x7
62		mov r2, r0
63		ldr r1, [r0, #-4]
64		adr r0, .L._prints_str0
65		bl printf
66		mov r0, #0
67		bl fflush
68		mov sp, fp
69		pop {fp, pc}
70	
71	@ length of .L._println_str0
72		.word 0
73	.L._println_str0:
74		.asciz ""
75	.align 4
76	_println:
77		push {fp, lr}
78		mov fp, sp
79		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
80		bic sp, sp, #0x7
81		adr r0, .L._println_str0
82		bl puts
83		mov r0, #0
84		bl fflush
85		mov sp, fp
86		pop {fp, pc}
===========================================================
-- Finished

