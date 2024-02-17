./valid/function/nested_functions/functionConditionalReturn.wacc
calling the reference compiler on ./valid/function/nested_functions/functionConditionalReturn.wacc
-- Test: functionConditionalReturn.wacc

-- Uploaded file: 
---------------------------------------------------------------
# program has function which only contains an if statement and a return in each branch

# Output:
# true
#

# Program:

begin
  bool f() is
    if true then
      return true
    else
      return false
    fi
  end
  bool x = call f();
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
functionConditionalReturn.s contents are:
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
10		bl wacc_f
11		mov r12, r0
12		@ Stack pointer unchanged, no stack allocated arguments
13		mov r8, r12
14		mov r4, r8
15		@ Stack pointer unchanged, no stack allocated arguments
16		mov r8, r4
17		mov r0, r8
18		@ statement primitives do not return results (but will clobber r0/rax)
19		bl _printb
20		bl _println
21		@ Stack pointer unchanged, no stack allocated variables
22		mov r0, #0
23		pop {r4, r8, r10, r12}
24		pop {fp, pc}
25	
26	wacc_f:
27		push {fp, lr}
28		mov fp, sp
29		mov r8, #1
30		cmp r8, #1
31		beq .L0
32		mov r0, #0
33		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
34		mov sp, fp
35		pop {fp, pc}
36		b .L1
37	.L0:
38		mov r0, #1
39		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
40		mov sp, fp
41		pop {fp, pc}
42	.L1:
43		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
44		@ the .ltorg here is used to generate the constant pool after every function
45		@ this is important for ensuring string literals and large integers are accessible!
46		.ltorg
47	
48	@ length of .L._printb_str0
49		.word 5
50	.L._printb_str0:
51		.asciz "false"
52	@ length of .L._printb_str1
53		.word 4
54	.L._printb_str1:
55		.asciz "true"
56	@ length of .L._printb_str2
57		.word 4
58	.L._printb_str2:
59		.asciz "%.*s"
60	.align 4
61	_printb:
62		push {fp, lr}
63		mov fp, sp
64		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
65		bic sp, sp, #0x7
66		cmp r0, #0
67		bne .L_printb0
68		adr r2, .L._printb_str0
69		b .L_printb1
70	.L_printb0:
71		adr r2, .L._printb_str1
72	.L_printb1:
73		ldr r1, [r2, #-4]
74		adr r0, .L._printb_str2
75		bl printf
76		mov r0, #0
77		bl fflush
78		mov sp, fp
79		pop {fp, pc}
80	
81	@ length of .L._println_str0
82		.word 0
83	.L._println_str0:
84		.asciz ""
85	.align 4
86	_println:
87		push {fp, lr}
88		mov fp, sp
89		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
90		bic sp, sp, #0x7
91		adr r0, .L._println_str0
92		bl puts
93		mov r0, #0
94		bl fflush
95		mov sp, fp
96		pop {fp, pc}
===========================================================
-- Finished

