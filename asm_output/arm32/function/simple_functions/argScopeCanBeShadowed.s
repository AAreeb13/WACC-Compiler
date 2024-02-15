./valid/function/simple_functions/argScopeCanBeShadowed.wacc
calling the reference compiler on ./valid/function/simple_functions/argScopeCanBeShadowed.wacc
-- Test: argScopeCanBeShadowed.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Arguments can be shadowed by the function body

# Output:
# true
#

#Program:

begin
  bool f(int x) is
    bool x = true ;
    return x
  end

  bool x = call f(5);
  println x

end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
argScopeCanBeShadowed.s contents are:
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
10		mov r8, #5
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
21		bl _printb
22		bl _println
23		@ Stack pointer unchanged, no stack allocated variables
24		mov r0, #0
25		pop {r4, r8, r10, r12}
26		pop {fp, pc}
27	
28	wacc_f:
29		push {fp, lr}
30		push {r4}
31		mov fp, sp
32		@ Stack pointer unchanged, no stack allocated variables
33		mov r8, #1
34		mov r4, r8
35		mov r0, r4
36		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
37		mov sp, fp
38		pop {r4}
39		pop {fp, pc}
40		@ Stack pointer unchanged, no stack allocated variables
41		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
42		@ the .ltorg here is used to generate the constant pool after every function
43		@ this is important for ensuring string literals and large integers are accessible!
44		.ltorg
45	
46	@ length of .L._printb_str0
47		.word 5
48	.L._printb_str0:
49		.asciz "false"
50	@ length of .L._printb_str1
51		.word 4
52	.L._printb_str1:
53		.asciz "true"
54	@ length of .L._printb_str2
55		.word 4
56	.L._printb_str2:
57		.asciz "%.*s"
58	.align 4
59	_printb:
60		push {fp, lr}
61		mov fp, sp
62		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
63		bic sp, sp, #0x7
64		cmp r0, #0
65		bne .L_printb0
66		adr r2, .L._printb_str0
67		b .L_printb1
68	.L_printb0:
69		adr r2, .L._printb_str1
70	.L_printb1:
71		ldr r1, [r2, #-4]
72		adr r0, .L._printb_str2
73		bl printf
74		mov r0, #0
75		bl fflush
76		mov sp, fp
77		pop {fp, pc}
78	
79	@ length of .L._println_str0
80		.word 0
81	.L._println_str0:
82		.asciz ""
83	.align 4
84	_println:
85		push {fp, lr}
86		mov fp, sp
87		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
88		bic sp, sp, #0x7
89		adr r0, .L._println_str0
90		bl puts
91		mov r0, #0
92		bl fflush
93		mov sp, fp
94		pop {fp, pc}
===========================================================
-- Finished

