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
5		// push {fp, lr}
6		stp fp, lr, [sp, #-16]!
7		// push {x19}
8		stp x19, xzr, [sp, #-16]!
9		mov fp, sp
10		// Stack pointer unchanged, no stack allocated variables
11		// Stack pointer unchanged, no stack allocated arguments
12		bl wacc_f
13		mov x16, x0
14		// Stack pointer unchanged, no stack allocated arguments
15		mov x8, x16
16		mov x19, x8
17		// Stack pointer unchanged, no stack allocated arguments
18		mov x8, x19
19		mov x0, x8
20		// statement primitives do not return results (but will clobber r0/rax)
21		bl _printb
22		bl _println
23		// Stack pointer unchanged, no stack allocated variables
24		mov x0, #0
25		// pop {x19}
26		ldp x19, xzr, [sp], #16
27		// pop {fp, lr}
28		ldp fp, lr, [sp], #16
29		ret
30	
31	wacc_f:
32		// push {fp, lr}
33		stp fp, lr, [sp, #-16]!
34		mov fp, sp
35		mov x8, #1
36		cmp x8, #1
37		b.eq .L0
38		mov x0, #0
39		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
40		mov sp, fp
41		// pop {fp, lr}
42		ldp fp, lr, [sp], #16
43		ret
44		b .L1
45	.L0:
46		mov x0, #1
47		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
48		mov sp, fp
49		// pop {fp, lr}
50		ldp fp, lr, [sp], #16
51		ret
52	.L1:
53		// 'ere be dragons: this is 100% dead code, functions always end in returns!
54	
55	// length of .L._printb_str0
56		.word 5
57	.L._printb_str0:
58		.asciz "false"
59	// length of .L._printb_str1
60		.word 4
61	.L._printb_str1:
62		.asciz "true"
63	// length of .L._printb_str2
64		.word 4
65	.L._printb_str2:
66		.asciz "%.*s"
67	.align 4
68	_printb:
69		// push {lr}
70		stp lr, xzr, [sp, #-16]!
71		cmp w0, #0
72		b.ne .L_printb0
73		adr x2, .L._printb_str0
74		b .L_printb1
75	.L_printb0:
76		adr x2, .L._printb_str1
77	.L_printb1:
78		ldrsw x1, [x2, #-4]
79		adr x0, .L._printb_str2
80		bl printf
81		mov x0, #0
82		bl fflush
83		// pop {lr}
84		ldp lr, xzr, [sp], #16
85		ret
86	
87	// length of .L._println_str0
88		.word 0
89	.L._println_str0:
90		.asciz ""
91	.align 4
92	_println:
93		// push {lr}
94		stp lr, xzr, [sp, #-16]!
95		adr x0, .L._println_str0
96		bl puts
97		mov x0, #0
98		bl fflush
99		// pop {lr}
100		ldp lr, xzr, [sp], #16
101		ret
===========================================================
-- Finished

