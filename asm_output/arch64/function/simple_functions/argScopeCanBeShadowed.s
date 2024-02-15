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
5		// push {fp, lr}
6		stp fp, lr, [sp, #-16]!
7		// push {x19}
8		stp x19, xzr, [sp, #-16]!
9		mov fp, sp
10		// Stack pointer unchanged, no stack allocated variables
11		// Stack pointer unchanged, no stack allocated arguments
12		mov x8, #5
13		mov x0, x8
14		bl wacc_f
15		mov x16, x0
16		// Stack pointer unchanged, no stack allocated arguments
17		mov x8, x16
18		mov x19, x8
19		// Stack pointer unchanged, no stack allocated arguments
20		mov x8, x19
21		mov x0, x8
22		// statement primitives do not return results (but will clobber r0/rax)
23		bl _printb
24		bl _println
25		// Stack pointer unchanged, no stack allocated variables
26		mov x0, #0
27		// pop {x19}
28		ldp x19, xzr, [sp], #16
29		// pop {fp, lr}
30		ldp fp, lr, [sp], #16
31		ret
32	
33	wacc_f:
34		// push {fp, lr}
35		stp fp, lr, [sp, #-16]!
36		// push {x19}
37		stp x19, xzr, [sp, #-16]!
38		mov fp, sp
39		// Stack pointer unchanged, no stack allocated variables
40		mov x8, #1
41		mov x19, x8
42		mov x0, x19
43		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
44		mov sp, fp
45		// pop {x19}
46		ldp x19, xzr, [sp], #16
47		// pop {fp, lr}
48		ldp fp, lr, [sp], #16
49		ret
50		// Stack pointer unchanged, no stack allocated variables
51		// 'ere be dragons: this is 100% dead code, functions always end in returns!
52	
53	// length of .L._printb_str0
54		.word 5
55	.L._printb_str0:
56		.asciz "false"
57	// length of .L._printb_str1
58		.word 4
59	.L._printb_str1:
60		.asciz "true"
61	// length of .L._printb_str2
62		.word 4
63	.L._printb_str2:
64		.asciz "%.*s"
65	.align 4
66	_printb:
67		// push {lr}
68		stp lr, xzr, [sp, #-16]!
69		cmp w0, #0
70		b.ne .L_printb0
71		adr x2, .L._printb_str0
72		b .L_printb1
73	.L_printb0:
74		adr x2, .L._printb_str1
75	.L_printb1:
76		ldrsw x1, [x2, #-4]
77		adr x0, .L._printb_str2
78		bl printf
79		mov x0, #0
80		bl fflush
81		// pop {lr}
82		ldp lr, xzr, [sp], #16
83		ret
84	
85	// length of .L._println_str0
86		.word 0
87	.L._println_str0:
88		.asciz ""
89	.align 4
90	_println:
91		// push {lr}
92		stp lr, xzr, [sp, #-16]!
93		adr x0, .L._println_str0
94		bl puts
95		mov x0, #0
96		bl fflush
97		// pop {lr}
98		ldp lr, xzr, [sp], #16
99		ret
===========================================================
-- Finished

