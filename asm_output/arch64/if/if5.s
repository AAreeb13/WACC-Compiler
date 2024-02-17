./valid/if/if5.wacc
calling the reference compiler on ./valid/if/if5.wacc
-- Test: if5.wacc

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
  if b || c
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
if5.s contents are:
===========================================================
0	.data
1	// length of .L.str0
2		.word 9
3	.L.str0:
4		.asciz "incorrect"
5	// length of .L.str1
6		.word 7
7	.L.str1:
8		.asciz "correct"
9	.align 4
10	.text
11	.global main
12	main:
13		// push {fp, lr}
14		stp fp, lr, [sp, #-16]!
15		// push {x19, x20}
16		stp x19, x20, [sp, #-16]!
17		mov fp, sp
18		// Stack pointer unchanged, no stack allocated variables
19		mov x8, #1
20		mov x19, x8
21		mov x8, #0
22		mov x20, x8
23		cmp x19, #1
24		b.eq .L2
25		cmp x20, #1
26	.L2:
27		b.eq .L0
28		// Stack pointer unchanged, no stack allocated arguments
29		adrp x8, .L.str0
30		add x8, x8, :lo12:.L.str0
31		// push {x8}
32		stp x8, xzr, [sp, #-16]!
33		// pop {x8}
34		ldp x8, xzr, [sp], #16
35		mov x8, x8
36		mov x0, x8
37		// statement primitives do not return results (but will clobber r0/rax)
38		bl _prints
39		bl _println
40		b .L1
41	.L0:
42		// Stack pointer unchanged, no stack allocated arguments
43		adrp x8, .L.str1
44		add x8, x8, :lo12:.L.str1
45		// push {x8}
46		stp x8, xzr, [sp, #-16]!
47		// pop {x8}
48		ldp x8, xzr, [sp], #16
49		mov x8, x8
50		mov x0, x8
51		// statement primitives do not return results (but will clobber r0/rax)
52		bl _prints
53		bl _println
54	.L1:
55		// Stack pointer unchanged, no stack allocated variables
56		mov x0, #0
57		// pop {x19, x20}
58		ldp x19, x20, [sp], #16
59		// pop {fp, lr}
60		ldp fp, lr, [sp], #16
61		ret
62	
63	// length of .L._prints_str0
64		.word 4
65	.L._prints_str0:
66		.asciz "%.*s"
67	.align 4
68	_prints:
69		// push {lr}
70		stp lr, xzr, [sp, #-16]!
71		mov x2, x0
72		ldrsw x1, [x0, #-4]
73		adr x0, .L._prints_str0
74		bl printf
75		mov x0, #0
76		bl fflush
77		// pop {lr}
78		ldp lr, xzr, [sp], #16
79		ret
80	
81	// length of .L._println_str0
82		.word 0
83	.L._println_str0:
84		.asciz ""
85	.align 4
86	_println:
87		// push {lr}
88		stp lr, xzr, [sp, #-16]!
89		adr x0, .L._println_str0
90		bl puts
91		mov x0, #0
92		bl fflush
93		// pop {lr}
94		ldp lr, xzr, [sp], #16
95		ret
===========================================================
-- Finished

