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
15		// push {x19}
16		stp x19, xzr, [sp, #-16]!
17		mov fp, sp
18		// Stack pointer unchanged, no stack allocated variables
19		mov x8, #13
20		mov x19, x8
21		cmp x19, #13
22		b.eq .L0
23		// Stack pointer unchanged, no stack allocated arguments
24		adrp x8, .L.str0
25		add x8, x8, :lo12:.L.str0
26		// push {x8}
27		stp x8, xzr, [sp, #-16]!
28		// pop {x8}
29		ldp x8, xzr, [sp], #16
30		mov x8, x8
31		mov x0, x8
32		// statement primitives do not return results (but will clobber r0/rax)
33		bl _prints
34		bl _println
35		b .L1
36	.L0:
37		cmp x19, #5
38		b.gt .L2
39		// Stack pointer unchanged, no stack allocated arguments
40		adrp x8, .L.str0
41		add x8, x8, :lo12:.L.str0
42		// push {x8}
43		stp x8, xzr, [sp, #-16]!
44		// pop {x8}
45		ldp x8, xzr, [sp], #16
46		mov x8, x8
47		mov x0, x8
48		// statement primitives do not return results (but will clobber r0/rax)
49		bl _prints
50		bl _println
51		b .L3
52	.L2:
53		// Stack pointer unchanged, no stack allocated arguments
54		adrp x8, .L.str1
55		add x8, x8, :lo12:.L.str1
56		// push {x8}
57		stp x8, xzr, [sp, #-16]!
58		// pop {x8}
59		ldp x8, xzr, [sp], #16
60		mov x8, x8
61		mov x0, x8
62		// statement primitives do not return results (but will clobber r0/rax)
63		bl _prints
64		bl _println
65	.L3:
66	.L1:
67		// Stack pointer unchanged, no stack allocated variables
68		mov x0, #0
69		// pop {x19}
70		ldp x19, xzr, [sp], #16
71		// pop {fp, lr}
72		ldp fp, lr, [sp], #16
73		ret
74	
75	// length of .L._prints_str0
76		.word 4
77	.L._prints_str0:
78		.asciz "%.*s"
79	.align 4
80	_prints:
81		// push {lr}
82		stp lr, xzr, [sp, #-16]!
83		mov x2, x0
84		ldrsw x1, [x0, #-4]
85		adr x0, .L._prints_str0
86		bl printf
87		mov x0, #0
88		bl fflush
89		// pop {lr}
90		ldp lr, xzr, [sp], #16
91		ret
92	
93	// length of .L._println_str0
94		.word 0
95	.L._println_str0:
96		.asciz ""
97	.align 4
98	_println:
99		// push {lr}
100		stp lr, xzr, [sp, #-16]!
101		adr x0, .L._println_str0
102		bl puts
103		mov x0, #0
104		bl fflush
105		// pop {lr}
106		ldp lr, xzr, [sp], #16
107		ret
===========================================================
-- Finished

