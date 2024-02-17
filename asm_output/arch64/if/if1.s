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
37		// Stack pointer unchanged, no stack allocated arguments
38		adrp x8, .L.str1
39		add x8, x8, :lo12:.L.str1
40		// push {x8}
41		stp x8, xzr, [sp, #-16]!
42		// pop {x8}
43		ldp x8, xzr, [sp], #16
44		mov x8, x8
45		mov x0, x8
46		// statement primitives do not return results (but will clobber r0/rax)
47		bl _prints
48		bl _println
49	.L1:
50		// Stack pointer unchanged, no stack allocated variables
51		mov x0, #0
52		// pop {x19}
53		ldp x19, xzr, [sp], #16
54		// pop {fp, lr}
55		ldp fp, lr, [sp], #16
56		ret
57	
58	// length of .L._prints_str0
59		.word 4
60	.L._prints_str0:
61		.asciz "%.*s"
62	.align 4
63	_prints:
64		// push {lr}
65		stp lr, xzr, [sp, #-16]!
66		mov x2, x0
67		ldrsw x1, [x0, #-4]
68		adr x0, .L._prints_str0
69		bl printf
70		mov x0, #0
71		bl fflush
72		// pop {lr}
73		ldp lr, xzr, [sp], #16
74		ret
75	
76	// length of .L._println_str0
77		.word 0
78	.L._println_str0:
79		.asciz ""
80	.align 4
81	_println:
82		// push {lr}
83		stp lr, xzr, [sp, #-16]!
84		adr x0, .L._println_str0
85		bl puts
86		mov x0, #0
87		bl fflush
88		// pop {lr}
89		ldp lr, xzr, [sp], #16
90		ret
===========================================================
-- Finished

