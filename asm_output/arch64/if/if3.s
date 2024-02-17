./valid/if/if3.wacc
calling the reference compiler on ./valid/if/if3.wacc
-- Test: if3.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Simple conditional statement with int comparison test

# Output:
# correct
#

# Program:

begin
  int a = 13;
  int b = 37;
  if a < b
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
if3.s contents are:
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
19		mov x8, #13
20		mov x19, x8
21		mov x8, #37
22		mov x20, x8
23		cmp x19, x20
24		b.lt .L0
25		// Stack pointer unchanged, no stack allocated arguments
26		adrp x8, .L.str0
27		add x8, x8, :lo12:.L.str0
28		// push {x8}
29		stp x8, xzr, [sp, #-16]!
30		// pop {x8}
31		ldp x8, xzr, [sp], #16
32		mov x8, x8
33		mov x0, x8
34		// statement primitives do not return results (but will clobber r0/rax)
35		bl _prints
36		bl _println
37		b .L1
38	.L0:
39		// Stack pointer unchanged, no stack allocated arguments
40		adrp x8, .L.str1
41		add x8, x8, :lo12:.L.str1
42		// push {x8}
43		stp x8, xzr, [sp, #-16]!
44		// pop {x8}
45		ldp x8, xzr, [sp], #16
46		mov x8, x8
47		mov x0, x8
48		// statement primitives do not return results (but will clobber r0/rax)
49		bl _prints
50		bl _println
51	.L1:
52		// Stack pointer unchanged, no stack allocated variables
53		mov x0, #0
54		// pop {x19, x20}
55		ldp x19, x20, [sp], #16
56		// pop {fp, lr}
57		ldp fp, lr, [sp], #16
58		ret
59	
60	// length of .L._prints_str0
61		.word 4
62	.L._prints_str0:
63		.asciz "%.*s"
64	.align 4
65	_prints:
66		// push {lr}
67		stp lr, xzr, [sp, #-16]!
68		mov x2, x0
69		ldrsw x1, [x0, #-4]
70		adr x0, .L._prints_str0
71		bl printf
72		mov x0, #0
73		bl fflush
74		// pop {lr}
75		ldp lr, xzr, [sp], #16
76		ret
77	
78	// length of .L._println_str0
79		.word 0
80	.L._println_str0:
81		.asciz ""
82	.align 4
83	_println:
84		// push {lr}
85		stp lr, xzr, [sp, #-16]!
86		adr x0, .L._println_str0
87		bl puts
88		mov x0, #0
89		bl fflush
90		// pop {lr}
91		ldp lr, xzr, [sp], #16
92		ret
===========================================================
-- Finished

