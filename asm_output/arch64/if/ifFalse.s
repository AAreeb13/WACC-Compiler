./valid/if/ifFalse.wacc
calling the reference compiler on ./valid/if/ifFalse.wacc
-- Test: ifFalse.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple false if statement

# Output:
# here
#

# Program:

begin
  if false
  then
    println "not here"
  else
    println "here"
  fi
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
ifFalse.s contents are:
===========================================================
0	.data
1	// length of .L.str0
2		.word 4
3	.L.str0:
4		.asciz "here"
5	// length of .L.str1
6		.word 8
7	.L.str1:
8		.asciz "not here"
9	.align 4
10	.text
11	.global main
12	main:
13		// push {fp, lr}
14		stp fp, lr, [sp, #-16]!
15		mov fp, sp
16		mov x8, #0
17		cmp x8, #1
18		b.eq .L0
19		// Stack pointer unchanged, no stack allocated arguments
20		adrp x8, .L.str0
21		add x8, x8, :lo12:.L.str0
22		// push {x8}
23		stp x8, xzr, [sp, #-16]!
24		// pop {x8}
25		ldp x8, xzr, [sp], #16
26		mov x8, x8
27		mov x0, x8
28		// statement primitives do not return results (but will clobber r0/rax)
29		bl _prints
30		bl _println
31		b .L1
32	.L0:
33		// Stack pointer unchanged, no stack allocated arguments
34		adrp x8, .L.str1
35		add x8, x8, :lo12:.L.str1
36		// push {x8}
37		stp x8, xzr, [sp, #-16]!
38		// pop {x8}
39		ldp x8, xzr, [sp], #16
40		mov x8, x8
41		mov x0, x8
42		// statement primitives do not return results (but will clobber r0/rax)
43		bl _prints
44		bl _println
45	.L1:
46		mov x0, #0
47		// pop {fp, lr}
48		ldp fp, lr, [sp], #16
49		ret
50	
51	// length of .L._prints_str0
52		.word 4
53	.L._prints_str0:
54		.asciz "%.*s"
55	.align 4
56	_prints:
57		// push {lr}
58		stp lr, xzr, [sp, #-16]!
59		mov x2, x0
60		ldrsw x1, [x0, #-4]
61		adr x0, .L._prints_str0
62		bl printf
63		mov x0, #0
64		bl fflush
65		// pop {lr}
66		ldp lr, xzr, [sp], #16
67		ret
68	
69	// length of .L._println_str0
70		.word 0
71	.L._println_str0:
72		.asciz ""
73	.align 4
74	_println:
75		// push {lr}
76		stp lr, xzr, [sp, #-16]!
77		adr x0, .L._println_str0
78		bl puts
79		mov x0, #0
80		bl fflush
81		// pop {lr}
82		ldp lr, xzr, [sp], #16
83		ret
===========================================================
-- Finished

