./valid/runtimeErr/nullDereference/freeNull.wacc
calling the reference compiler on ./valid/runtimeErr/nullDereference/freeNull.wacc
-- Test: freeNull.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Create and free a null pair

# Output:
# #runtime_error#

# Exit:
# 255

# Program:

begin
  pair(pair, pair) a = null ;
  free a
end

---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
freeNull.s contents are:
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
9		mov r8, #0
10		mov r4, r8
11		@ Stack pointer unchanged, no stack allocated arguments
12		mov r8, r4
13		mov r0, r8
14		@ statement primitives do not return results (but will clobber r0/rax)
15		bl _freepair
16		@ Stack pointer unchanged, no stack allocated variables
17		mov r0, #0
18		pop {r4, r8, r10, r12}
19		pop {fp, pc}
20	
21	_freepair:
22		push {fp, lr}
23		mov fp, sp
24		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
25		bic sp, sp, #0x7
26		cmp r0, #0
27		bleq _errNull
28		bl free
29		mov sp, fp
30		pop {fp, pc}
31	
32	@ length of .L._prints_str0
33		.word 4
34	.L._prints_str0:
35		.asciz "%.*s"
36	.align 4
37	_prints:
38		push {fp, lr}
39		mov fp, sp
40		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
41		bic sp, sp, #0x7
42		mov r2, r0
43		ldr r1, [r0, #-4]
44		adr r0, .L._prints_str0
45		bl printf
46		mov r0, #0
47		bl fflush
48		mov sp, fp
49		pop {fp, pc}
50	
51	_exit:
52		push {fp, lr}
53		mov fp, sp
54		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
55		bic sp, sp, #0x7
56		bl exit
57		mov sp, fp
58		pop {fp, pc}
59	
60	@ length of .L._errNull_str0
61		.word 45
62	.L._errNull_str0:
63		.asciz "fatal error: null pair dereferenced or freed\n"
64	.align 4
65	_errNull:
66		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
67		bic sp, sp, #0x7
68		adr r0, .L._errNull_str0
69		bl _prints
70		mov r0, #255
71		bl _exit
===========================================================
-- Finished

