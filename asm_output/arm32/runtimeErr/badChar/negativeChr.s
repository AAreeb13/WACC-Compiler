./valid/runtimeErr/badChar/negativeChr.wacc
calling the reference compiler on ./valid/runtimeErr/badChar/negativeChr.wacc
-- Test: negativeChr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# try to convert negative int to character

# Output:
# #runtime_error#

# Exit:
# 255

# Program:

begin
    char c = chr -1
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
negativeChr.s contents are:
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
9		ldr r8, =-1
10		mov r8, r8
11		ldr r9, =-128
12		tst r8, r9
13		movne r1, r8
14		blne _errBadChar
15		push {r8}
16		pop {r8}
17		mov r8, r8
18		mov r4, r8
19		@ Stack pointer unchanged, no stack allocated variables
20		mov r0, #0
21		pop {r4, r8, r10, r12}
22		pop {fp, pc}
23	
24	@ length of .L._prints_str0
25		.word 4
26	.L._prints_str0:
27		.asciz "%.*s"
28	.align 4
29	_prints:
30		push {fp, lr}
31		mov fp, sp
32		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
33		bic sp, sp, #0x7
34		mov r2, r0
35		ldr r1, [r0, #-4]
36		adr r0, .L._prints_str0
37		bl printf
38		mov r0, #0
39		bl fflush
40		mov sp, fp
41		pop {fp, pc}
42	
43	_exit:
44		push {fp, lr}
45		mov fp, sp
46		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
47		bic sp, sp, #0x7
48		bl exit
49		mov sp, fp
50		pop {fp, pc}
51	
52	@ length of .L._errBadChar_str0
53		.word 50
54	.L._errBadChar_str0:
55		.asciz "fatal error: int %d is not ascii character 0-127 \n"
56	.align 4
57	_errBadChar:
58		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
59		bic sp, sp, #0x7
60		adr r0, .L._errBadChar_str0
61		bl printf
62		mov r0, #0
63		bl fflush
64		mov r0, #255
65		bl _exit
===========================================================
-- Finished

