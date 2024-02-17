./valid/scope/indentationNotImportant.wacc
calling the reference compiler on ./valid/scope/indentationNotImportant.wacc
-- Test: indentationNotImportant.wacc

-- Uploaded file: 
---------------------------------------------------------------
# consistent indentation is for readability purposes only

# Output:

# Program:

           begin
 while
false
         do
    skip
done
                    end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
indentationNotImportant.s contents are:
===========================================================
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		push {fp, lr}
6		push {r8, r10, r12}
7		mov fp, sp
8		b .L0
9	.L1:
10	.L0:
11		mov r8, #0
12		cmp r8, #1
13		beq .L1
14		mov r0, #0
15		pop {r8, r10, r12}
16		pop {fp, pc}
===========================================================
-- Finished

