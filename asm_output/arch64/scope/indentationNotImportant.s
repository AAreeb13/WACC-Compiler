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
5		// push {fp, lr}
6		stp fp, lr, [sp, #-16]!
7		mov fp, sp
8		b .L0
9	.L1:
10	.L0:
11		mov x8, #0
12		cmp x8, #1
13		b.eq .L1
14		mov x0, #0
15		// pop {fp, lr}
16		ldp fp, lr, [sp], #16
17		ret
===========================================================
-- Finished

