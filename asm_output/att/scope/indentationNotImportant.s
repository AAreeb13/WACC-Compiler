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
0	.globl main
1	.section .rodata
2	.text
3	main:
4		pushq %rbp
5		pushq %rbx
6		movq %rsp, %rbp
7		jmp .L0
8	.L1:
9	.L0:
10		movq $0, %rax
11		cmpq $1, %rax
12		je .L1
13		movq $0, %rax
14		popq %rbx
15		popq %rbp
16		ret
===========================================================
-- Finished

