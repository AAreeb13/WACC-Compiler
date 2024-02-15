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
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	.text
4	main:
5		push rbp
6		push rbx
7		mov rbp, rsp
8		jmp .L0
9	.L1:
10	.L0:
11		mov rax, 0
12		cmp rax, 1
13		je .L1
14		mov rax, 0
15		pop rbx
16		pop rbp
17		ret
===========================================================
-- Finished

