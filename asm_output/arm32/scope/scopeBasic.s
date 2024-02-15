./valid/scope/scopeBasic.wacc
calling the reference compiler on ./valid/scope/scopeBasic.wacc
-- Test: scopeBasic.wacc

-- Uploaded file: 
---------------------------------------------------------------
# very simple scoping test

# Output:

# Program:

begin
  skip;
  begin
    skip
  end
end

---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
scopeBasic.s contents are:
===========================================================
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		push {fp, lr}
6		push {r8, r10, r12}
7		mov fp, sp
8		mov r0, #0
9		pop {r8, r10, r12}
10		pop {fp, pc}
===========================================================
-- Finished

