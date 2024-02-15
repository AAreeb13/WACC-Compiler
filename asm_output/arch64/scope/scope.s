./valid/scope/scope.wacc
calling the reference compiler on ./valid/scope/scope.wacc
-- Test: scope.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple scoping test

# Output:

# Program:

begin
  begin
    begin
      begin
        begin
          skip
        end
      end
    end
  end
end

---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
scope.s contents are:
===========================================================
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		// push {fp, lr}
6		stp fp, lr, [sp, #-16]!
7		mov fp, sp
8		mov x0, #0
9		// pop {fp, lr}
10		ldp fp, lr, [sp], #16
11		ret
===========================================================
-- Finished

