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
0	.globl main
1	.section .rodata
2	.text
3	main:
4		pushq %rbp
5		pushq %rbx
6		movq %rsp, %rbp
7		movq $0, %rax
8		popq %rbx
9		popq %rbp
10		ret
===========================================================
-- Finished

