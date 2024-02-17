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
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	.text
4	main:
5		push rbp
6		push rbx
7		mov rbp, rsp
8		mov rax, 0
9		pop rbx
10		pop rbp
11		ret
===========================================================
-- Finished

