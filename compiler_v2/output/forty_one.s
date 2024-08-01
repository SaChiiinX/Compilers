
  .text
  .globl our_code_starts_here
  our_code_starts_here:
  mov $42, %rax
  sub $1, %rax
  ret
  
  
