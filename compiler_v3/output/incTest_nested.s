
  .text
  .globl our_code_starts_here
  
  our_code_starts_here:
  mov $8, %rax
  add $1, %rax
  add $1, %rax
  ret
  
  
