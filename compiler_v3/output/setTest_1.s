
  .text
  .globl our_code_starts_here
  
  our_code_starts_here:
  mov $5, %rax
  mov %rax, -8(%rsp)
  mov $10, %rax
  mov %rax, -8(%rsp)
  ret
  
  
