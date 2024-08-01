
  .text
  .globl our_code_starts_here
  our_code_starts_here:
  mov $10, %rax
  mov %rax, -8(%rsp)
  mov $3, %rax
  imul -8(%rsp), %rax
  ret
  
  
