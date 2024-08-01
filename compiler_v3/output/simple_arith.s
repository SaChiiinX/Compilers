
  .text
  .globl our_code_starts_here
  
  our_code_starts_here:
  mov $4, %rax
  mov %rax, -8(%rsp)
  mov $3, %rax
  imul -8(%rsp), %rax
  mov %rax, -8(%rsp)
  mov $7, %rax
  add -8(%rsp), %rax
  ret
  
  
