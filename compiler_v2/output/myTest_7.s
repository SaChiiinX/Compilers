
  .text
  .globl our_code_starts_here
  our_code_starts_here:
  mov $7, %rax
  mov $0, -8(%rsp)
  add -8(%rsp), %rax
  mov %rax, -8(%rsp)
  mov $5, %rax
  add -8(%rsp), %rax
  ret
  
  
