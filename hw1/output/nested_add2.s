
  .text
  .globl our_code_starts_here
  our_code_starts_here:
  mov $10, %rax
  mov %rax, -8(%rsp)
  mov $5, %rax
  sub %rax, -8(%rsp)
  mov -8(%rsp), %rax
  mov %rax, -8(%rsp)
  mov $20, %rax
  add -8(%rsp), %rax
  ret
  
  
