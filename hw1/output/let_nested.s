
  .text
  .globl our_code_starts_here
  our_code_starts_here:
  mov $5, %rax
  mov %rax, -16(%rsp)
  mov $10, %rax
  mov %rax, -24(%rsp)
  mov $20, %rax
  add -24(%rsp), %rax
  add -16(%rsp), %rax
  mov %rax, -8(%rsp)
  mov -8(%rsp), %rax
  mov %rax, -24(%rsp)
  mov -8(%rsp), %rax
  imul -24(%rsp), %rax
  ret
  
  
