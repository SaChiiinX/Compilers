
  .text
  .globl our_code_starts_here
  
  our_code_starts_here:
  mov $7, %rax
  mov %rax, -8(%rsp)
  mov $1, %rax
  add -8(%rsp), %rax
  mov %rax, -8(%rsp)
  mov $3, %rax
  mov %rax, -16(%rsp)
  mov $1, %rax
  add -16(%rsp), %rax
  mov %rax, -16(%rsp)
  mov $5, %rax
  sub -16(%rsp), %rax
  add -8(%rsp), %rax
  ret
  
  
