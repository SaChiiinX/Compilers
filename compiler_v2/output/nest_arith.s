
  .text
  .globl our_code_starts_here
  our_code_starts_here:
  mov $2, %rax
  mov $0, -8(%rsp)
  add -8(%rsp), %rax
  mov %rax, -8(%rsp)
  mov $6, %rax
  mov $1, -16(%rsp)
  imul -16(%rsp), %rax
  mov %rax, -16(%rsp)
  mov $5, %rax
  imul -16(%rsp), %rax
  sub -8(%rsp), %rax
  ret
  
  
