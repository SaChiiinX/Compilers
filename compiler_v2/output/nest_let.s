
  .text
  .globl our_code_starts_here
  our_code_starts_here:
  mov $5, %rax
  mov %rax, -8(%rsp)
  mov $3, %rax
  mov %rax, -16(%rsp)
  mov -16(%rsp), %rax
  mov $0, -24(%rsp)
  add -24(%rsp), %rax
  mov %rax, -24(%rsp)
  mov -8(%rsp), %rax
  add -24(%rsp), %rax
  mov %rax, -16(%rsp)
  mov -16(%rsp), %rax
  mov $1, -24(%rsp)
  imul -24(%rsp), %rax
  mov %rax, -24(%rsp)
  mov -8(%rsp), %rax
  imul -24(%rsp), %rax
  ret
  
  
