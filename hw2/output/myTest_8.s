
  .text
  .globl our_code_starts_here
  our_code_starts_here:
  mov $3, %rax
  mov %rax, -8(%rsp)
  mov $4, %rax
  mov %rax, -16(%rsp)
  mov -16(%rsp), %rax
  mov $0, -24(%rsp)
  add -24(%rsp), %rax
  mov %rax, -24(%rsp)
  mov -8(%rsp), %rax
  add -24(%rsp), %rax
  ret
  
  
