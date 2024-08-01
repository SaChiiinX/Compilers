
  .text
  .globl our_code_starts_here
  our_code_starts_here:
  mov $2, %rax
  mov %rax, -8(%rsp)
  mov $5, %rax
  mov %rax, -16(%rsp)
  mov $3, %rax
  sub %rax, -16(%rsp)
  mov -16(%rsp), %rax
  imul -8(%rsp), %rax
  mov %rax, -8(%rsp)
  mov $10, %rax
  add -8(%rsp), %rax
  ret
  
  
