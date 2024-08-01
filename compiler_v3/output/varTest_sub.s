
  .text
  .globl our_code_starts_here
  
  our_code_starts_here:
  mov $5, %rax
  mov %rax, -8(%rsp)
  mov $2, %rax
  mov %rax, -16(%rsp)
  mov -16(%rsp), %rax
  mov %rax, -24(%rsp)
  mov $2, %rax
  mov %rax, -32(%rsp)
  mov -8(%rsp), %rax
  imul -32(%rsp), %rax
  sub -24(%rsp), %rax
  ret
  
  
