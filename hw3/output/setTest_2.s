
  .text
  .globl our_code_starts_here
  
  our_code_starts_here:
  mov $5, %rax
  mov %rax, -8(%rsp)
  mov $10, %rax
  mov %rax, -16(%rsp)
  mov $1, %rax
  mov %rax, -8(%rsp)
  mov -16(%rsp), %rax
  mov %rax, -32(%rsp)
  mov -8(%rsp), %rax
  add -32(%rsp), %rax
  mov %rax, -16(%rsp)
  mov -16(%rsp), %rax
  ret
  
  
