
  .text
  .globl our_code_starts_here
  our_code_starts_here:
  mov $10, %rax
  mov %rax, -8(%rsp)
  mov $10, %rax
  mov %rax, -16(%rsp)
  mov $6, %rax
  mov %rax, -24(%rsp)
  mov -24(%rsp), %rax
  sub $1, %rax
  sub %rax, -16(%rsp)
  mov -16(%rsp), %rax
  sub %rax, -8(%rsp)
  mov -8(%rsp), %rax
  mov %rax, -8(%rsp)
  mov $10, %rax
  mov %rax, -16(%rsp)
  mov $10, %rax
  mov %rax, -24(%rsp)
  mov $5, %rax
  sub %rax, -24(%rsp)
  mov -24(%rsp), %rax
  sub %rax, -16(%rsp)
  mov -16(%rsp), %rax
  add -8(%rsp), %rax
  ret
  
  
