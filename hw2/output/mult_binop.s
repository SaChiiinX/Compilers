
  .text
  .globl our_code_starts_here
  our_code_starts_here:
  mov $2, %rax
  mov $0, -8(%rsp)
  add -8(%rsp), %rax
  mov %rax, -8(%rsp)
  mov $3, %rax
  mov $0, -8(%rsp)
  add -8(%rsp), %rax
  mov %rax, -8(%rsp)
  mov $4, %rax
  mov $0, -8(%rsp)
  add -8(%rsp), %rax
  mov %rax, -8(%rsp)
  mov $5, %rax
  mov $0, -8(%rsp)
  add -8(%rsp), %rax
  mov %rax, -8(%rsp)
  mov $1, %rax
  add -8(%rsp), %rax
  ret
  
  
