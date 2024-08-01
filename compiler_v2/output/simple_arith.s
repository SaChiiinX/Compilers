
  .text
  .globl our_code_starts_here
  our_code_starts_here:
  mov $4, %rax
  mov $1, -8(%rsp)
  imul -8(%rsp), %rax
  mov %rax, -8(%rsp)
  mov $3, %rax
  imul -8(%rsp), %rax
  mov $0, -8(%rsp)
  add -8(%rsp), %rax
  mov %rax, -8(%rsp)
  mov $7, %rax
  add -8(%rsp), %rax
  ret
  
  
