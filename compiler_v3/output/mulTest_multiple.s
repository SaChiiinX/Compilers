
  .text
  .globl our_code_starts_here
  
  our_code_starts_here:
  mov $3, %rax
  mov %rax, -8(%rsp)
  mov $2, %rax
  imul -8(%rsp), %rax
  mov %rax, -8(%rsp)
  mov $1, %rax
  imul -8(%rsp), %rax
  mov %rax, -8(%rsp)
  mov $10, %rax
  imul -8(%rsp), %rax
  ret
  
  
