
  .text
  .globl our_code_starts_here
  our_code_starts_here:
  mov $3, %rax
  mov $1, -8(%rsp)
  imul -8(%rsp), %rax
  mov %rax, -8(%rsp)
  mov $10, %rax
  imul -8(%rsp), %rax
  ret
  
  
