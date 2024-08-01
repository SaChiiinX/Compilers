
  .text
  .globl our_code_starts_here
  our_code_starts_here:
  mov $5, %rax
  mov %rax, -8(%rsp)
  mov $6, %rax
  imul -8(%rsp), %rax
  mov %rax, -8(%rsp)
  mov $2, %rax
  sub %rax, -8(%rsp)
  mov -8(%rsp), %rax
  ret
  
  
