
  .text
  .globl our_code_starts_here
  our_code_starts_here:
  mov $7, %rax
  mov %rax, -8(%rsp)
  mov $3, %rax
  mov %rax, -24(%rsp)
  mov -8(%rsp), %rax
  mov %rax, -40(%rsp)
  mov -24(%rsp), %rax
  add -40(%rsp), %rax
  ret
  
  
