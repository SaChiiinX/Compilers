
  .text
  .globl our_code_starts_here
  our_code_starts_here:
  mov $-1, %rax
  mov %rax, -8(%rsp)
  mov $-8, %rax
  mov %rax, -24(%rsp)
  mov $5, %rax
  mov %rax, -40(%rsp)
  mov -24(%rsp), %rax
  mov %rax, -56(%rsp)
  mov -40(%rsp), %rax
  add -56(%rsp), %rax
  ret
  
  
