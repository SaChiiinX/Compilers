
  .text
  .globl our_code_starts_here
  our_code_starts_here:
  mov $5, %rax
  mov %rax, -8(%rsp)
  mov $3, %rax
  mov %rax, -32(%rsp)
  mov -8(%rsp), %rax
  mov %rax, -48(%rsp)
  mov -32(%rsp), %rax
  add -48(%rsp), %rax
  mov %rax, -24(%rsp)
  mov -8(%rsp), %rax
  mov %rax, -40(%rsp)
  mov -24(%rsp), %rax
  imul -40(%rsp), %rax
  ret
  
  
