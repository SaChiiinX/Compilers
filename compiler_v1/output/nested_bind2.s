
  .text
  .globl our_code_starts_here
  our_code_starts_here:
  mov $3, %rax
  mov %rax, -16(%rsp)
  mov $4, %rax
  add -16(%rsp), %rax
  mov %rax, -8(%rsp)
  mov -8(%rsp), %rax
  add $1, %rax
  mov %rax, -24(%rsp)
  mov -8(%rsp), %rax
  mov %rax, -48(%rsp)
  mov -24(%rsp), %rax
  add -48(%rsp), %rax
  mov %rax, -40(%rsp)
  mov -40(%rsp), %rax
  mov %rax, -56(%rsp)
  mov -40(%rsp), %rax
  add -56(%rsp), %rax
  ret
  
  
