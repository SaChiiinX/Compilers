
  .text
  .globl our_code_starts_here
  our_code_starts_here:
  mov $2, %rax
  mov %rax, -16(%rsp)
  mov $3, %rax
  add -16(%rsp), %rax
  mov %rax, -8(%rsp)
  mov -8(%rsp), %rax
  mov %rax, -24(%rsp)
  mov -24(%rsp), %rax
  mov %rax, -40(%rsp)
  mov -8(%rsp), %rax
  mov %rax, -56(%rsp)
  mov -24(%rsp), %rax
  mov %rax, -64(%rsp)
  mov -40(%rsp), %rax
  sub %rax, -64(%rsp)
  mov -64(%rsp), %rax
  add -56(%rsp), %rax
  ret
  
  
