
  .text
  .globl our_code_starts_here
  our_code_starts_here:
  mov $3, %rax
  mov %rax, -16(%rsp)
  mov -16(%rsp), %rax
  add $1, %rax
  mov %rax, -8(%rsp)
  mov $6, %rax
  mov %rax, -32(%rsp)
  mov -32(%rsp), %rax
  sub $1, %rax
  mov %rax, -24(%rsp)
  mov -8(%rsp), %rax
  mov %rax, -40(%rsp)
  mov -24(%rsp), %rax
  imul -40(%rsp), %rax
  ret
  
  
