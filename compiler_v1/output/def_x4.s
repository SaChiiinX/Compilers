
  .text
  .globl our_code_starts_here
  our_code_starts_here:
  mov $5, %rax
  mov %rax, -16(%rsp)
  mov -16(%rsp), %rax
  sub $1, %rax
  mov %rax, -8(%rsp)
  mov -8(%rsp), %rax
  sub $1, %rax
  ret
  
  
