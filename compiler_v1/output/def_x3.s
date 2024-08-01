
  .text
  .globl our_code_starts_here
  our_code_starts_here:
  mov $5, %rax
  mov %rax, -8(%rsp)
  mov $67, %rax
  mov %rax, -24(%rsp)
  mov -24(%rsp), %rax
  sub $1, %rax
  ret
  
  
