
  .text
  .globl our_code_starts_here
  our_code_starts_here:
  mov $1, %rax
  mov %rax, -8(%rsp)
  mov $2, %rax
  mov %rax, -8(%rsp)
  mov -8(%rsp), %rax
  ret
  
  
