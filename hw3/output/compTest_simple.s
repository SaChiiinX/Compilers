
  .text
  .globl our_code_starts_here
  
  our_code_starts_here:
  mov $3, %rax
  mov %rax, -8(%rsp)
  mov $6, %rax
  cmp %rax, -8(%rsp)
  mov $0, %rax
  jge both14
  mov $1, %rax
  both14:
  ret
  
  
