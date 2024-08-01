
  .text
  .globl our_code_starts_here
  our_code_starts_here:
  mov $1, %rax
  cmp $0, %rax
  je else0
  mov $5, %rax
  jmp both1
  else0:
  mov $6, %rax
  both1:
  ret
  
  
