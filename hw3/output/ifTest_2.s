
  .text
  .globl our_code_starts_here
  
  our_code_starts_here:
  mov $1, %rax
  cmp $0, %rax
  je else0
  mov $42, %rax
  jmp both1
  else0:
  mov $0, %rax
  both1:
  ret
  
  
