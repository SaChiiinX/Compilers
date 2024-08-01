
  .text
  .globl our_code_starts_here
  our_code_starts_here:
  mov $5, %rax
  mov %rax, -8(%rsp)
  mov -8(%rsp), %rax
  mov %rax, -16(%rsp)
  mov $7, %rax
  cmp %rax, -16(%rsp)
  mov $0, %rax
  jne both0
  mov $1, %rax
  both0:
  cmp $0, %rax
  je else1
  mov $7, %rax
  jmp both2
  else1:
  mov $8, %rax
  both2:
  ret
  
  
