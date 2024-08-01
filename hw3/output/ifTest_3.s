
  .text
  .globl our_code_starts_here
  
  our_code_starts_here:
  mov $5, %rax
  mov %rax, -8(%rsp)
  mov -8(%rsp), %rax
  add $1, %rax
  mov -8(%rsp), %rax
  mov %rax, -24(%rsp)
  mov $5, %rax
  cmp %rax, -24(%rsp)
  mov $0, %rax
  jne both0
  mov $1, %rax
  both0:
  cmp $0, %rax
  je else1
  mov $1, %rax
  jmp both2
  else1:
  mov $0, %rax
  both2:
  ret
  
  
