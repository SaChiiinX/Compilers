
  .text
  .globl our_code_starts_here
  abs_val:
  mov -16(%rsp), %rax
  mov %rax, -24(%rsp)
  mov $0, %rax
  cmp %rax, -24(%rsp)
  mov $0, %rax
  jge both0
  mov $1, %rax
  both0:
  cmp $0, %rax
  je else1
  mov -16(%rsp), %rax
  mov %rax, -24(%rsp)
  mov $-1, %rax
  imul -24(%rsp), %rax
  jmp both2
  else1:
  mov -16(%rsp), %rax
  both2:
  ret
  
  our_code_starts_here:
  mov $after3, %rax
  mov %rax, -8(%rsp)
  mov %rsp, -16(%rsp)
  mov $-3, %rax
  mov %rax, -24(%rsp)
  sub $8, %rsp
  jmp abs_val
  after3:
  mov -16(%rsp), %rsp
  ret
  
  
