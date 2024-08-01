
  .text
  .globl our_code_starts_here
  our_code_starts_here:
  mov $5, %rax
  mov %rax, -8(%rsp)
  mov -8(%rsp), %rax
  mov %rax, -16(%rsp)
  mov $5, %rax
  cmp %rax, -16(%rsp)
  mov $0, %rax
  jne both3
  mov $1, %rax
  both3:
  cmp $0, %rax
  je else4
  mov $1, %rax
  jmp both5
  else4:
  mov $0, %rax
  both5:
  ret
  
  
