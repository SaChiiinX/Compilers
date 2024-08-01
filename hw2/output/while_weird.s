
  .text
  .globl our_code_starts_here
  our_code_starts_here:
  mov $5, %rax
  mov %rax, -8(%rsp)
  cond1:
  mov -8(%rsp), %rax
  mov %rax, -16(%rsp)
  mov $5, %rax
  cmp %rax, -16(%rsp)
  mov $0, %rax
  jge both0
  mov $1, %rax
  both0:
  cmp $0, %rax
  je break2
  mov -8(%rsp), %rax
  add $1, %rax
  mov %rax, -8(%rsp)
  mov $1, %rax
  mov %rax, -32(%rsp)
  mov $1, %rax
  cmp %rax, -32(%rsp)
  mov $0, %rax
  jne both3
  mov $1, %rax
  both3:
  jmp cond1
  break2:
  mov -8(%rsp), %rax
  ret
  
  
