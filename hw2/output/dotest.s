
  .text
  .globl our_code_starts_here
  our_code_starts_here:
  mov $0, %rax
  mov %rax, -8(%rsp)
  cond4:
  mov -8(%rsp), %rax
  add $1, %rax
  mov %rax, -8(%rsp)
  mov -8(%rsp), %rax
  mov %rax, -16(%rsp)
  mov $4, %rax
  cmp %rax, -16(%rsp)
  mov $0, %rax
  jge both3
  mov $1, %rax
  both3:
  cmp $0, %rax
  jne cond4
  mov -8(%rsp), %rax
  ret
  
  
