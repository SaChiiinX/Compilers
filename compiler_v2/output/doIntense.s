
  .text
  .globl our_code_starts_here
  our_code_starts_here:
  mov $2, %rax
  mov %rax, -8(%rsp)
  mov $5, %rax
  mov %rax, -16(%rsp)
  cond1:
  mov -8(%rsp), %rax
  add $1, %rax
  mov %rax, -8(%rsp)
  mov -16(%rsp), %rax
  sub $1, %rax
  mov %rax, -16(%rsp)
  mov -8(%rsp), %rax
  mov %rax, -24(%rsp)
  mov -16(%rsp), %rax
  cmp %rax, -24(%rsp)
  mov $0, %rax
  jge both0
  mov $1, %rax
  both0:
  cmp $0, %rax
  jne cond1
  mov -16(%rsp), %rax
  mov $0, -32(%rsp)
  add -32(%rsp), %rax
  mov %rax, -32(%rsp)
  mov -8(%rsp), %rax
  add -32(%rsp), %rax
  ret
  
  
