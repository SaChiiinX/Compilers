
  .text
  .globl our_code_starts_here
  nested_four:
  mov -40(%rsp), %rax
  mov %rax, -48(%rsp)
  mov -32(%rsp), %rax
  add -48(%rsp), %rax
  mov %rax, -48(%rsp)
  mov -24(%rsp), %rax
  mov %rax, -56(%rsp)
  mov -16(%rsp), %rax
  imul -56(%rsp), %rax
  sub -48(%rsp), %rax
  ret
  
  our_code_starts_here:
  mov $after3, %rax
  mov %rax, -8(%rsp)
  mov %rsp, -16(%rsp)
  mov $2, %rax
  mov %rax, -24(%rsp)
  mov $3, %rax
  mov %rax, -32(%rsp)
  mov $4, %rax
  mov %rax, -40(%rsp)
  mov $5, %rax
  mov %rax, -48(%rsp)
  sub $8, %rsp
  jmp nested_four
  after3:
  mov -16(%rsp), %rsp
  ret
  
  
