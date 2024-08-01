
  .text
  .globl our_code_starts_here
  even:
  mov -16(%rsp), %rax
  mov %rax, -24(%rsp)
  mov $0, %rax
  cmp %rax, -24(%rsp)
  mov $0, %rax
  jne both4
  mov $1, %rax
  both4:
  cmp $0, %rax
  je else6
  mov $1, %rax
  jmp both7
  else6:
  mov $after5, %rax
  mov %rax, -24(%rsp)
  mov %rsp, -32(%rsp)
  mov $1, %rax
  mov %rax, -40(%rsp)
  mov -16(%rsp), %rax
  sub -40(%rsp), %rax
  mov %rax, -40(%rsp)
  sub $24, %rsp
  jmp odd
  after5:
  mov -16(%rsp), %rsp
  both7:
  ret
  odd:
  mov -16(%rsp), %rax
  mov %rax, -24(%rsp)
  mov $0, %rax
  cmp %rax, -24(%rsp)
  mov $0, %rax
  jne both0
  mov $1, %rax
  both0:
  cmp $0, %rax
  je else2
  mov $0, %rax
  jmp both3
  else2:
  mov $after1, %rax
  mov %rax, -24(%rsp)
  mov %rsp, -32(%rsp)
  mov $1, %rax
  mov %rax, -40(%rsp)
  mov -16(%rsp), %rax
  sub -40(%rsp), %rax
  mov %rax, -40(%rsp)
  sub $24, %rsp
  jmp even
  after1:
  mov -16(%rsp), %rsp
  both3:
  ret
  
  our_code_starts_here:
  mov $after8, %rax
  mov %rax, -8(%rsp)
  mov %rsp, -16(%rsp)
  mov $8, %rax
  mov %rax, -24(%rsp)
  sub $8, %rsp
  jmp even
  after8:
  mov -16(%rsp), %rsp
  ret
  
  
