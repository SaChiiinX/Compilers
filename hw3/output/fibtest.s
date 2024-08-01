
  .text
  .globl our_code_starts_here
  fib:
  mov -16(%rsp), %rax
  mov %rax, -24(%rsp)
  mov $0, %rax
  cmp %rax, -24(%rsp)
  mov $0, %rax
  jne both0
  mov $1, %rax
  both0:
  cmp $0, %rax
  je else6
  mov $0, %rax
  jmp both7
  else6:
  mov -16(%rsp), %rax
  mov %rax, -24(%rsp)
  mov $1, %rax
  cmp %rax, -24(%rsp)
  mov $0, %rax
  jne both1
  mov $1, %rax
  both1:
  cmp $0, %rax
  je else4
  mov $1, %rax
  jmp both5
  else4:
  mov $after3, %rax
  mov %rax, -24(%rsp)
  mov %rsp, -32(%rsp)
  mov $2, %rax
  mov %rax, -40(%rsp)
  mov -16(%rsp), %rax
  sub -40(%rsp), %rax
  mov %rax, -40(%rsp)
  sub $24, %rsp
  jmp fib
  after3:
  mov -16(%rsp), %rsp
  mov %rax, -24(%rsp)
  mov $after2, %rax
  mov %rax, -32(%rsp)
  mov %rsp, -40(%rsp)
  mov $1, %rax
  mov %rax, -48(%rsp)
  mov -16(%rsp), %rax
  sub -48(%rsp), %rax
  mov %rax, -48(%rsp)
  sub $32, %rsp
  jmp fib
  after2:
  mov -16(%rsp), %rsp
  add -24(%rsp), %rax
  both5:
  both7:
  ret
  
  our_code_starts_here:
  mov $after8, %rax
  mov %rax, -8(%rsp)
  mov %rsp, -16(%rsp)
  mov $4, %rax
  mov %rax, -24(%rsp)
  sub $8, %rsp
  jmp fib
  after8:
  mov -16(%rsp), %rsp
  ret
  
  
