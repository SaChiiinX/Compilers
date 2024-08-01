
  .text
  .globl our_code_starts_here
  rem:
  mov -16(%rsp), %rax
  mov %rax, -32(%rsp)
  mov -24(%rsp), %rax
  cmp %rax, -32(%rsp)
  mov $0, %rax
  jge both9
  mov $1, %rax
  both9:
  cmp $0, %rax
  je else11
  mov -16(%rsp), %rax
  jmp both12
  else11:
  mov $after10, %rax
  mov %rax, -32(%rsp)
  mov %rsp, -40(%rsp)
  mov -24(%rsp), %rax
  mov %rax, -48(%rsp)
  mov -16(%rsp), %rax
  sub -48(%rsp), %rax
  mov %rax, -48(%rsp)
  mov -24(%rsp), %rax
  mov %rax, -56(%rsp)
  sub $32, %rsp
  jmp rem
  after10:
  mov -16(%rsp), %rsp
  both12:
  ret
  helper:
  mov -24(%rsp), %rax
  mov %rax, -32(%rsp)
  mov -16(%rsp), %rax
  cmp %rax, -32(%rsp)
  mov $0, %rax
  jge both1
  mov $1, %rax
  both1:
  cmp $0, %rax
  je else7
  mov $after2, %rax
  mov %rax, -32(%rsp)
  mov %rsp, -40(%rsp)
  mov -16(%rsp), %rax
  mov %rax, -48(%rsp)
  mov -24(%rsp), %rax
  mov %rax, -56(%rsp)
  sub $32, %rsp
  jmp rem
  after2:
  mov -16(%rsp), %rsp
  mov %rax, -32(%rsp)
  mov $0, %rax
  cmp %rax, -32(%rsp)
  mov $0, %rax
  jne both3
  mov $1, %rax
  both3:
  cmp $0, %rax
  je else5
  mov $0, %rax
  jmp both6
  else5:
  mov $after4, %rax
  mov %rax, -32(%rsp)
  mov %rsp, -40(%rsp)
  mov -16(%rsp), %rax
  mov %rax, -48(%rsp)
  mov -24(%rsp), %rax
  add $1, %rax
  mov %rax, -56(%rsp)
  sub $32, %rsp
  jmp helper
  after4:
  mov -16(%rsp), %rsp
  both6:
  jmp both8
  else7:
  mov $1, %rax
  both8:
  ret
  isPrime:
  mov $after0, %rax
  mov %rax, -24(%rsp)
  mov %rsp, -32(%rsp)
  mov -16(%rsp), %rax
  mov %rax, -40(%rsp)
  mov $2, %rax
  mov %rax, -48(%rsp)
  sub $24, %rsp
  jmp helper
  after0:
  mov -16(%rsp), %rsp
  ret
  
  our_code_starts_here:
  mov $after13, %rax
  mov %rax, -8(%rsp)
  mov %rsp, -16(%rsp)
  mov $12, %rax
  mov %rax, -24(%rsp)
  sub $8, %rsp
  jmp isPrime
  after13:
  mov -16(%rsp), %rsp
  ret
  
  
