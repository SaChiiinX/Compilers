
  .text
  .globl our_code_starts_here
  recursive_product_four:
  mov -16(%rsp), %rax
  mov %rax, -48(%rsp)
  mov $0, %rax
  cmp %rax, -48(%rsp)
  mov $0, %rax
  jne both2
  mov $1, %rax
  both2:
  cmp $0, %rax
  je else4
  mov $1, %rax
  jmp both5
  else4:
  mov $after3, %rax
  mov %rax, -48(%rsp)
  mov %rsp, -56(%rsp)
  mov $1, %rax
  mov %rax, -64(%rsp)
  mov -16(%rsp), %rax
  sub -64(%rsp), %rax
  mov %rax, -64(%rsp)
  mov -24(%rsp), %rax
  mov %rax, -72(%rsp)
  mov -32(%rsp), %rax
  mov %rax, -80(%rsp)
  mov -40(%rsp), %rax
  mov %rax, -88(%rsp)
  sub $48, %rsp
  jmp recursive_product_four
  after3:
  mov -16(%rsp), %rsp
  mov %rax, -48(%rsp)
  mov -16(%rsp), %rax
  imul -48(%rsp), %rax
  both5:
  ret
  
  our_code_starts_here:
  mov $after6, %rax
  mov %rax, -8(%rsp)
  mov %rsp, -16(%rsp)
  mov $3, %rax
  mov %rax, -24(%rsp)
  mov $2, %rax
  mov %rax, -32(%rsp)
  mov $3, %rax
  mov %rax, -40(%rsp)
  mov $1, %rax
  mov %rax, -48(%rsp)
  sub $8, %rsp
  jmp recursive_product_four
  after6:
  mov -16(%rsp), %rsp
  ret
  
  
