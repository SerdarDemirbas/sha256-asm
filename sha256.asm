  .code32
  .section .data

cube_roots:
  .long 0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5, 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174, 0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da, 0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967, 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85, 0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070, 0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3, 0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2

chunks: # preprocessed message of "abc" is in the first 16
  .long 0x61626380, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000018, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000

hash_prev:
  .long 0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19

hash_curr:
  .long 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000

  .section .text
  .globl _start

_start:
  # Extend words
  movl $16, %edi
extend_words:
  cmpl $64, %edi
  je main_loop

  # sigma_1(chunks[i-2])
  movl %edi, %esi
  subl $2, %esi
  movl chunks(, %esi, 4), %eax
  pushl %edi
  pushl %eax
  call sigma_1
  addl $4, %esp
  popl %edi

  # sigma_0(chunks[i-15])
  movl %edi, %esi
  subl $15, %esi
  movl chunks(, %esi, 4), %ebx
  pushl %edi
  pushl %eax
  pushl %ebx
  call sigma_0
  addl $4, %esp
  popl %ebx
  popl %edi

  # result from sigma_0 is in %eax, and sigma_1 is in %ebx
  movl %edi, %esi
  subl $7, %esi
  movl chunks(, %esi, 4), %ecx
  addl %ecx, %ebx # sigma_1 + chunks[i-7]

  subl $9, %esi
  movl chunks(, %esi, 4), %ecx
  addl %ecx, %eax # sigma_0 + chunks[i-16]
  addl %ebx, %eax

  # same result
  movl %eax, chunks(, %edi, 4)

  incl %edi
  jmp extend_words

main_loop:
  # Copy initial hash into working hash
  movl $0, %edi
init_hash:
  movl hash_prev(, %edi, 4), %eax
  movl %eax, hash_curr(, %edi, 4)
  incl %edi
  cmpl $8, %edi
  jl init_hash

  # Hash all of the things
  movl $0, %edi
process_loop:
  pushl $cube_roots
  pushl $chunks
  pushl $hash_curr
  pushl %edi
  call iterate_hash
  popl %edi
  addl $12, %esp

  incl %edi
  cmpl $64, %edi
  jl process_loop

  # Add new hash values to previous hash
  movl $0, %edi
add_hash:
  movl hash_curr(, %edi, 4), %eax
  movl hash_prev(, %edi, 4), %ebx
  addl %ebx, %eax
  movl %eax, hash_prev(, %edi, 4)

  incl %edi
  cmpl $8, %edi
  jl add_hash

exit:
  movl $1, %eax
  movl $0, %ebx
  int $0x80

# Function sigma_0
#
# Performs the SHA-256 sigma 0 calculation:
#   rotateright(x, 7) ^ rotateright(x, 18) ^ shiftright(x, 3)
#
# Input:
#   First argument - value of x
sigma_0:
  pushl %ebp
  movl %esp, %ebp
  movl 8(%ebp), %eax

  movl %eax, %ebx
  rorl $7, %ebx
  movl %eax, %ecx
  rorl $18, %ecx
  xorl %ebx, %ecx
  shrl $3, %eax
  xorl %ecx, %eax

  movl %ebp, %esp
  popl %ebp
  ret

# Function sigma_1
#
# Performs the SHA-256 sigma 1 calculation:
#   rotateright(x, 17) ^ rotateright(x, 19) ^ shiftright(x, 10)
#
# Input:
#   First argument - value of x
sigma_1:
  pushl %ebp
  movl %esp, %ebp
  movl 8(%ebp), %eax

  movl %eax, %ebx
  rorl $17, %ebx
  movl %eax, %ecx
  rorl $19, %ecx
  xorl %ebx, %ecx
  shrl $10, %eax
  xorl %ecx, %eax

  movl %ebp, %esp
  popl %ebp
  ret

# Function Sigma_0
#
# Performs the SHA-256 Sigma 0 calculation:
#   rotateright(x, 2) ^ rotateright(x, 13) ^ rotateright(x, 22)
#
# Input:
#   First argument - value of x
Sigma_0:
  pushl %ebp
  movl %esp, %ebp
  movl 8(%ebp), %eax

  movl %eax, %ebx
  rorl $2, %ebx
  movl %eax, %ecx
  rorl $13, %ecx
  xorl %ebx, %ecx
  rorl $22, %eax
  xorl %ecx, %eax

  movl %ebp, %esp
  popl %ebp
  ret

# Function Sigma_1
#
# Performs the SHA-256 Sigma 1 calculation:
#   rotateright(x, 6) ^ rotateright(x, 11) ^ rotateright(x, 25)
#
# Input:
#   First argument - value of x
Sigma_1:
  pushl %ebp
  movl %esp, %ebp
  movl 8(%ebp), %eax

  movl %eax, %ebx
  rorl $6, %ebx
  movl %eax, %ecx
  rorl $11, %ecx
  xorl %ebx, %ecx
  rorl $25, %eax
  xorl %ecx, %eax

  movl %ebp, %esp
  popl %ebp
  ret

# Function Ch
#
# Performs the SHA-256 Ch calculation:
#   (x & y) ^ ((~x) & z)
#
# Input:
#   First argument  - value of x
#   Second argument - value of y
#   Third argument  - value of z
Ch:
  pushl %ebp
  movl %esp, %ebp
  movl 8(%ebp), %eax
  movl 12(%ebp), %ebx
  movl 16(%ebp), %ecx

  andl %eax, %ebx
  notl %eax
  andl %ecx, %eax
  xorl %ebx, %eax

  movl %ebp, %esp
  popl %ebp
  ret

# Function Maj
#
# Performs the SHA-256 Maj calculation:
#   (x & y) ^ (x & z) ^ (y & z)
#
# Input:
#   First argument  - value of x
#   Second argument - value of y
#   Third argument  - value of z
Maj:
  pushl %ebp
  movl %esp, %ebp
  movl 8(%ebp), %eax
  movl 12(%ebp), %ebx
  movl 16(%ebp), %ecx

  movl %eax, %edx
  andl %ebx, %eax # a = x & y
  andl %ecx, %edx # b = x & z
  xorl %edx, %eax # c = a ^ b
  andl %ebx, %ecx # d = y & z
  xorl %ecx, %eax # result = c ^ d

  movl %ebp, %esp
  pop %ebp
  ret

# Function T_1
#
# Calculate the formula:
#   h + Sigma_1(e) + Ch(e, f, g) + K[i] + W[i]
#
# Input:
#   First argument  - h
#   Second argument - e
#   Third argument  - f
#   Fourth argument - g
#   Fifth argument  - K[i]
#   Sixth argument  - W[i]
#
# Local variables:
#   total
T_1:
  pushl %ebp
  movl %esp, %ebp
  subl $4, %esp

  # get Sigma_1(e)
  movl 12(%ebp), %eax # e
  pushl %eax
  call Sigma_1
  addl $4, %esp

  # save result in total
  movl %eax, -4(%ebp)

  # get Ch(e, f, g)
  movl 20(%ebp), %eax # g
  pushl %eax
  movl 16(%ebp), %eax # f
  pushl %eax
  movl 12(%ebp), %eax # e
  pushl %eax
  call Ch
  addl $12, %esp

  # add result to total
  movl -4(%ebp), %ebx
  addl %ebx, %eax

  # add h
  movl 8(%ebp), %ebx
  addl %ebx, %eax

  # add K[i]
  movl 24(%ebp), %ebx
  addl %ebx, %eax

  # add W[i]
  movl 28(%ebp), %ebx
  addl %ebx, %eax

  # return
  movl %ebp, %esp
  pop %ebp
  ret

# Function T_2
#
# Calculate the formula:
#   Sigma_0(a) + Maj(a, b, c)
#
# Input:
#   First argument  - a
#   Second argument - b
#   Third argument  - c
#
# Local variables:
#   total
T_2:
  pushl %ebp
  movl %esp, %ebp
  subl $4, %esp

  # get Sigma_0(a)
  movl 8(%ebp), %eax # a
  pushl %eax
  call Sigma_0
  addl $4, %esp

  # save result in total
  movl %eax, -4(%ebp)

  # get Maj(e, f, g)
  movl 16(%ebp), %eax # c
  pushl %eax
  movl 12(%ebp), %eax # b
  pushl %eax
  movl 8(%ebp), %eax # a
  pushl %eax
  call Maj
  addl $12, %esp

  # add result to total
  movl -4(%ebp), %ebx
  addl %ebx, %eax

  # return
  movl %ebp, %esp
  pop %ebp
  ret

# Function iterate_hash
#
# Perform a round of SHA-256 hash iteration.
#
# Input:
#   First argument  - round index
#   Second argument - hash address
#   Third argument  - chunk address
#   Fourth argument - cube_roots address
#
# Local variables:
#   T_1 result
iterate_hash:
  pushl %ebp
  movl %esp, %ebp
  subl $4, %esp

  # Get T_1 result
  movl 8(%ebp), %edi # round index
  movl 12(%ebp), %eax # hash address
  movl 16(%ebp), %ebx # chunk address
  movl 20(%ebp), %ecx # cube_roots address

  movl (%ebx, %edi, 4), %edx # W[i]
  pushl %edx
  movl (%ecx, %edi, 4), %edx # K[i]
  pushl %edx
  movl 24(%eax), %edx # g
  pushl %edx
  movl 20(%eax), %edx # f
  pushl %edx
  movl 16(%eax), %edx # e
  pushl %edx
  movl 28(%eax), %edx # h
  pushl %edx
  call T_1
  addl $24, %esp

  # save T_1 result
  movl %eax, -4(%ebp)

  # get T_2 result
  movl 12(%ebp), %eax # hash address
  movl 8(%eax), %ebx # c
  pushl %ebx
  movl 4(%eax), %ebx # b
  pushl %ebx
  movl (%eax), %ebx # a
  pushl %ebx
  call T_2
  addl $12, %esp

  # get T_1 result from local variable
  movl -4(%ebp), %ebx

  # set hash values
  movl 12(%ebp), %ecx # hash address

  # h = g
  movl 24(%ecx), %edx # g
  movl %edx, 28(%ecx) # set h

  # g = f
  movl 20(%ecx), %edx # f
  movl %edx, 24(%ecx) # set g

  # f = e
  movl 16(%ecx), %edx # e
  movl %edx, 20(%ecx) # set f

  # e = d + T1
  movl 12(%ecx), %edx # d
  addl %ebx, %edx # d + T1
  movl %edx, 16(%ecx) # set e

  # d = c
  movl 8(%ecx), %edx # c
  movl %edx, 12(%ecx) # set d

  # c = b
  movl 4(%ecx), %edx # b
  movl %edx, 8(%ecx) # set c

  # b = a
  movl (%ecx), %edx # a
  movl %edx, 4(%ecx) # set b

  # a = T1 + T2
  addl %ebx, %eax
  movl %eax, (%ecx)

  # return
  movl $0, %eax
  movl %ebp, %esp
  pop %ebp
  ret
