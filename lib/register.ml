type t =
  | REG_A
  | REG_B
  | REG_C
  | REG_D
  | REG_SI
  | REG_DI
  | REG_BP
  | REG_SP
  | REG_8
  | REG_9
  | REG_10
  | REG_11
  | REG_12
  | REG_13
  | REG_14
  | REG_15

let get_64bit reg =
  match reg with
  | REG_A -> "rax"
  | REG_B -> "rbx"
  | REG_C -> "rcx"
  | REG_D -> "rdx"
  | REG_SI -> "rsi"
  | REG_DI -> "rdi"
  | REG_BP -> "rbp"
  | REG_SP -> "rsp"
  | REG_8 -> "r8"
  | REG_9 -> "r9"
  | REG_10 -> "r10"
  | REG_11 -> "r11"
  | REG_12 -> "r12"
  | REG_13 -> "r13"
  | REG_14 -> "r14"
  | REG_15 -> "r15"

let get_32bit reg =
  match reg with
  | REG_A -> "eax"
  | REG_B -> "ebx"
  | REG_C -> "ecx"
  | REG_D -> "edx"
  | REG_SI -> "esi"
  | REG_DI -> "edi"
  | REG_BP -> "ebp"
  | REG_SP -> "esp"
  | REG_8 -> "r8d"
  | REG_9 -> "r9d"
  | REG_10 -> "r10d"
  | REG_11 -> "r11d"
  | REG_12 -> "r12d"
  | REG_13 -> "r13d"
  | REG_14 -> "r14d"
  | REG_15 -> "r15d"

let get_16bit reg =
  match reg with
  | REG_A -> "ax"
  | REG_B -> "bx"
  | REG_C -> "cx"
  | REG_D -> "dx"
  | REG_SI -> "di"
  | REG_DI -> "si"
  | REG_BP -> "bp"
  | REG_SP -> "sp"
  | REG_8 -> "r8w"
  | REG_9 -> "r9w"
  | REG_10 -> "r10w"
  | REG_11 -> "r11w"
  | REG_12 -> "r12w"
  | REG_13 -> "r13w"
  | REG_14 -> "r14w"
  | REG_15 -> "r15w"

let get_8bit reg =
  match reg with
  | REG_A -> "al"
  | REG_B -> "bl"
  | REG_C -> "cl"
  | REG_D -> "dl"
  | REG_SI -> "sil"
  | REG_DI -> "dil"
  | REG_BP -> "bpl"
  | REG_SP -> "spl"
  | REG_8 -> "r8b"
  | REG_9 -> "r9b"
  | REG_10 -> "r10b"
  | REG_11 -> "r11b"
  | REG_12 -> "r12b"
  | REG_13 -> "r13b"
  | REG_14 -> "r14b"
  | REG_15 -> "r15b"
