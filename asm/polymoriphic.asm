lhu x1, 0(x0)

sub x1, x1, x1

addi x2, x1, -1
addi x3, x1, 1

addi x4, x0, -1
addi x5, x0, 1

slt x8, x2, x3
slt x9, x4, x5

    add     x1, x0, x0          # x1(tag=0) = 0

    ################################################################
    # LOADS (base = x0, absolute address = imm). We'll read from
    # addresses that happen to contain code bytes/words—data value
    # irrelevant; we just want the resulting *tags* from the loader.
    ################################################################
    # Word-tag path (LW/LWU => tags 0b0100 / 0b0101)
    lw      x2, 0(x0)           # x2(tag=0b0100) = mem[0]
    lw      x3, 4(x0)           # x3(tag=0b0100) = mem[4]

    add     x4, x2, x3          # OK: tags equal (0b0100)
    sub     x5, x4, x2          # still tag 0b0100
    sll     x6, x5, x1          # shift by 0, preserves tag (0b0100)
    srl     x7, x6, x1          # shift by 0, preserves tag (0b0100)
    sra     x8, x7, x1          # shift by 0, preserves tag (0b0100)
    slt     x9, x2, x3          # OK: tags equal, result tag becomes 0

    # Byte signed/unsigned (LB/LBU => tags 0b0000 / 0b0001)
    lb      x10, 0(x0)          # x10(tag=0b0000)
    lb      x11, 1(x0)          # x11(tag=0b0000)
    add     x12, x10, x11       # OK: 0b0000

    lbu     x13, 0(x0)          # x13(tag=0b0001)
    lbu     x14, 1(x0)          # x14(tag=0b0001)
    xor     x15, x13, x14       # OK: 0b0001

    # Halfword signed/unsigned (LH/LHU => tags 0b0010 / 0b0011)
    lh      x16, 0(x0)          # x16(tag=0b0010)
    lh      x17, 2(x0)          # x17(tag=0b0010)
    or      x18, x16, x17       # OK: 0b0010

    lhu     x19, 0(x0)          # x19(tag=0b0011)
    lhu     x20, 2(x0)          # x20(tag=0b0011)
    and     x21, x19, x20       # OK: 0b0011
