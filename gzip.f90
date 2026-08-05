module gzip_mod
    ! Self-contained gzip (RFC 1952) / DEFLATE (RFC 1951) codec.
    !
    ! No external libraries, no ISO_C_BINDING, no platform DLLs - only
    ! standard Fortran, so this compiles unchanged with gfortran or ifort.
    !
    ! Byte payloads are integer(1) arrays. Byte values are handled as
    ! unsigned 0..255 via IAND(INT(b,4),255) on the way in, and stored
    ! back by relying on the two's-complement wraparound of INT(v,1) for
    ! v in 0..255 (true on every mainstream twos-complement Fortran target,
    ! including gfortran and ifort on x86/x64).
    implicit none
    private

    public :: gzip_compress
    public :: gzip_uncompress
    public :: gzip_compress_file
    public :: gzip_uncompress_file

    integer, parameter :: MAXBITS = 15
    integer(8), parameter :: M32 = 4294967295_8   ! 2^32 - 1, i.e. 0xFFFFFFFF

    ! canonical Huffman decode table: symbols grouped/sorted by code length
    type :: huff_t
        integer :: count(0:MAXBITS) = 0
        integer, allocatable :: symbol(:)
    end type huff_t

    ! LSB-first bit reader over a byte buffer
    type :: bitreader_t
        integer(1), allocatable :: buf(:)
        integer(8) :: nbytes = 0
        integer(8) :: bytepos = 1
        integer :: bitpos = 0
    end type bitreader_t

    ! bit writer: normal fields LSB-first, Huffman codes written MSB-first
    type :: bitwriter_t
        integer(1), allocatable :: buf(:)
        integer(8) :: cap = 0
        integer(8) :: bytepos = 1
        integer :: bitpos = 0
    end type bitwriter_t

    ! length/distance base values and extra-bit counts, RFC1951 3.2.5
    integer, parameter :: LBASE(0:28) = [3,4,5,6,7,8,9,10,11,13,15,17,19,23,27,31,35,43,51, &
                                          59,67,83,99,115,131,163,195,227,258]
    integer, parameter :: LEXT(0:28)  = [0,0,0,0,0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,0]
    integer, parameter :: DBASE(0:29) = [1,2,3,4,5,7,9,13,17,25,33,49,65,97,129,193,257,385,513,769, &
                                         1025,1537,2049,3073,4097,6145,8193,12289,16385,24577]
    integer, parameter :: DEXT(0:29)  = [0,0,0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,13,13]

    ! order in which dynamic-block code-length-code lengths are transmitted
    integer, parameter :: CLC_ORDER(0:18) = [16,17,18,0,8,7,9,6,10,5,11,4,12,3,13,2,14,1,15]

contains

    ! =====================================================================
    ! CRC32 (RFC 1952 trailer), bit-by-bit reflected algorithm, poly 0xEDB88320
    ! =====================================================================
    function crc32(data) result(crc)
        integer(1), intent(in) :: data(:)
        integer(8) :: crc
        integer(8) :: c, poly
        integer :: i, j, b

        poly = 3988292384_8   ! 0xEDB88320, reflected CRC-32 polynomial
        c = M32
        do i = 1, size(data)
            b = iand(int(data(i), 4), 255)
            c = ieor(c, int(b, 8))
            do j = 1, 8
                if (iand(c, 1_8) == 1_8) then
                    c = ieor(ishft(c, -1), poly)
                else
                    c = ishft(c, -1)
                end if
            end do
        end do
        crc = iand(ieor(c, M32), M32)
    end function crc32

    ! =====================================================================
    ! Bit reader (LSB-first), used to parse DEFLATE streams
    ! =====================================================================
    subroutine br_init(br, bytes)
        type(bitreader_t), intent(out) :: br
        integer(1), intent(in) :: bytes(:)
        allocate (br%buf(size(bytes)))
        br%buf = bytes
        br%nbytes = size(bytes, kind=8)
        br%bytepos = 1
        br%bitpos = 0
    end subroutine br_init

    function br_getbit(br) result(bit)
        type(bitreader_t), intent(inout) :: br
        integer :: bit
        integer :: byteval
        byteval = iand(int(br%buf(br%bytepos), 4), 255)
        bit = iand(ishft(byteval, -br%bitpos), 1)
        br%bitpos = br%bitpos + 1
        if (br%bitpos == 8) then
            br%bitpos = 0
            br%bytepos = br%bytepos + 1
        end if
    end function br_getbit

    function br_getbits(br, n) result(v)
        type(bitreader_t), intent(inout) :: br
        integer, intent(in) :: n
        integer :: v
        integer :: i
        v = 0
        do i = 0, n - 1
            v = ior(v, ishft(br_getbit(br), i))
        end do
    end function br_getbits

    subroutine br_align(br)
        type(bitreader_t), intent(inout) :: br
        if (br%bitpos /= 0) then
            br%bitpos = 0
            br%bytepos = br%bytepos + 1
        end if
    end subroutine br_align

    ! =====================================================================
    ! Bit writer: bw_putbits (LSB-first) for headers/extra-bits,
    ! bw_puthuff (MSB-first) for Huffman codes, per RFC1951 3.1.1
    ! =====================================================================
    subroutine bw_init(bw, initial_cap)
        type(bitwriter_t), intent(out) :: bw
        integer(8), intent(in) :: initial_cap
        bw%cap = max(initial_cap, 64_8)
        allocate (bw%buf(bw%cap))
        bw%buf = 0_1
        bw%bytepos = 1
        bw%bitpos = 0
    end subroutine bw_init

    subroutine bw_ensure(bw)
        type(bitwriter_t), intent(inout) :: bw
        integer(1), allocatable :: tmp(:)
        integer(8) :: newcap
        if (bw%bytepos <= bw%cap) return
        newcap = bw%cap * 2_8
        allocate (tmp(newcap))
        tmp = 0_1
        tmp(1:bw%cap) = bw%buf(1:bw%cap)
        call move_alloc(tmp, bw%buf)
        bw%cap = newcap
    end subroutine bw_ensure

    subroutine bw_putbit(bw, bit)
        type(bitwriter_t), intent(inout) :: bw
        integer, intent(in) :: bit
        integer :: cur
        call bw_ensure(bw)
        if (bit /= 0) then
            cur = iand(int(bw%buf(bw%bytepos), 4), 255)
            cur = ior(cur, ishft(1, bw%bitpos))
            bw%buf(bw%bytepos) = int(cur, 1)
        end if
        bw%bitpos = bw%bitpos + 1
        if (bw%bitpos == 8) then
            bw%bitpos = 0
            bw%bytepos = bw%bytepos + 1
            call bw_ensure(bw)
        end if
    end subroutine bw_putbit

    subroutine bw_putbits_lsb(bw, value, n)
        type(bitwriter_t), intent(inout) :: bw
        integer, intent(in) :: value, n
        integer :: i
        do i = 0, n - 1
            call bw_putbit(bw, iand(ishft(value, -i), 1))
        end do
    end subroutine bw_putbits_lsb

    subroutine bw_puthuff(bw, code, n)
        type(bitwriter_t), intent(inout) :: bw
        integer, intent(in) :: code, n
        integer :: i
        do i = n - 1, 0, -1
            call bw_putbit(bw, iand(ishft(code, -i), 1))
        end do
    end subroutine bw_puthuff

    subroutine bw_align(bw)
        type(bitwriter_t), intent(inout) :: bw
        if (bw%bitpos /= 0) then
            bw%bitpos = 0
            bw%bytepos = bw%bytepos + 1
            call bw_ensure(bw)
        end if
    end subroutine bw_align

    subroutine bw_put_byte_raw(bw, byteval)
        type(bitwriter_t), intent(inout) :: bw
        integer, intent(in) :: byteval  ! 0..255, must be called byte-aligned
        call bw_ensure(bw)
        bw%buf(bw%bytepos) = int(byteval, 1)
        bw%bytepos = bw%bytepos + 1
        call bw_ensure(bw)
    end subroutine bw_put_byte_raw

    function bw_length(bw) result(n)
        type(bitwriter_t), intent(in) :: bw
        integer(8) :: n
        n = bw%bytepos - 1
        if (bw%bitpos /= 0) n = n + 1
    end function bw_length

    ! =====================================================================
    ! Canonical Huffman construction, RFC1951 3.2.2, shared by encode/decode
    ! =====================================================================
    subroutine huff_build_decode(lengths, h)
        integer, intent(in) :: lengths(0:)
        type(huff_t), intent(out) :: h
        integer :: n, i, len_, offs(0:MAXBITS)

        n = size(lengths)
        h%count = 0
        do i = 0, n - 1
            h%count(lengths(i)) = h%count(lengths(i)) + 1
        end do
        h%count(0) = 0

        offs(0) = 0
        offs(1) = 0
        do len_ = 1, MAXBITS - 1
            offs(len_ + 1) = offs(len_) + h%count(len_)
        end do

        allocate (h%symbol(0:n - 1))
        do i = 0, n - 1
            len_ = lengths(i)
            if (len_ > 0) then
                h%symbol(offs(len_)) = i
                offs(len_) = offs(len_) + 1
            end if
        end do
    end subroutine huff_build_decode

    subroutine huff_build_encode(lengths, codes)
        integer, intent(in) :: lengths(0:)
        integer, intent(out) :: codes(0:)
        integer :: n, i, len_, bl_count(0:MAXBITS), next_code(0:MAXBITS), code

        n = size(lengths)
        bl_count = 0
        do i = 0, n - 1
            bl_count(lengths(i)) = bl_count(lengths(i)) + 1
        end do
        bl_count(0) = 0

        code = 0
        next_code(0) = 0
        do len_ = 1, MAXBITS
            code = ishft(code + bl_count(len_ - 1), 1)
            next_code(len_) = code
        end do

        codes = 0
        do i = 0, n - 1
            len_ = lengths(i)
            if (len_ > 0) then
                codes(i) = next_code(len_)
                next_code(len_) = next_code(len_) + 1
            end if
        end do
    end subroutine huff_build_encode

    function huff_decode(br, h) result(sym)
        type(bitreader_t), intent(inout) :: br
        type(huff_t), intent(in) :: h
        integer :: sym
        integer :: code, first, index_, len_, count_l

        code = 0
        first = 0
        index_ = 0
        do len_ = 1, MAXBITS
            code = ior(code, br_getbit(br))
            count_l = h%count(len_)
            if (code - first < count_l) then
                sym = h%symbol(index_ + (code - first))
                return
            end if
            index_ = index_ + count_l
            first = first + count_l
            first = ishft(first, 1)
            code = ishft(code, 1)
        end do
        sym = -1   ! invalid code (corrupt stream)
    end function huff_decode

    ! =====================================================================
    ! Fixed Huffman tables, RFC1951 3.2.6
    ! =====================================================================
    subroutine fixed_litlen_lengths(lengths)
        integer, intent(out) :: lengths(0:287)
        integer :: i
        do i = 0, 143
            lengths(i) = 8
        end do
        do i = 144, 255
            lengths(i) = 9
        end do
        do i = 256, 279
            lengths(i) = 7
        end do
        do i = 280, 287
            lengths(i) = 8
        end do
    end subroutine fixed_litlen_lengths

    subroutine fixed_dist_lengths(lengths)
        integer, intent(out) :: lengths(0:29)
        lengths = 5
    end subroutine fixed_dist_lengths

    ! =====================================================================
    ! INFLATE core: consumes one DEFLATE stream, returns raw output bytes
    ! =====================================================================
    subroutine inflate_stream(br, out, outlen, status)
        type(bitreader_t), intent(inout) :: br
        integer(1), allocatable, intent(inout) :: out(:)
        integer(8), intent(inout) :: outlen
        integer, intent(out) :: status

        integer :: bfinal, btype
        integer :: fixed_ll_lengths(0:287), fixed_d_lengths(0:29)
        type(huff_t) :: fixed_ll, fixed_d
        logical :: fixed_built

        status = 0
        fixed_built = .false.

        do
            bfinal = br_getbits(br, 1)
            btype = br_getbits(br, 2)

            select case (btype)
            case (0)
                call inflate_stored(br, out, outlen, status)
            case (1)
                if (.not. fixed_built) then
                    call fixed_litlen_lengths(fixed_ll_lengths)
                    call fixed_dist_lengths(fixed_d_lengths)
                    call huff_build_decode(fixed_ll_lengths, fixed_ll)
                    call huff_build_decode(fixed_d_lengths, fixed_d)
                    fixed_built = .true.
                end if
                call inflate_huffblock(br, fixed_ll, fixed_d, out, outlen, status)
            case (2)
                call inflate_dynamic_block(br, out, outlen, status)
            case default
                status = -1  ! reserved BTYPE=3, invalid
            end select

            if (status /= 0) return
            if (bfinal == 1) exit
        end do
    end subroutine inflate_stream

    subroutine ensure_capacity(out, outlen, needed)
        integer(1), allocatable, intent(inout) :: out(:)
        integer(8), intent(in) :: outlen, needed
        integer(1), allocatable :: tmp(:)
        integer(8) :: newcap
        if (outlen + needed <= size(out, kind=8)) return
        newcap = max(size(out, kind=8) * 2_8, outlen + needed)
        allocate (tmp(newcap))
        if (outlen > 0) tmp(1:outlen) = out(1:outlen)
        call move_alloc(tmp, out)
    end subroutine ensure_capacity

    subroutine inflate_stored(br, out, outlen, status)
        type(bitreader_t), intent(inout) :: br
        integer(1), allocatable, intent(inout) :: out(:)
        integer(8), intent(inout) :: outlen
        integer, intent(out) :: status
        integer :: len_, nlen_, lo, hi, i

        call br_align(br)
        lo = br_getbits(br, 8)
        hi = br_getbits(br, 8)
        len_ = ior(lo, ishft(hi, 8))
        lo = br_getbits(br, 8)
        hi = br_getbits(br, 8)
        nlen_ = ior(lo, ishft(hi, 8))

        if (iand(len_, 65535) /= iand(ieor(nlen_, 65535), 65535)) then
            status = -2  ! LEN/NLEN mismatch, corrupt stream
            return
        end if

        call ensure_capacity(out, outlen, int(len_, 8))
        do i = 1, len_
            outlen = outlen + 1
            out(outlen) = int(br_getbits(br, 8), 1)
        end do
        status = 0
    end subroutine inflate_stored

    subroutine inflate_huffblock(br, ll_table, d_table, out, outlen, status)
        type(bitreader_t), intent(inout) :: br
        type(huff_t), intent(in) :: ll_table, d_table
        integer(1), allocatable, intent(inout) :: out(:)
        integer(8), intent(inout) :: outlen
        integer, intent(out) :: status

        integer :: sym, lenidx, distsym, length, distance, k
        integer(8) :: srcpos

        status = 0
        do
            sym = huff_decode(br, ll_table)
            if (sym < 0) then
                status = -3  ! invalid literal/length code
                return
            else if (sym < 256) then
                call ensure_capacity(out, outlen, 1_8)
                outlen = outlen + 1
                out(outlen) = int(sym, 1)
            else if (sym == 256) then
                exit  ! end of block
            else
                lenidx = sym - 257
                if (lenidx > 28) then
                    status = -4  ! invalid length code
                    return
                end if
                length = LBASE(lenidx) + br_getbits(br, LEXT(lenidx))

                distsym = huff_decode(br, d_table)
                if (distsym < 0 .or. distsym > 29) then
                    status = -5  ! invalid distance code
                    return
                end if
                distance = DBASE(distsym) + br_getbits(br, DEXT(distsym))

                if (int(distance, 8) > outlen) then
                    status = -6  ! distance refers before start of output
                    return
                end if

                call ensure_capacity(out, outlen, int(length, 8))
                srcpos = outlen - distance
                do k = 1, length
                    srcpos = srcpos + 1
                    outlen = outlen + 1
                    out(outlen) = out(srcpos)
                end do
            end if
        end do
    end subroutine inflate_huffblock

    subroutine inflate_dynamic_block(br, out, outlen, status)
        type(bitreader_t), intent(inout) :: br
        integer(1), allocatable, intent(inout) :: out(:)
        integer(8), intent(inout) :: outlen
        integer, intent(out) :: status

        integer :: hlit, hdist, hclen, i, sym, rep, prevlen
        integer :: clc_lengths(0:18)
        integer, allocatable :: all_lengths(:)
        type(huff_t) :: clc_table, ll_table, d_table
        integer :: nlit, ndist, total, pos

        status = 0
        hlit = br_getbits(br, 5) + 257
        hdist = br_getbits(br, 5) + 1
        hclen = br_getbits(br, 4) + 4

        clc_lengths = 0
        do i = 0, hclen - 1
            clc_lengths(CLC_ORDER(i)) = br_getbits(br, 3)
        end do
        call huff_build_decode(clc_lengths, clc_table)

        nlit = hlit
        ndist = hdist
        total = nlit + ndist
        allocate (all_lengths(0:total - 1))

        pos = 0
        prevlen = 0
        do while (pos < total)
            sym = huff_decode(br, clc_table)
            if (sym < 0) then
                status = -7  ! invalid code-length code
                return
            end if
            if (sym < 16) then
                all_lengths(pos) = sym
                prevlen = sym
                pos = pos + 1
            else if (sym == 16) then
                rep = 3 + br_getbits(br, 2)
                if (pos == 0) then
                    status = -8
                    return
                end if
                do i = 1, rep
                    if (pos >= total) exit
                    all_lengths(pos) = prevlen
                    pos = pos + 1
                end do
            else if (sym == 17) then
                rep = 3 + br_getbits(br, 3)
                do i = 1, rep
                    if (pos >= total) exit
                    all_lengths(pos) = 0
                    pos = pos + 1
                end do
                prevlen = 0
            else  ! sym == 18
                rep = 11 + br_getbits(br, 7)
                do i = 1, rep
                    if (pos >= total) exit
                    all_lengths(pos) = 0
                    pos = pos + 1
                end do
                prevlen = 0
            end if
        end do

        call huff_build_decode(all_lengths(0:nlit - 1), ll_table)
        call huff_build_decode(all_lengths(nlit:total - 1), d_table)

        call inflate_huffblock(br, ll_table, d_table, out, outlen, status)
    end subroutine inflate_dynamic_block

    ! =====================================================================
    ! LZ77 match finder + fixed-Huffman DEFLATE encoder
    ! =====================================================================
    subroutine deflate_fixed(input, bw)
        integer(1), intent(in) :: input(:)
        type(bitwriter_t), intent(inout) :: bw

        integer, parameter :: HASH_BITS = 15
        integer, parameter :: HASH_SIZE = ishft(1, HASH_BITS)
        integer, parameter :: MIN_MATCH = 3
        integer, parameter :: MAX_MATCH = 258
        integer, parameter :: MAX_DIST = 32768
        integer, parameter :: MAX_CHAIN = 128

        integer, allocatable :: head(:), prev(:)
        integer :: n, i, h, cand, chainlen
        integer :: bestlen, bestdist, matchlen, maxlen
        integer :: ll_lengths(0:287), d_lengths(0:29)
        integer :: ll_codes(0:287), d_codes(0:29)
        integer :: lenidx, distsym, extra
        integer(4) :: b0, b1, b2

        n = size(input)

        call fixed_litlen_lengths(ll_lengths)
        call fixed_dist_lengths(d_lengths)
        call huff_build_encode(ll_lengths, ll_codes)
        call huff_build_encode(d_lengths, d_codes)

        allocate (head(0:HASH_SIZE - 1), prev(1:max(n, 1)))
        head = 0

        i = 1
        do while (i <= n)
            bestlen = 0
            bestdist = 0

            if (i + MIN_MATCH - 1 <= n) then
                b0 = iand(int(input(i), 4), 255)
                b1 = iand(int(input(i + 1), 4), 255)
                b2 = iand(int(input(i + 2), 4), 255)
                h = iand(ieor(ieor(ishft(b0, 10), ishft(b1, 5)), b2), HASH_SIZE - 1)

                cand = head(h)
                chainlen = 0
                maxlen = min(MAX_MATCH, n - i + 1)
                do while (cand /= 0 .and. chainlen < MAX_CHAIN)
                    if (i - cand <= MAX_DIST) then
                        matchlen = match_length(input, cand, i, maxlen)
                        if (matchlen > bestlen) then
                            bestlen = matchlen
                            bestdist = i - cand
                            if (bestlen >= maxlen) exit
                        end if
                    end if
                    cand = prev(cand)
                    chainlen = chainlen + 1
                end do
            end if

            if (bestlen >= MIN_MATCH) then
                call insert_hash_range(input, n, i, min(i + bestlen - 1, n), head, prev, HASH_SIZE)

                lenidx = length_to_index(bestlen)
                call bw_puthuff(bw, ll_codes(257 + lenidx), ll_lengths(257 + lenidx))
                extra = bestlen - LBASE(lenidx)
                if (LEXT(lenidx) > 0) call bw_putbits_lsb(bw, extra, LEXT(lenidx))

                distsym = distance_to_index(bestdist)
                call bw_puthuff(bw, d_codes(distsym), d_lengths(distsym))
                extra = bestdist - DBASE(distsym)
                if (DEXT(distsym) > 0) call bw_putbits_lsb(bw, extra, DEXT(distsym))

                i = i + bestlen
            else
                if (i + MIN_MATCH - 1 <= n) then
                    prev(i) = head(h)
                    head(h) = i
                end if
                call bw_puthuff(bw, ll_codes(iand(int(input(i), 4), 255)), &
                                ll_lengths(iand(int(input(i), 4), 255)))
                i = i + 1
            end if
        end do

        call bw_puthuff(bw, ll_codes(256), ll_lengths(256))  ! end of block
    end subroutine deflate_fixed

    subroutine insert_hash_range(input, n, lo, hi, head, prev, hash_size)
        integer(1), intent(in) :: input(:)
        integer, intent(in) :: n, lo, hi, hash_size
        integer, intent(inout) :: head(0:), prev(:)
        integer :: p, h
        integer(4) :: b0, b1, b2
        do p = lo, hi
            if (p + 2 <= n) then
                b0 = iand(int(input(p), 4), 255)
                b1 = iand(int(input(p + 1), 4), 255)
                b2 = iand(int(input(p + 2), 4), 255)
                h = iand(ieor(ieor(ishft(b0, 10), ishft(b1, 5)), b2), hash_size - 1)
                prev(p) = head(h)
                head(h) = p
            end if
        end do
    end subroutine insert_hash_range

    function match_length(input, cand, cur, maxlen) result(len_)
        integer(1), intent(in) :: input(:)
        integer, intent(in) :: cand, cur, maxlen
        integer :: len_
        len_ = 0
        do while (len_ < maxlen)
            if (input(cand + len_) /= input(cur + len_)) exit
            len_ = len_ + 1
        end do
    end function match_length

    function length_to_index(length) result(idx)
        integer, intent(in) :: length
        integer :: idx
        do idx = 28, 0, -1
            if (LBASE(idx) <= length) then
                if (idx == 28) return
                if (length < LBASE(idx + 1)) return
            end if
        end do
        idx = 0
    end function length_to_index

    function distance_to_index(dist) result(idx)
        integer, intent(in) :: dist
        integer :: idx
        do idx = 29, 0, -1
            if (DBASE(idx) <= dist) then
                if (idx == 29) return
                if (dist < DBASE(idx + 1)) return
            end if
        end do
        idx = 0
    end function distance_to_index

    ! =====================================================================
    ! Public API
    ! =====================================================================
    subroutine gzip_compress(input, output, status)
        integer(1), intent(in) :: input(:)
        integer(1), allocatable, intent(out) :: output(:)
        integer, intent(out), optional :: status

        type(bitwriter_t) :: bw
        integer(8) :: c, isize, deflen

        call bw_init(bw, max(int(size(input), 8) / 2_8, 64_8) + 18_8)

        ! ---- gzip header (RFC1952 2.3.1): minimal, no optional fields ----
        call bw_put_byte_raw(bw, 31)   ! ID1
        call bw_put_byte_raw(bw, 139)  ! ID2
        call bw_put_byte_raw(bw, 8)    ! CM = deflate
        call bw_put_byte_raw(bw, 0)    ! FLG = 0
        call bw_put_byte_raw(bw, 0)    ! MTIME
        call bw_put_byte_raw(bw, 0)
        call bw_put_byte_raw(bw, 0)
        call bw_put_byte_raw(bw, 0)
        call bw_put_byte_raw(bw, 0)    ! XFL
        call bw_put_byte_raw(bw, 255)  ! OS = unknown

        ! ---- single final block, fixed Huffman ----
        call bw_putbit(bw, 1)          ! BFINAL = 1
        call bw_putbits_lsb(bw, 1, 2)  ! BTYPE = 01 (fixed Huffman)
        call deflate_fixed(input, bw)
        call bw_align(bw)

        ! ---- trailer: CRC32 + ISIZE mod 2^32, little-endian ----
        c = crc32(input)
        isize = iand(int(size(input), 8), M32)
        call bw_put_byte_raw(bw, int(iand(c, 255_8), 4))
        call bw_put_byte_raw(bw, int(iand(ishft(c, -8), 255_8), 4))
        call bw_put_byte_raw(bw, int(iand(ishft(c, -16), 255_8), 4))
        call bw_put_byte_raw(bw, int(iand(ishft(c, -24), 255_8), 4))
        call bw_put_byte_raw(bw, int(iand(isize, 255_8), 4))
        call bw_put_byte_raw(bw, int(iand(ishft(isize, -8), 255_8), 4))
        call bw_put_byte_raw(bw, int(iand(ishft(isize, -16), 255_8), 4))
        call bw_put_byte_raw(bw, int(iand(ishft(isize, -24), 255_8), 4))

        deflen = bw_length(bw)
        allocate (output(deflen))
        output = bw%buf(1:deflen)
        if (present(status)) status = 0
    end subroutine gzip_compress

    subroutine gzip_uncompress(input, output, status)
        integer(1), intent(in) :: input(:)
        integer(1), allocatable, intent(out) :: output(:)
        integer, intent(out), optional :: status

        type(bitreader_t) :: br
        integer(8) :: pos, n, outlen, expected_isize, expected_crc, actual_crc
        integer :: flg, xlen, i, st
        integer(1), allocatable :: out(:)

        n = size(input, kind=8)
        st = 0

        if (n < 18) then
            call fail_status(status, -100, "input too small to be gzip")
            return
        end if
        if (iand(int(input(1), 4), 255) /= 31 .or. iand(int(input(2), 4), 255) /= 139) then
            call fail_status(status, -101, "bad gzip magic")
            return
        end if
        if (iand(int(input(3), 4), 255) /= 8) then
            call fail_status(status, -102, "unsupported compression method")
            return
        end if

        flg = iand(int(input(4), 4), 255)
        pos = 11  ! byte after MTIME(4)+XFL+OS, i.e. 4(header)+4(mtime)+1(xfl)+1(os)+1

        if (iand(flg, 4) /= 0) then  ! FEXTRA
            xlen = iand(int(input(pos), 4), 255) + ishft(iand(int(input(pos + 1), 4), 255), 8)
            pos = pos + 2 + xlen
        end if
        if (iand(flg, 8) /= 0) then  ! FNAME
            do while (input(pos) /= 0_1)
                pos = pos + 1
            end do
            pos = pos + 1
        end if
        if (iand(flg, 16) /= 0) then  ! FCOMMENT
            do while (input(pos) /= 0_1)
                pos = pos + 1
            end do
            pos = pos + 1
        end if
        if (iand(flg, 2) /= 0) then  ! FHCRC
            pos = pos + 2
        end if

        call br_init(br, input(pos:n))

        allocate (out(max(n * 4_8, 1024_8)))
        outlen = 0
        call inflate_stream(br, out, outlen, st)
        if (st /= 0) then
            call fail_status(status, st, "inflate failed")
            return
        end if

        expected_crc = 0
        expected_isize = 0
        do i = 0, 3
            expected_crc = ior(expected_crc, ishft(int(iand(int(input(n - 7 + i), 4), 255), 8), 8 * i))
        end do
        do i = 0, 3
            expected_isize = ior(expected_isize, ishft(int(iand(int(input(n - 3 + i), 4), 255), 8), 8 * i))
        end do

        actual_crc = crc32(out(1:outlen))
        if (actual_crc /= expected_crc) then
            call fail_status(status, -103, "CRC32 mismatch")
            return
        end if
        if (iand(outlen, M32) /= expected_isize) then
            call fail_status(status, -104, "ISIZE mismatch")
            return
        end if

        allocate (output(outlen))
        if (outlen > 0) output = out(1:outlen)
        if (present(status)) status = 0
    end subroutine gzip_uncompress

    subroutine gzip_compress_file(infile, outfile, status)
        character(len=*), intent(in) :: infile, outfile
        integer, intent(out), optional :: status
        integer(1), allocatable :: raw(:), packed(:)
        integer :: st

        call read_file_bytes(infile, raw, st)
        if (st /= 0) then
            call fail_status(status, st, "read "//trim(infile))
            return
        end if

        call gzip_compress(raw, packed, st)
        if (st /= 0) then
            if (present(status)) status = st
            return
        end if

        call write_file_bytes(outfile, packed, st)
        if (present(status)) status = st
    end subroutine gzip_compress_file

    subroutine gzip_uncompress_file(infile, outfile, status)
        character(len=*), intent(in) :: infile, outfile
        integer, intent(out), optional :: status
        integer(1), allocatable :: packed(:), raw(:)
        integer :: st

        call read_file_bytes(infile, packed, st)
        if (st /= 0) then
            call fail_status(status, st, "read "//trim(infile))
            return
        end if

        call gzip_uncompress(packed, raw, st)
        if (st /= 0) then
            if (present(status)) status = st
            return
        end if

        call write_file_bytes(outfile, raw, st)
        if (present(status)) status = st
    end subroutine gzip_uncompress_file

    ! =====================================================================
    ! File and error-handling helpers
    ! =====================================================================
    subroutine fail_status(status, code, where)
        integer, intent(out), optional :: status
        integer, intent(in) :: code
        character(len=*), intent(in) :: where
        if (present(status)) then
            status = code
        else
            write (0, '(A,A,A,I0)') "gzip_mod: ", where, " failed, code=", code
        end if
    end subroutine fail_status

    subroutine read_file_bytes(filename, bytes, status)
        character(len=*), intent(in) :: filename
        integer(1), allocatable, intent(out) :: bytes(:)
        integer, intent(out) :: status
        integer :: unit, ios
        integer(8) :: fsize

        inquire (file=filename, size=fsize, iostat=ios)
        if (ios /= 0) then
            status = ios
            return
        end if

        open (newunit=unit, file=filename, access='stream', form='unformatted', &
              status='old', action='read', iostat=ios)
        if (ios /= 0) then
            status = ios
            return
        end if

        allocate (bytes(fsize))
        if (fsize > 0) read (unit, iostat=ios) bytes
        close (unit)
        status = ios
        if (fsize == 0) status = 0
    end subroutine read_file_bytes

    subroutine write_file_bytes(filename, bytes, status)
        character(len=*), intent(in) :: filename
        integer(1), intent(in) :: bytes(:)
        integer, intent(out) :: status
        integer :: unit, ios

        open (newunit=unit, file=filename, access='stream', form='unformatted', &
              status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            status = ios
            return
        end if

        if (size(bytes) > 0) write (unit, iostat=ios) bytes
        close (unit)
        status = ios
    end subroutine write_file_bytes

end module gzip_mod
