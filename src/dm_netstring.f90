! Author:  Philipp Engel
! Licence: ISC
module dm_netstring
    !! Module for netstring parsing.
    !!
    !! The next sections are the original format description by D. J. Bernstein
    !! (1997-02-01).
    !!
    !! ## Introduction
    !!
    !! A netstring is a self-delimiting encoding of a string. Netstrings are
    !! very easy to generate and to parse. Any string may be encoded as a
    !! netstring; there are no restrictions on length or on allowed bytes.
    !! Another virtue of a netstring is that it declares the string size up
    !! front. Thus an application can check in advance whether it has enough
    !! space to store the entire string.
    !!
    !! Netstrings may be used as a basic building block for reliable network
    !! protocols. Most high-level protocols, in effect, transmit a sequence of
    !! strings; those strings may be encoded as netstrings and then concatenated
    !! into a sequence of characters, which in turn may be transmitted over a
    !! reliable stream protocol such as TCP.
    !!
    !! Note that netstrings can be used recursively. The result of encoding a
    !! sequence of strings is a single string. A series of those encoded strings
    !! may in turn be encoded into a single string. And so on.
    !!
    !! In this document, a string of 8-bit bytes may be written in two different
    !! forms: as a series of hexadecimal numbers between angle brackets, or as a
    !! sequence of ASCII characters between double quotes.  For example, `68 65
    !! 6c 6c 6f 20 77 6f 72 6c 64 21` is a string of length 12; it is the same
    !! as the string `hello world!`.
    !!
    !! Although this document restricts attention to strings of 8-bit bytes,
    !! netstrings could be used with any 6-bit-or-larger character set.
    !!
    !! ## Definition
    !!
    !! Any string of 8-bit bytes may be encoded as `[len]:[string],`. Here
    !! `[string]` is the string and `[len]` is a nonempty sequence of ASCII
    !! digits giving the length of [string] in decimal. The ASCII digits are
    !! `30` for 0, `31` for 1, and so on up through `39` for 9. Extra zeros at
    !! the front of `[len]` are prohibited: `[len]` begins with `30` exactly
    !! when `[string]` is empty.
    !!
    !! For example, the string `hello world!` is encoded as `31 32 3a 68 65 6c
    !! 6c 6f 20 77 6f 72 6c 64 21 2c`, i.e., `12:hello world!,`. The empty
    !! string is encoded as `0:,`.
    !!
    !! `[len]:[string],` is called a netstring. `[string]` is called the
    !! interpretation of the netstring.
    !!
    !! ## Sample code
    !!
    !! The following C code starts with a buffer buf of length len and prints it
    !! as a netstring.
    !!
    !! ``` c
    !! if (printf("%lu:",len) < 0) barf();
    !! if (fwrite(buf,1,len,stdout) < len) barf();
    !! if (putchar(',') < 0) barf();
    !! ```
    !!
    !! The following C code reads a netstring and decodes it into a dynamically
    !! allocated buffer buf of length len.
    !!
    !! ``` c
    !! if (scanf("%9lu",&len) < 1) barf();  /* > 999999999 bytes is bad */
    !! if (getchar() != ':') barf();
    !! buf = malloc(len + 1);               /* malloc(0) is not portable */
    !! if (!buf) barf();
    !! if (fread(buf,1,len,stdin) < len) barf();
    !! if (getchar() != ',') barf();
    !! ```
    !!
    !! Both of these code fragments assume that the local character set is
    !! ASCII, and that the relevant stdio streams are in binary mode.
    !!
    !! ## Security considerations
    !!
    !! The famous Finger security hole may be blamed on Finger’s use of the CRLF
    !! encoding. In that encoding, each string is simply terminated by CRLF.
    !! This encoding has several problems. Most importantly, it does not declare
    !! the string size in advance. This means that a correct CRLF parser must be
    !! prepared to ask for more and more memory as it is reading the string. In
    !! the case of Finger, a lazy implementor found this to be too much trouble;
    !! instead he simply declared a fixed-size buffer and used C’s `gets()`
    !! function. The rest is history.
    !!
    !! In contrast, as the above sample code shows, it is very easy to handle
    !! netstrings without risking buffer overflow. Thus widespread use of
    !! netstrings may improve network security
    use :: dm_ascii
    use :: dm_error
    implicit none (type, external)
    private

    public :: dm_netstring_read
    public :: dm_netstring_write
contains
    pure elemental subroutine dm_netstring_read(input, output, length, first, last, error)
        !! Reads arbitrary contents from netstring buffer `input` into `output`.
        !! Returns the length of the output in `length`. Argument `first` will
        !! be the start of the data and `last` the end of the data in the
        !! netstring. The output buffer is not cleared!
        !!
        !! Argument `error` is set to the following error codes:
        !!
        !! * `E_FORMAT` if the input is not in netstring format.
        !!
        character(*), intent(inout)           :: input  !! Input buffer.
        character(*), intent(inout), optional :: output !! Netstring buffer.
        integer,      intent(out),   optional :: length !! Length of output.
        integer,      intent(out),   optional :: first  !! First position.
        integer,      intent(out),   optional :: last   !! Last position.
        integer,      intent(out),   optional :: error  !! Error code.

        integer :: i, j, k, l, n, rc

        if (present(length)) length = 0
        if (present(last))   last   = 0

        ns_block: block
            rc = E_FORMAT
            if (len(input) < 3) exit ns_block
            if (.not. dm_ascii_is_digit(input(1:1))) exit ns_block
            if (input(1:1) == '0' .and. dm_ascii_is_digit(input(2:2))) exit ns_block

            i = 1
            n = 0

            do
                if (i > len(input) .or. i > 9) exit ns_block
                if (.not. dm_ascii_is_digit(input(i:i))) exit
                n = n * 10 + (iachar(input(i:i)) - iachar('0'))
                i = i + 1
            end do

            if (present(length)) length = n
            if (i + n + 1 > len(input)) exit ns_block
            if (input(i:i) /= ':') exit ns_block

            j = i + 1
            k = i + n
            l = j + n

            if (input(l:l) /= ',') exit ns_block

            rc = E_NONE
            if (present(output)) output = input(j:k)
            if (present(first))  first  = j
            if (present(last))   last   = l
        end block ns_block

        if (present(error)) error = rc
    end subroutine dm_netstring_read

    pure elemental subroutine dm_netstring_write(input, output, length, error)
        !! Writes input bytes `input` in netstring format to output buffer
        !! `output`. The length of the netstring is returned in `length`. The
        !! output buffer is not cleared!
        !!
        !! Argument `error` is set to the following error codes:
        !!
        !! * `E_BOUNDS` if the netstring is larger than output buffer.
        !! * `E_LIMIT` if the input size is > 999999999 bytes.
        !! * `E_WRITE` if an I/O error occured.
        !!
        character(*), intent(inout)         :: input  !! Input buffer.
        character(*), intent(inout)         :: output !! Netstring buffer.
        integer,      intent(out), optional :: length !! Length of netstring.
        integer,      intent(out), optional :: error  !! Error code.

        integer :: i, j, n, rc, stat

        i = 1 + floor(log10(real(len(input))))
        j = i + 2
        n = j + len(input)

        ns_block: block
            rc = E_LIMIT
            if (i > 9) exit ns_block

            rc = E_BOUNDS
            if (n > len(output)) exit ns_block

            rc = E_WRITE
            write (output, '(i0, ":", a, ",")', iostat=stat) len(input), input
            if (stat == 0) rc = E_NONE
        end block ns_block

        if (present(length)) length = n
        if (present(error))  error  = rc
    end subroutine dm_netstring_write
end module dm_netstring
