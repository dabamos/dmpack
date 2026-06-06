! dmtestods.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestods
    !! Tests for OpenDocument Spreadsheet export.
    use :: dmpack
    implicit none (type, external)

    character(*), parameter :: TEST_NAME = 'dmtestods'
    integer,      parameter :: NTESTS    = 1

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01) &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats)
contains
    logical function test01() result(stat)
        character(*), parameter :: ODS_FILE = '/tmp/testdmpack.ods' ! Must be absolute path.

        type(ods_type) :: ods

        stat = TEST_FAILED
        if (dm_file_exists(ODS_FILE)) call dm_file_delete(ODS_FILE)

        ods_block: block
            integer, parameter :: NCOLUMNS = 8
            integer, parameter :: NROWS    = 32

            character(512)       :: formula
            integer              :: i, j
            type(ods_style_type) :: styles(2)

            styles(1) = ods_style_type( &
                name       = 'ce1', &
                family     = ODS_STYLE_FAMILY_TABLE_CELL, &
                paragraph  = ods_style_paragraph_type(text_align='center'), &
                table_cell = ods_style_table_cell_type(background_color='#f0f0f0'), &
                text       = ods_style_text_type(font_name='Liberation Sans', font_weight='bold') &
            )

            styles(2) = ods_style_type( &
                name      = 'ce2', &
                family    = ODS_STYLE_FAMILY_TABLE_CELL, &
                paragraph = ods_style_paragraph_type(text_align='center'), &
                text      = ods_style_text_type(font_name='Liberation Sans') &
            )

            ! Create ODS context.
            call dm_ods_init(ods, styles=styles)
            if (dm_ods_is_error(ods)) exit ods_block

            print '(" ODS directory: ", a)', dm_ods_path(ods)
            print '(" ODS file.....: ", a)', ODS_FILE

            ! Create table.
            call dm_ods_create_table(ods, 'Table1', NCOLUMNS)
            if (dm_ods_is_error(ods)) exit ods_block

            ! Add header row.
            call dm_ods_add_row(ods)
            if (dm_ods_is_error(ods)) exit ods_block

            do i = 1, NCOLUMNS
                call dm_ods_add_cell(ods, achar(64 + i), style_name='ce1')
                if (dm_ods_is_error(ods)) exit ods_block
            end do

            call dm_ods_finalize_row(ods)
            if (dm_ods_is_error(ods)) exit ods_block

            ! Add data rows.
            do i = 1, NROWS
                call dm_ods_add_row(ods)
                if (dm_ods_is_error(ods)) exit ods_block

                ! Add cells.
                do j = 1, NCOLUMNS
                    call dm_ods_add_cell(ods, j, style_name='ce2')
                    if (dm_ods_is_error(ods)) exit ods_block
                end do

                call dm_ods_finalize_row(ods)
                if (dm_ods_is_error(ods)) exit ods_block
            end do

            ! Add footer row.
            call dm_ods_add_row(ods)
            if (dm_ods_is_error(ods)) exit ods_block

            do i = 1, NCOLUMNS
                write (formula, '("of:=SUM([.", a, i0, ":.", a, i0, "]")') achar(64 + i), 2, achar(64 + i), 1 + NROWS
                call dm_ods_add_cell(ods, i * NROWS, formula=formula, style_name='ce2')
                if (dm_ods_is_error(ods)) exit ods_block
            end do

            call dm_ods_finalize_row(ods)
            if (dm_ods_is_error(ods)) exit ods_block

            ! Finish table and document.
            call dm_ods_finalize_table(ods); if (dm_ods_is_error(ods)) exit ods_block
            call dm_ods_finalize(ods);       if (dm_ods_is_error(ods)) exit ods_block

            ! Write ODS file.
            call dm_ods_output(ods, ODS_FILE)
            if (dm_ods_is_error(ods)) exit ods_block
        end block ods_block

        call dm_error_out(dm_ods_error(ods))
        !call dm_ods_destroy(ods)
        if (dm_ods_is_error(ods)) return
        if (.not. dm_file_exists(ODS_FILE)) return

        stat = TEST_PASSED
    end function test01
end program dmtestods
