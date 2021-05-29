module shell_prompts

    use constants

    implicit none

    contains ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    subroutine program_prompt(text, div_sym, div_len) ! ----------------------> SUBR program_prompt()
        ! Generates console output in specific format
        character(len=*), intent(in) :: text                                    ! text to be displayed
        character(len=1), intent(in) :: div_sym                                 ! symbol of the divider
        integer, optional :: div_len
        integer :: div_length
        integer :: text_shift

        if (present(div_len)) then
            div_length = div_len
        else
            div_length = len(trim(text)) + 2
        endif

        text_shift = (div_length - len(trim(text))) / 2 

        write(*, fmt='(/, a)')          repeat(div_sym, div_length)
        write(*, fmt='(a, a)')          repeat(' ', text_shift), trim(text)     ! align center horizontally
        write(*, fmt='(a, /)')          repeat(div_sym, div_length)

    end subroutine program_prompt


    subroutine progress(pos) ! -----------------------------------------------> ! SUBR progress()

        character(len=17) :: bar="???% |          |"
        integer(kind=iter) :: i
        integer(kind=iter), intent(in) :: pos

        write(unit=bar(1:3),fmt="(i3)") 10 * pos

        do i = 1, pos
            bar(6+i:6+i) = "#"
        enddo

        write(unit=6,fmt="(a1,a20,$)") char(13), bar

    end subroutine progress


end module shell_prompts