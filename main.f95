program simulated_annealing

    use constants
    use io
    use shell_prompts

    implicit none
    ! static 
    integer, parameter :: u_pos=11,u_csf=12,u_out=13                            ! file units
    integer, parameter :: dim_m=254, dim_n=333                                  ! image dimensions
    ! dynamic
    integer :: i, j, T_n
    integer, dimension(N_CSF) :: px_sac, px_csf                                 ! pixel accuracy validation
    integer, allocatable :: matrix(:,:), matrix_sim(:,:), matrix_csf(:,:)
    real(kind=p) :: T, T_init, T_final, lambda                                  ! temperature

    ! allocation
    allocate(matrix(dim_m, dim_n))
    allocate(matrix_sim(dim_m, dim_n))
    allocate(matrix_csf(dim_m, dim_n))

    ! %%% Program %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


    call program_prompt(TITLE, '-')                                             ! shell_prompts
    
    ! read data
    call sa_read_data(u_pos, MR_DATA_FILE, matrix_sim, dim_m, dim_n)            ! io
    call sa_read_data(u_csf, SA_DATA_FILE, matrix_csf, dim_m, dim_n)            ! io

    ! initialize random matrix
    call matrix_rand_init(matrix, dim_m, dim_n)                                 ! main
    
    ! asssign temperature values
    T_final = TEMP(1)
    T_init = TEMP(2)
    lambda = TEMP(3)
    T_n = n_iter(T_init, T_final, lambda)
    T = T_init

    ! parameter display
    if (VERBOSE) call parameter_display(T_init, T_final, lambda, SWEEPS, JJ)    ! main

    ! simulated annealing
    call progress(0)                                                            ! shell_prompts
    do i = 1, T_n
        ! monitor progress
        if (mod(real(i, kind=p), T_n/T_init) .lt. 1._p) then
            call progress(i/ceiling(T_n/T_init))                                ! shell prompts
        end if
        ! sweeps
        do j = 1, SWEEPS
            call sweep(matrix, matrix_sim, dim_m, dim_n, 1/T, method)           ! main
        end do
        T = T / lambda
    end do
    call progress(10)                                                           ! shell_prompts

    ! write data
    if (OUTPUT) then
        if (JJ .eq. 0) then
            call sa_write_data(u_out, 'data/SegLocal.dat', matrix, dim_m, dim_n)! io
        else
            call sa_write_data(u_out, 'data/SegSA.dat', matrix, dim_m, dim_n)   ! io
        end if
        write(*, fmt='(/,a,/)') "File successfully created."
    end if

    ! accuracy validation
    if (errcalc) then
        do i = 1, N_CSF
          px_sac(i) = count(matrix .eq. i)
          px_csf(i) = count(matrix_csf .eq. i)
        end do

        if (VERBOSE) then
            print *
            write(*,*) " Class occurences:"
            write(*,*) " SA", px_sac(:)
            write(*,*) " MR", px_csf(:)
            print *
        end if

        do i = 1, N_CSF
            write(*, fmt='(a,i2,a,f6.3,a)')                                 &
                " Error for class ", i, ": ",                               &
                calc_error(px_csf(i), px_sac(i), size(matrix)), "%"             ! main
        end do
        write(*, fmt='(/,a,f6.3,a)')                                        &
            "Total error: ", calc_total_error(px_sac,px_csf,N_CSF),  "%"        ! main
    end if

    call program_prompt('Program ended successully', '-', len(trim(TITLE)) + 2) ! shell_prompts 

    stop

    contains ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


    function n_iter(x_i, x_f, dx) result(n_x) ! ------------------------------> FUNC n_iter()
        ! calculated how many iterations until condition is true
        real(kind=p) :: x, x_i, x_f, dx
        integer :: n_x

        n_x = 0
        x = x_i
        do while(x .gt. x_f)
            n_x = n_x + 1
            x = x / dx
        end do
        
    end function n_iter


    subroutine matrix_rand_init(matrix, dim_m, dim_n) ! ----------------------> SUBR matrix_rand_init
        ! Fill matrix with random class
        integer, intent(in) :: dim_m, dim_n
        integer, dimension(dim_m, dim_n), intent(inout) :: matrix
        integer :: i, j
        real, save :: rand

        do i = 1, dim_m
            do j = 1, dim_n
                call random_number(rand)
                matrix(i,j) = int(rand * 5._p) + 1
            enddo
        enddo

    end subroutine matrix_rand_init


    subroutine parameter_display(Ti, Tf, l, s, j) ! --------------------------> SUBR parameter_display()
        ! display parameter in console output
        character(len=*), parameter :: freal='(a,a,t28,f7.3)'
        integer, intent(in) :: s
        real(kind=p), intent(in) :: Ti, Tf, l, j

        print *
        write(*, fmt=freal) "|| - ", "Inital temperature:", Ti
        write(*, fmt=freal) "|| - ", "Final temperature:", Tf
        write(*, fmt=freal) "|| - ", "Lambda parameter:", l
        write(*, fmt=freal) "|| - ", "N° of sweeps: ", real(s, kind=p)
        write(*, fmt=freal) "|| - ", "Coupling constant:", j
        print *

    end subroutine parameter_display


    subroutine sweep(matrix, data, dim_m, dim_n, beta, method) ! -------------> SUBR sweep()
        ! perform sweep routine
        character(len=*), intent(in) :: method
        integer, intent(in) :: dim_m, dim_n
        integer, dimension(dim_m,dim_n), intent(in) :: data
        integer, dimension(dim_m,dim_n), intent(inout) :: matrix
        integer :: i, pos_m, pos_n, csf_x
        real(kind=p), intent(in) :: beta
        real(kind=p) :: delta_E, r

        do i = 1, size(matrix)

            pos_m = int(rand_real() * dim_m) + 1
            pos_n = int(rand_real() * dim_n) + 1
            csf_x = int(rand_real() * 5._p) + 1
            
            delta_E = energy_difference(                                    &
                matrix, data, dim_m, dim_n, csf_x, pos_m, pos_n             &
            )

            if (delta_E * beta .gt. 20._p) then
                r = 0._p
            else
                if (delta_E * beta .lt. 0.05_p) then
                    r = 1._p
                else
                    r = exp(-beta * delta_E)
                end if
            end if

            if (rand_real() .lt. acceptance_probability(method, r)) then
                matrix(pos_m, pos_n) = csf_x
            end if
            
            ! r = exp(-beta * delta_E)
            
            ! if (delta_E .le. 0._p) then
            !     continue
            ! else 
            !     if (rand_real() .lt. acceptance_probability(method, r)) then
            !         matrix(pos_m, pos_n) = csf_x
            !     end if
            ! end if

        end do

    end subroutine sweep


    function rand_real() result(r) ! -----------------------------------------> FUNC rand_real()
        ! assign random real number between 0 and 1
        real(kind=p) :: r

        call random_number(r)

    end function rand_real


    function acceptance_probability(method, r) ! -----------------------------> FUNC acceptance_probability()
        ! calculate acceptance probibilty based on passed method
        character(len=*) :: method
        real(kind=p) :: acceptance_probability, r

        select case (method)
            case ('metropolis')
                acceptance_probability =  min(1._p, r)
            case ('heat_bath')
                acceptance_probability = r / (1 + r)
            case default
                acceptance_probability =  min(1._p, r)
        end select

    end function acceptance_probability


    function energy_difference(                         &   ! ----------------> FUNC energy_difference()   
        matrix, data, dim_m, dim_n, csf, pos_m, pos_n   &
    ) result(delta_E)
        ! calculate energy difference between pixel and neighbors
        integer :: dim_m, dim_n, pos_m, pos_n
        integer :: csf, csf_                                                    ! csf = current value, csf_ = old value
        integer :: i, Inew, Jnew
        integer, dimension(dim_m, dim_n) :: matrix, data
        real(kind=p) :: delta_E
        real(kind=p) :: E_old, E_new

        E_old = 0._p
        E_new = 0._p

        csf_= matrix(pos_m, pos_n)

        do i = 1, NN
            
            Inew = pos_m + Inn(i)
            Jnew = pos_n + Jnn(i)
            
            ! enforcing periodic boundary conditions
            if (Inew .le. 0) then
                Inew = dim_m
            else
                if(Inew .gt. dim_m) Inew = 1
            endif
            if (Jnew .le. 0) then
                Jnew = dim_n
            else
                if(Jnew .gt. dim_n) Jnew = 1
            endif

            if (csf .eq. matrix(Inew, Jnew)) E_new = E_new - JJ
            if (matrix(pos_m, pos_n) .eq. matrix(Inew, Jnew)) E_old = E_old - JJ

        end do

        E_old = E_old + energy(data(pos_m, pos_n), MEANV(csf_), VARV(csf_))
        E_new = E_new + energy(data(pos_m, pos_n), MEANV(csf), VARV(csf))

        delta_E = E_new - E_old
        
    end function energy_difference


    function energy(value, mean, variance) ! ---------------------------------> FUNC energy()
        ! calculate energy of pixel
        integer :: value, mean, variance
        real(kind=p) :: energy

        energy =                                                            &
            (value - mean)**2 / (2._p * variance**2) +                      &
            log(real(variance, kind=p))
        
    end function energy


    function calc_error(val_x, val_y, val_n) result(err) ! -------------------> FUNC calc_error()
        ! calculates error [%]
        integer :: val_x, val_y, val_n
        real(kind=p) :: err

        err = abs((val_x - val_y) / real(val_n, kind=p)) * 100._p
        
    end function calc_error


    function calc_total_error(calculated, comparison, dim) result(err) ! -----> FUNC calc_total_error()
        ! calculates total error [%]
        integer :: dim, n, i
        integer, dimension(dim) :: calculated, comparison
        real(kind=p) :: err

        n = sum(comparison(:))
        err = 0
        do i = 1, dim
            err = err +                                                     &
                abs((calculated(i) - comparison(i)) / real(n, kind=p)) * 100._p
        end do
        
    end function calc_total_error


end program simulated_annealing