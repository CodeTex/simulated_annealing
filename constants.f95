module constants

    implicit none
    ! static
    integer, parameter :: iter=selected_int_kind(8)                             ! kind for iterative variables
    integer, parameter :: p=selected_real_kind(16)                              ! float precision parameter
    integer, parameter :: char_max=80                                           ! max character length
    ! Program Interface Parameters %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    character(len=char_max), parameter :: TITLE='Simulated Annealing of NMR Scans'  ! title which is displayed in shell
    character(len=char_max), parameter :: DIRNAME='data'                            ! path for data output
    character(len=char_max), parameter :: MR_DATA_FILE='data/SimMRimage.dat'        ! MR image contained measurements
    character(len=char_max), parameter :: SA_DATA_FILE='data/CorrectSegImage.dat'   ! correcly segmented image for validation
    logical, parameter :: OUTPUT=.true.                                         ! generate output files
    logical, parameter :: VERBOSE=.true.                                        ! console output with add. information
    logical, parameter :: ERRCALC=.true.                                        ! perform accuracy validation
    ! Ising Model Parameters %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    character(len=char_max), parameter :: METHOD='metropolis'                   ! 'metropolis' or 'heat_bath'
    integer, parameter :: SWEEPS=120                                            ! N° of sweeps
    integer, parameter :: N_CSF=5                                               ! N° of classes
    integer, parameter :: NN=4                                                  ! N° of neighbors
    integer, dimension(NN), parameter :: Inn=(/ 1, -1, 0, 0/)                   ! Nearest neighbor array I
    integer, dimension(NN), parameter :: Jnn=(/ 0, 0, 1, -1/)                   ! Nearest neighbor array J
    integer, dimension(N_CSF), parameter ::  MEANV=(/30,426,602,1223,167/)      ! mean values
    integer, dimension(N_CSF), parameter ::  VARV=(/30,59,102,307,69/)          ! variance values
    real(kind=p), dimension(3), parameter :: TEMP=(/.1,10.,1.1/)                ! Init, Finit, lambda (steps)
    real(kind=p), parameter :: JJ=2.003                                         ! coupling constant J
    !
    !            BG   WM   GM   CSF   SB
    !   mean:   (30, 426, 602, 1223, 167)
    !   std:    (30,  59, 102,  307,  69)
    !
    
end module constants