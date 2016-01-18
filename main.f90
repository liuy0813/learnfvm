!main program for fvm
PROGRAM MAIN

use mod_constants    , only : p2, zero
use mod_grid_data    , only : read_grid, construct_grid_data  !, check_grid_data
use mod_time         , only : get_date,time_string
implicit none

!Inout data files
 character(80) :: datafile_grid_in  !Grid file
 character(80) :: datafile_bcmap_in !Boundary condition specification file

!Output data file
 character(80) :: datafile_tec      !Tecplot file for viewing the result

!Local variables
 integer       :: i

 write(*,*) "***************************************************************"
 write(*,*) " Starting FVM liuy  "
 write(*,*) "***************************************************************"
 
 ! Input files
 datafile_grid_in  = 'g:\kuaipan\sharebox\liuy0813@gmail.com\Coding\CFD\LYFVM\learnfvm\FVM\0.geo'
 datafile_bcmap_in = 'bump.bcmap'

! Output file
 datafile_tec      = 'bump_solution_tecplot.dat'

 ! (1) Read grid files
      call read_grid(datafile_grid_in, datafile_bcmap_in)
 ! (2) Construct grid data
      call construct_grid_data
      PAUSE
! (3) Check the grid data (It is always good to check them before use!)
  !    call check_grid_data
end program