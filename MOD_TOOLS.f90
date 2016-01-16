! some tools from idolikecfd.
module mod_constants
  implicit none
  
  private
  
  public :: p2
  public :: zero, one, two, three, four, five, six, seven, eight, nine
  public :: ten, eleven
  public :: half, third, fourth, fifth, sixth, two_third, four_third
  public :: three_fourth, twelfth, pi, one_twentyfourth
  
  integer , parameter :: sp = kind(1.0)
  integer , parameter :: p2 = selected_real_kind(2*precision(1.0_sp))
  
  real(p2), parameter :: zero            = 0.0_p2,                 &
                        one              = 1.0_p2,                 &
                        two              = 2.0_p2,                 &
                        three            = 3.0_p2,                 &
                        four             = 4.0_p2,                 &
                        five             = 5.0_p2,                 &
                        six              = 6.0_p2,                 &
                        seven            = 7.0_p2,                 &
                        eight            = 8.0_p2,                 &
                        nine             = 9.0_p2,                 &
                        ten              = 10.0_p2,                &
                        eleven           = 11.0_p2,                &
                        half             = 0.5_p2,                 &
                        third            = 1.0_p2/ 3.0_p2,         &
                        fourth           = 1.0_p2/ 4.0_p2,         &
                        fifth            = 1.0_p2/ 5.0_p2,         &
                        sixth            = 1.0_p2/ 6.0_p2,         &
                        two_third        = 2.0_p2/ 3.0_p2,         &
                        four_third       = 4.0_p2/ 3.0_p2,         &
                        three_fourth     = 3.0_p2/ 4.0_p2,         &
                        twelfth          = 1.0_p2/12.0_p2,         &
                        one_twentyfourth = 1.0_p2/24.0_p2
  real(p2), parameter :: pi = 3.141592653589793238_p2

end module mod_constants
 !* 2. module grid_data_type
!*
!* This module defines custom grid data types for unstructured grids
!*
!* NOTE: These data types are designed to make it easier to understand the code.
!*       They may not be the best in terms of efficiency.
!*
!* NOTE: Custom grid data types (derived types) are very useful.
!*       For example, if I declare a variable, "a", by the statemant:
!*           type(node_type), dimension(100) :: a
!*       The variable, a, is a 1D array each component of which contains all data
!*       defined as below. These data can be accessed by %, e.g.,
!*           a(1)%x, a(1)%y, a(1)%nghbr(1:nnghbrs), etc.
!*       In C-programming, this type of data is called "structure", I think.
 module mod_grid_data_type

  use mod_constants, only : p2

  implicit none

  private

  public ::  node_type
  public ::   elm_type
  public ::  edge_type
  public :: bgrid_type
  public ::  face_type
  public ::   jac_type

!----------------------------------------------------------
! Data type for nodal quantities (used for node-centered schemes)
! Note: Each node has the following data.
!----------------------------------------------------------
  type node_type
!  to be read from a grid file
   real(p2)                          :: x, y      !nodal coordinates
   real(p2)                          :: z         !nodal depth
!  to be constructed in the code
   integer                           :: nnghbrs   !number of neighbors
   integer,   dimension(:), pointer  :: nghbr     !list of neighbors
   integer                           :: nelms     !number of elements
   integer,   dimension(:), pointer  :: elm       !list of elements
   real(p2)                          :: vol       !dual-cell volume
   integer                           :: bmark     !Boundary mark
   integer                           :: nbmarks   !# of boundary marks
!  to be computed in the code
   !Below are arrays always allocated.
   real(p2), dimension(:)  , pointer :: u         !conservative variables
   real(p2), dimension(:)  , pointer :: uexact    !conservative variables
   real(p2), dimension(:,:), pointer :: gradu     !gradient of u
   real(p2), dimension(:)  , pointer :: res       !residual (rhs)
   real(p2)                          :: ar        ! Control volume aspect ratio
   real(p2), dimension(:)  , pointer :: lsq2x2_cx !    Linear LSQ coefficient for wx
   real(p2), dimension(:)  , pointer :: lsq2x2_cy !    Linear LSQ coefficient for wy
   real(p2), dimension(:)  , pointer :: lsq5x5_cx ! Quadratic LSQ coefficient for wx
   real(p2), dimension(:)  , pointer :: lsq5x5_cy ! Quadratic LSQ coefficient for wy
   real(p2), dimension(:  ), pointer :: dx, dy    ! Extra data used by Quadratic LSQ
   real(p2), dimension(:,:), pointer :: dw        ! Extra data used by Quadratic LSQ

   !Below are optional: Pointers need to be allocated in the main program if necessary.
   real(p2), dimension(:)  , pointer :: du        !change in conservative variables
   real(p2), dimension(:)  , pointer :: w         !primitive variables(optional)
   real(p2), dimension(:,:), pointer :: gradw     !gradient of w
   real(p2)                          :: phi       !limiter function (0 <= phi <= 1)
   real(p2)                          :: dt        !local time step
   real(p2)                          :: wsn       !Half the max wave speed at face
   real(p2), dimension(:), pointer   :: r_temp    ! For GCR implementation
   real(p2), dimension(:), pointer   :: u_temp    ! For GCR implementation
   real(p2), dimension(:), pointer   :: w_temp    ! For GCR implementation

  end type node_type
  !----------------------------------------------------------
! Data type for element/cell quantities (used for cell-centered schemes)
! Note: Each element has the following data.
!----------------------------------------------------------
  type elm_type
!  to be read from a grid file
   integer                           :: nvtx     !number of vertices
   integer,   dimension(:), pointer  :: vtx      !list of vertices
   integer                           :: imt      ! material of element
!  to be constructed in the code
   integer                           :: nnghbrs  !number of neighbors
   integer,   dimension(:), pointer  :: nghbr    !list of neighbors
   real(p2)                          :: x, y     !cell center coordinates
   real(p2)                          :: vol      !cell volume

   integer,  dimension(:)  , pointer :: edge     !list of edges
   real(p2), dimension(:)  , pointer :: u        !conservative variables
   real(p2), dimension(:)  , pointer :: uexact   !conservative variables
!NotUsed   real(p2), dimension(:)  , pointer :: du       !change in conservative variables
   real(p2), dimension(:,:), pointer :: gradu    !gradient of u
   real(p2), dimension(:)  , pointer :: res      !residual (rhs)
   real(p2)                          :: dt       !local time step
   real(p2)                          :: wsn      !
   integer                           :: bmark    !Boundary mark
   integer                           :: nvnghbrs !number of vertex neighbors
   integer,  dimension(:), pointer   :: vnghbr   !list of vertex neighbors
   real(p2)                          :: ar       !Element volume aspect ratio
   real(p2), dimension(:) , pointer  :: lsq2x2_cx!Linear LSQ coefficient for ux
   real(p2), dimension(:) , pointer  :: lsq2x2_cy!Linear LSQ coefficient for uy

  end type elm_type
!----------------------------------------------------------
! Data type for edge quantities (used for node-centered scheemes)
! Note: Each edge has the following data.
!----------------------------------------------------------
  type edge_type
!  to be constructed in the code
   integer                          :: n1, n2 !associated nodes
   integer                          :: e1, e2 !associated elements
   real(p2),           dimension(2) :: dav    !unit directed-area vector
   real(p2)                         :: da     !magnitude of the directed-area vector
   real(p2),           dimension(2) :: ev     !unit edge vector
   real(p2)                         :: e      !magnitude of the edge vector
   integer                          :: kth_nghbr_of_1 !neighbor index
   integer                          :: kth_nghbr_of_2 !neighbor index
  end type edge_type

  !----------------------------------------------------------
! Data type for boundary quantities (for both node/cell-centered schemes)
! Note: Each boundary segment has the following data.
!----------------------------------------------------------
  type bgrid_type
!  to be read from a boundary grid file
   character(80)                    :: bc_type !type of boundary condition
   integer                          :: nbnodes !# of boundary nodes
   integer,   dimension(:), pointer :: bnode   !list of boundary nodes
!  to be constructed in the code
   integer                          :: nbfaces !# of boundary faces
   real(p2),  dimension(:), pointer :: bfnx    !x-component of the face outward normal
   real(p2),  dimension(:), pointer :: bfny    !y-component of the face outward normal
   real(p2),  dimension(:), pointer :: bfn     !magnitude of the face normal vector
   real(p2),  dimension(:), pointer :: bnx     !x-component of the outward normal
   real(p2),  dimension(:), pointer :: bny     !y-component of the outward normal
   real(p2),  dimension(:), pointer :: bn      !magnitude of the normal vector
   integer ,  dimension(:), pointer :: belm    !list of elm adjacent to boundary face
   integer ,  dimension(:), pointer :: kth_nghbr_of_1
   integer ,  dimension(:), pointer :: kth_nghbr_of_2
  end type bgrid_type
!----------------------------------------------------------
! Data type for face quantities (used for cell-centered schemes)
!
! A face is defined by a line segment connecting two nodes.
! The directed area is defined as a normal vector to the face,
! pointing in the direction from e1 to e2.
!
!      n2
!       o------------o
!     .  \         .
!    .    \   e2  .
!   .  e1  \    .
!  .        \ .         Directed area is positive: n1 -> n2
! o----------o         e1: left element
!             n1       e2: right element (e2 > e1 or e2 = 0)
!
! Note: Each face has the following data.
!----------------------------------------------------------
  type face_type
! to be constructed in the code (NB: boundary faces are excluded.)
   integer                         :: n1, n2 !associated nodes
   integer                         :: e1, e2 !associated elements
   real(p2),          dimension(2) :: dav    !unit directed-area vector
   real(p2)                        :: da     !magnitude of the directed-area vector
  end type face_type

    type jac_type
!  to be constructed in the code
   real(p2),  dimension(:,:)  , pointer :: diag ! diagonal block of Jacobian matrix
   real(p2),  dimension(:,:,:), pointer :: off  ! off-diagonal block of Jacobian matrix
  end type jac_type


 end module mod_grid_data_type
 !* 3. module my_main_data 
!*
!* This module defines the main data that will be used in the code.
!
module mod_my_main_data

  use mod_constants     , only : p2, one
  use mod_grid_data_type, only : node_type, elm_type, edge_type, bgrid_type, face_type, jac_type

  implicit none

  private

  public :: nnodes, node
  public :: ntria, nelms, elm
  public :: nedges, edge
  public :: nbound, bound
  public :: nfaces, face

  public :: nq
  public :: gradient_type, gradient_weight, gradient_weight_p
  public :: inviscid_flux, inviscid_jac 
  public :: M_inf, rho_inf, u_inf, v_inf, p_inf 
  public :: gamma

  public :: tolerance, max_iterations
  public :: jac
  public :: iteration_method
  public :: CFL, CFLexp, CFL1, CFL2, CFL_ramp_steps
  public :: sweeps

!  Parameters

   !Number of equtaions/variables in the target equtaion.
   integer       :: nq 

   !LSQ gradient related parameteres:
   character(80) ::     gradient_type  ! "linear"; or for node-centered schemes can use "quadratic2"
   character(80) ::    gradient_weight ! "none" or "inverse_distance"
   real(p2)      :: gradient_weight_p  !  1.0  or any other real value

   !Scheme parameters
   character(80) :: inviscid_flux !Numerial flux for the inviscid terms (Euler)

   !Reference quantities
   real(p2) :: M_inf, rho_inf, u_inf, v_inf, p_inf

   !Ratio of specific heats = 1.4 fpr air
   real(p2) :: gamma = 1.4_p2

   !Parameter for explicit scheme
   real(p2) :: CFLexp              ! Input CFL number for explicit RK2 scheme

   !Implicit solver parameters
    integer       :: max_iterations      ! Maximum number of iterations
    real(p2)      :: tolerance           ! Tolerance for steady convergence
    real(p2)      :: CFL                 ! Actual CFL number 
    character(80) :: iteration_method    ! explicit or implicit
    character(80) :: inviscid_jac        ! Inviscid flux for Jacobian
    real(p2)      :: CFL1, CFL2          ! Initial and terminal CFL number for ramping
    integer       :: CFL_ramp_steps      ! Number of iterations to reach CFL2 from CFL1
    integer       :: sweeps              ! Number of GS relaxation

!  Node data
   integer                                 :: nnodes !total number of nodes
   type(node_type), dimension(:), pointer  :: node   !array of nodes

!  Element data (element=cell)
   integer                                 :: ntria  !total number of triangler elements
   integer                                 :: nelms  !total number of elements
   type(elm_type),  dimension(:), pointer  :: elm    !array of elements

!  Edge data
   integer                                 :: nedges !total number of edges
   type(edge_type), dimension(:), pointer  :: edge   !array of edges

!  Boundary data
   integer                                 :: nbound !total number of boundary types
   type(bgrid_type), dimension(:), pointer :: bound  !array of boundary segments

!  Face data (cell-centered scheme only)
   integer                                 :: nfaces !total number of cell-faces
   type(face_type), dimension(:), pointer  :: face   !array of cell-faces

!  Matrix data
   type(jac_type), dimension(:)  , pointer  :: jac   !array of edges

 end module mod_my_main_data

!********************************************************************************
!* 4. module my_allocation
!*
!* This module defines some useful subroutines used for dynamic allocation.
!*
!*  - my_alloc_int_ptr       : Allocate/reallocate an integer 1D array
!*  - my_alloc_p2_ptr        : Allcoate/reallocate a real 1D array
!*  - my_alloc_p2_matrix_ptr : Allcoate/reallocate a real 2D array
!*
!*
!*
!*        written by Dr. Katate Masatsuka (info[at]cfdbooks.com),
!*
!* the author of useful CFD books, "I do like CFD" (http://www.cfdbooks.com).
!*
!* This is Version 0 (July 2015).
!* This F90 code is written and made available for an educational purpose.
!* This file may be updated in future.
!*
!********************************************************************************
 module mod_my_allocation

  implicit none

  private

  public :: my_alloc_int_ptr
  public :: my_alloc_p2_ptr
  public :: my_alloc_p2_matrix_ptr

  contains

!********************************************************************************
!* This subroutine is useful to expand or shrink integer arrays.
!*
!*  Array, x, will be allocated if the requested dimension is 1 (i.e., n=1)
!*  Array, x, will be expanded to the requested dimension, n, if (n > dim(x)).
!*  Array, x, will be shrinked to the requested dimension, n, if (n < dim(x)).
!*
!********************************************************************************
  subroutine my_alloc_int_ptr(x,n)

  implicit none
  integer, intent(in) :: n
  integer, dimension(:), pointer :: x
  integer, dimension(:), pointer :: temp
  integer :: i

  if (n <= 0) then
   write(*,*) "my_alloc_int_ptr received non-positive dimension. Stop."
   stop
  endif

! If not allocated, allocate and return
  if (.not.(associated(x))) then
   allocate(x(n))
   return
  endif

! If reallocation, create a pointer with a target of new dimension.
  allocate(temp(n))
   temp = 0

! (1) Expand the array dimension
  if ( n > size(x) ) then

   do i = 1, size(x)
    temp(i) = x(i)
   end do

! (2) Shrink the array dimension: the extra data, x(n+1:size(x)), discarded.
  else

   do i = 1, n
    temp(i) = x(i)
   end do

  endif

! Destroy the target of x
!  deallocate(x)

! Re-assign the pointer
   x => temp

  return

  end subroutine my_alloc_int_ptr
!********************************************************************************


!********************************************************************************
!* This subroutine is useful to expand or shrink real arrays.
!*
!*  Array, x, will be allocated if the requested dimension is 1 (i.e., n=1)
!*  Array, x, will be expanded to the requested dimension, n, if (n > dim(x)).
!*  Array, x, will be shrinked to the requested dimension, n, if (n < dim(x)).
!*
!********************************************************************************
  subroutine my_alloc_p2_ptr(x,n)

  use edu2d_constants   , only : p2

  implicit none
  integer, intent(in) :: n
  real(p2), dimension(:), pointer :: x
  real(p2), dimension(:), pointer :: temp
  integer :: i

  if (n <= 0) then
   write(*,*) "my_alloc_int_ptr received non-positive dimension. Stop."
   stop
  endif

! If not allocated, allocate and return
  if (.not.(associated(x))) then
   allocate(x(n))
   return
  endif

! If reallocation, create a pointer with a target of new dimension.
  allocate(temp(n))
   temp = 0

! (1) Expand the array dimension
  if ( n > size(x) ) then

   do i = 1, size(x)
    temp(i) = x(i)
   end do

! (2) Shrink the array dimension: the extra data, x(n+1:size(x)), discarded.
  else

   do i = 1, n
    temp(i) = x(i)
   end do

  endif

! Destroy the target of x
  deallocate(x)

! Re-assign the pointer
   x => temp

  return

  end subroutine my_alloc_p2_ptr


!********************************************************************************
!* This subroutine is useful to expand or shrink real arrays.
!*
!*  Array, x, will be allocated if the requested dimension is 1 (i.e., n=1)
!*  Array, x, will be expanded to the requested dimension, n, if (n > dim(x)).
!*  Array, x, will be shrinked to the requested dimension, n, if (n < dim(x)).
!*
!********************************************************************************
  subroutine my_alloc_p2_matrix_ptr(x,n,m)

  use edu2d_constants   , only : p2

  implicit none
  integer, intent(in) :: n, m
  real(p2), dimension(:,:), pointer :: x
  real(p2), dimension(:,:), pointer :: temp
  integer :: i, j

  if (n <= 0) then
   write(*,*) "my_alloc_int_ptr received non-positive dimension. Stop."
   stop
  endif

! If not allocated, allocate and return
  if (.not.(associated(x))) then
   allocate(x(n,m))
   return
  endif

! If reallocation, create a pointer with a target of new dimension.
  allocate(temp(n,m))
   temp = 0.0_p2

  do i = 1, min(n, size(x,1))
   do j = 1, min(m, size(x,2))
    temp(i,j) = x(i,j)
   end do
  end do

! Destroy the target of x
  deallocate(x)

! Re-assign the pointer
   x => temp

  return

  end subroutine my_alloc_p2_matrix_ptr

 end module mod_my_allocation

!********************************************************************************
!* 5. module edu2d_grid_data
!*
!* This module contians subroutines used for reading a grid, constructing
!* additional grid data, and check the grid data.
!*
!*  - my_alloc_int_ptr       : Allocate/reallocate an integer 1D array
!*  - my_alloc_p2_ptr        : Allcoate/reallocate a real 1D array
!*  - my_alloc_p2_matrix_ptr : Allcoate/reallocate a real 2D array
!*
!*
!* Public subroutines:
!*
!*  - read_grid            : Read a grid file, allocate necessary data.
!*  - construct_grid_data  : Construct additional data, allocate more data.
!*  - check_grid_data      : Check the whole grid data.
!*
!* Private functions and subroutines:
!*
!*  - tri_area             : Computed a triangle area
!*  - check_skewness_nc    : Compute the skewness (e*n).
!*  - compute_ar           : Compute aspect ratio at node and element.
!*
!*
!*
!*        written by Dr. Katate Masatsuka (info[at]cfdbooks.com),
!*
!* the author of useful CFD books, "I do like CFD" (http://www.cfdbooks.com).
!********************************************************************************
!* Read the grid and the exact solution.
!* ------------------------------------------------------------------------------
module mod_grid_data

 private

 public :: read_grid
 public :: construct_grid_data
 public :: check_grid_data

 contains
!********************************************************************************
!* Data to be read and stored:
!*
!* 1. Some numbers
!*    nnodes        = Number of nodes
!*    ntria         = Number of triangular elements
!*    nquad         = Number of quadrilateral elements
!*    nelms         = Total number of elements (=ntria+nquad)
!*
!* 2. Element data:
!*    elm(1:nelms)%nvtx   =  Number of vertices of each element
!*    elm(1:nelms)%vtx(:) = Pointer to vertices of each element
!*
!* 3. Node data: nodes are stored in a 1D array
!*    node(1:nnodes)%x     = x-coordinate of the nodes
!*    node(1:nnodes)%y     = y-coordinate of the nodes
!*
!* 4. Boundary Data:
!*    nbound                   = Number of boundary segments
!*    bound(1:nbound)%nbnodes  = Number of nodes in each segment
!*    bound(1:nbound)%bnode(:) = List of node numbers for each segment
!*    bound(1:nbound)%bc_type  = Boundary condition name for each segment
!*    bound(1:nbound)%bc_type  = Boundary condition name for each segment
!*
!********************************************************************************
 subroutine read_grid(datafile_grid_in, datafile_bcmap_in)

 use mod_my_main_data, only : nnodes, node, ntria, nelms, elm, nbound, bound

 implicit none
 character(80), intent(in) :: datafile_grid_in, datafile_bcmap_in
 

!Local variables
 integer         :: i, j, os, dummy_int
 character(len=3)::TEMPSTRING
 integer(p2)     :: TEMPINI
 integer(p2)     :: nnodes,ntria
!--------------------------------------------------------------------------------
! 1. Read grid file>: datafile_grid_in
  ntria=0
  nnodes=0
  write(*,*) "Reading the grid file....", datafile_grid_in
!  Open the input file.
  open(unit=1, file=datafile_grid_in, status="unknown", iostat=os)
  read(1,*) TEMPSTRING
  read(1,*) TEMPSTRING
  read(1,*) TEMPSTRING
  read(1,*) TEMPSTRING
  read(1,*) TEMPSTRING
  do while (.not. eof(1))
    read(1,*) TEMPSTRING
    if(TEMPSTRING.eq.'GE') then
      ntria=ntria+1
    else
      nnodes=nnodes+1
    endif
  enddo
  close(1)
  allocate(node(nnodes))
  allocate(elm(  nelms))
  open(unit=1, file=datafile_grid_in, status="unknown", iostat=os)
  read(1,*) TEMPSTRING
  read(1,*) TEMPSTRING
  read(1,*) TEMPSTRING
  read(1,*) TEMPSTRING
  read(1,*) TEMPSTRING
  do i=1,nelms
    elm(i)%nvtx=3
    allocate(elm(i)%vtx(3))
    read(1,*) TEMPSTRING,TEMPINI,elm(i)%vtx(1),TEMPINI,elm(i)%vtx(2),TEMPINI,elm(i)%vtx(3),TEMPINI,TEMPINI,TEMPINI,elm(i)%imt,TEMPINI
  enddo
  do i=1,nnodes
    read(1,*) TEMPSTRING,TEMPINI,node(i)%x,node(i)%y,node(i)%z
  enddo
  close(1)
!  Write out the grid data.
   write(*,*)
   write(*,*) " Total numbers:"
   write(*,*) "      nodes = ", nnodes
   write(*,*) "  triangles = ", ntria
   write(*,*)
! 
!   attention  ! this part are for boundary condition read
!          left empty first   2016-1-16 22:53:28
!
  end subroutine read_grid

  !********************************************************************************
!* Construct the grid data:
!*
!* The following data, needed for NCFV method, will be constructed based on the
!* data read from the grid file.
!*
!* 1. Element data:
!*    elm(:)%nnghbrs  = Number of element neighbors of each element
!*    elm(:)%nghbr(:) = List of element neighbors of each element
!*    elm(:)%x        = x-coordinate of the centroid
!*    elm(:)%y        = y-coordinate of the centroid
!*    elm(:)%vol      = Volume of the element
!*
!*
!* 2. Node data:
!*    node(:)%nnghbrs = Number of node neighbors of each node
!*    node(:)%nghbr(:)= List of node neighbors of each node
!*    node(:)%nelms   = Number of adjacent elements of each node
!*    node(:)%elm     = List of adjacent elements of each node
!*    node(:)%vol     = Volume of the dual volume around each node
!*
!* 3. Edge data:
!*    edge(:)%n1, n2  = End nodes of each edge (edge points n1 -> n2)
!*    edge(:)%e1, e2  = Left and right elements of each edge
!*    edge(:)%dav     = Unit directed area vector of each edge
!*    edge(:)%da      = Magnitude of the directed area vector for each edge
!*    edge(:)%ev      = Unit edge vector of each edge (vector n1 -> n2)
!*    edge(:)%e       = Magnitude of the edge vector for each edge
!*
!*
!* 4. Boudnary data
!*    bound(:)%bnx    = Outward normal at boundary nodes (x-component of unit vector)
!*    bound(:)%bny    = Outward normal at boundary nodes (y-component of unit vector)
!*    bound(:)%bn     = Magnitude of (bnx,bny)
!*    NOTE: In this code, the above normal vector at boundary nodes is computed by
!*          a quadratic fit. It is sufficiently accuarte for 3rd-order schemes.
!*          See http://www.hiroakinishikawa.com/My_papers/nishikawa_jcp2015v281pp518-555_preprint.pdf
!*          for details on the quadratic approximation for computing more accurate normals.
!*    bound(:)%bfnx   = Outward normal at boundary nodes (x-component of unit vector)
!*    bound(:)%bfny   = Outward normal at boundary nodes (y-component of unit vector)
!*    bound(:)%bfn    = Magnitude of (bfnx,bfny)
!*    bound(:)%belm   = Element to which the boundary face belongs
!*
!********************************************************************************
subroutine construct_grid_data

use mod_my_main_data , only : nnodes, node, nelms, elm, nedges, edge, nbound, bound, face, nfaces
use mod_constants    , only : p2, zero, half, third
use mod_my_allocation, only : my_alloc_int_ptr, my_alloc_p2_ptr, my_alloc_p2_matrix_ptr

implicit none

!Local variables
integer                       ::  i, j, k, ii, in, im, jelm, v1, v2, v3, v4
real(p2)                      :: x1, x2, x3, x4, y1, y2, y3, y4, xm, ym, xc, yc
real(p2)                      :: xj, yj, xm1, ym1, xm2, ym2, dsL,dsR,dx,dy
logical                       :: found
integer                       :: vL, vR, n1, n2, e1, e2
integer                       :: vt1, vt2, ielm
integer                       :: ave_nghbr, min_nghbr, max_nghbr, imin, imax
integer                       :: iedge
real(p2)                      :: ds

! Some initialization
v2 = 0
vL = 0
im = 0
jelm = 0

write(*,*) "Constructing grid data...."

! Initializations
do i = 1, nnodes
  node(i)%nelms = 0
end do
nedges = 0
!--------------------------------------------------------------------------------
! Loop over elements and construct the fololowing data.
!
! 1. Surrounding elements: node(:)%nelms, node(:)%elm(:)
!
!    Example: Node i is surrounded by the eleemnts, 23, 101, 13, 41.
!             node(i)%nelms = 4
!             node(i)%elm(1) = 23
!             node(i)%elm(2) = 13
!             node(i)%elm(3) = 41
!             node(i)%elm(4) = 101
!
!        o-------o-------------o
!       /        |   .         |
!      /    23   |      41     |
!     o----------o-------------o
!      \        i \            |
!       \   101    \     13    |
!        \          \          | 
!         o----------o---------o
!
! 2. Element quantities  : elm(:)%x,elm(:)%y,elm(:)%vol
!
!  o-----------o            
!   \          |            o
!    \    (x,y)|           / \
!     \   .    |          /   \
!      \       |         /  .  \    (x,y): centroid coordinates
!       \      |        / (x,y) \     vol: volume of element
!        o-----o       o---------o
elements : do i = 1, nelms

   v1 = elm(i)%vtx(1)
   v2 = elm(i)%vtx(2)
   v3 = elm(i)%vtx(3)

   x1 = node(v1)%x
   x2 = node(v2)%x
   x3 = node(v3)%x

   y1 = node(v1)%y
   y2 = node(v2)%y
   y3 = node(v3)%y

! Distribute the element index to nodes.

   node(v1)%nelms = node(v1)%nelms + 1
   call my_alloc_int_ptr(node(v1)%elm, node(v1)%nelms)
   node(v1)%elm(node(v1)%nelms) = i

   node(v2)%nelms = node(v2)%nelms + 1
   call my_alloc_int_ptr(node(v2)%elm, node(v2)%nelms)
   node(v2)%elm(node(v2)%nelms) = i

   node(v3)%nelms = node(v3)%nelms + 1
   call my_alloc_int_ptr(node(v3)%elm, node(v3)%nelms)
   node(v3)%elm(node(v3)%nelms) = i
!   Triangle centroid and volume
    elm(i)%x   = third*(x1+x2+x3)
    elm(i)%y   = third*(y1+y2+y3)
    elm(i)%vol = tri_area(x1,x2,x3,y1,y2,y3)
 end do elements
! Median dual volume

  do i = 1, nnodes
   node(i)%vol = zero
  end do
elementsv : do i = 1, nelms

   v1 = elm(i)%vtx(1)
   v2 = elm(i)%vtx(2)
   v3 = elm(i)%vtx(3)
    node(v1)%vol = node(v1)%vol + third*elm(i)%vol
    node(v2)%vol = node(v2)%vol + third*elm(i)%vol
    node(v3)%vol = node(v3)%vol + third*elm(i)%vol
  end do elementsv
!--------------------------------------------------------------------------------
! Loop over elements 2
!
!  Allocate elm(:)%nghbr(:) : elm(:)%nnghrs, elm(:)%nghr(:)
!  Construct element nghbr data: elm(:)%nghbr(:)
!  Order of neighbor elements [e1,e2,e3,..] are closely related to
!  the order of vertices [v1,v2,v3,..] (see below).
!
!          o------o
!          |      |                
!        v4|  e1  |v3                     v3
!    o-----o------o------o      o---------o------------o
!    |     |      |      |       .      .   .        .
!    | e2  |      |  e4  |        . e2 .     . e1  .
!    o-----o------o------o         .  .       .  .
!       v1 |     .v2              v1 o---------o v2   
!          | e3 .                     .   e3  .
!          |   .                        .    .
!          |  .                           . .
!          | .                             o
!          o
!

! Allocate the neighbor array
  do i = 1, nelms
    elm(i)%nnghbrs = 3
    allocate(elm(i)%nghbr(3))
  end do
elements2 : do i = 1, nelms
   elm_vertex : do k = 1, elm(i)%nvtx  

! Begin constructing the element-neighbor data
!   Get the face of the element i:
!
!             vL      vR
!              o------o
!             /       |
!            /        |
!           o---------o
!
    if (k  < elm(i)%nvtx) vL = elm(i)%vtx(k+1)
    if (k == elm(i)%nvtx) vL = elm(i)%vtx(1)     
    vR = elm(i)%vtx(k)
    !   Loop over the surrounding elements of the node vR,
!   and find the element neighbor from them.
    found = .false.
    elms_around_vR : do j = 1, node(vR)%nelms
    jelm = node(vR)%elm(j)

     edge_matching : do ii = 1, elm(jelm)%nvtx
                   v1 = elm(jelm)%vtx(ii)
      if (ii  > 1) v2 = elm(jelm)%vtx(ii-1)
      if (ii == 1) v2 = elm(jelm)%vtx(elm(jelm)%nvtx)

      if (v1==vR .and. v2==vL) then
       found = .true.
       im = ii+1
       if (im > elm(jelm)%nvtx) im = im - elm(jelm)%nvtx
       exit edge_matching
      endif
     end do edge_matching

     if (found) exit elms_around_vR

    end do elms_around_vR

     in = k + 2
     if (in > elm(i)%nvtx) in = in - elm(i)%nvtx

    if (found) then
     elm(   i)%nghbr(in) = jelm
     elm(jelm)%nghbr(im) = i
    else
     elm(   i)%nghbr(in) = 0
    endif

   end do elm_vertex

  end do elements2
!--------------------------------------------------------------------------------
! Edge-data for node-centered (edge-based) scheme.
!
! Loop over elements 3
! Construct edge data: edge(:)%n1, n2, e1, e2.
! Edge points from node n1 to node n2.
!
!      n2
!       o------------o
!     .  \         .
!    .    \   e2  .
!   .  e1  \    .
!  .        \ .         Directed area is positive: n1 -> n2
! o----------o         e1: left element
!             n1       e2: right element (e2 > e1 or e2 = 0)

! First count the number of edges.
!
! NOTE: Count edges only if the neighbor element number is
!       greater than the current element (i) to avoid double
!       count. Zero element number indicates that it is outside
!       the domain (boundary face).

  elements0 : do i = 1, nelms

   v1 = elm(i)%vtx(1)
   v2 = elm(i)%vtx(2)
   v3 = elm(i)%vtx(3)
    if ( elm(i)%nghbr(3) > i  .or. elm(i)%nghbr(3)==0 ) then
     nedges = nedges + 1
    endif

    if ( elm(i)%nghbr(1) > i .or. elm(i)%nghbr(1)==0 ) then
     nedges = nedges + 1
    endif

    if ( elm(i)%nghbr(2) > i .or. elm(i)%nghbr(2)==0 ) then
     nedges = nedges + 1
    endif
  end do elements0
  ! Allocate the edge array.
  allocate(edge(nedges))
  nedges = 0
  edge(:)%e1 = 0
  edge(:)%e2 = 0
! Construct the edge data:
!  two end nodes (n1, n2), and left and right elements (e1, e2)

  elements3 : do i = 1, nelms

   v1 = elm(i)%vtx(1)
   v2 = elm(i)%vtx(2)
   v3 = elm(i)%vtx(3)
   if ( elm(i)%nghbr(3) > i  .or. elm(i)%nghbr(3)==0 ) then
     nedges = nedges + 1
     edge(nedges)%n1 = v1
     edge(nedges)%n2 = v2
     edge(nedges)%e1 = i
     edge(nedges)%e2 = elm(i)%nghbr(3)
    endif

    if ( elm(i)%nghbr(1) > i .or. elm(i)%nghbr(1)==0 ) then
     nedges = nedges + 1
     edge(nedges)%n1 = v2
     edge(nedges)%n2 = v3
     edge(nedges)%e1 = i
     edge(nedges)%e2 = elm(i)%nghbr(1)
    endif

    if ( elm(i)%nghbr(2) > i .or. elm(i)%nghbr(2)==0 ) then
     nedges = nedges + 1
     edge(nedges)%n1 = v3
     edge(nedges)%n2 = v1
     edge(nedges)%e1 = i
     edge(nedges)%e2 = elm(i)%nghbr(2)
    endif
  end do elements3
! Loop over edges
! Construct edge vector and directed area vector.
!
! Edge vector is a simple vector pointing froom n1 to n2.
! For each edge, add the directed area vector (dav) from
! the left and right elements.
!
!              n2
!   o-----------o-----------o
!   |     dav   |  dav      |
!   |       ^   |   ^       |
!   |       |   |   |       |
!   |   c - - - m - - -c    |
!   |           |           |
!   |           |           |    m: edge midpoint
!   |           |           |    c: element centroid
!   o-----------o-----------o
!                n1
!
  edges : do i = 1, nedges

   n1 = edge(i)%n1
   n2 = edge(i)%n2
   e1 = edge(i)%e1
   e2 = edge(i)%e2
   xm = half*( node(n1)%x + node(n2)%x )
   ym = half*( node(n1)%y + node(n2)%y )

   edge(i)%dav = zero

! Contribution from the left element
  if (e1 > 0) then
   xc = elm(e1)%x
   yc = elm(e1)%y
   edge(i)%dav(1) = -(ym-yc)
   edge(i)%dav(2) =   xm-xc
  endif

! Contribution from the right element
  if (e2 > 0) then
   xc = elm(e2)%x
   yc = elm(e2)%y
   edge(i)%dav(1) = edge(i)%dav(1) -(yc-ym)
   edge(i)%dav(2) = edge(i)%dav(2) + xc-xm
  endif

  if (e1 < 0 .and. e2 < 0) then
   write(*,*) "!!!!! e1 and e2 are both negative... No way..."
  endif

! Magnitude and unit vector
   edge(i)%da  = sqrt( edge(i)%dav(1)**2 + edge(i)%dav(2)**2 )
   edge(i)%dav = edge(i)%dav / edge(i)%da

! Edge vector

  edge(i)%ev(1) = node(n2)%x - node(n1)%x
  edge(i)%ev(2) = node(n2)%y - node(n1)%y
  edge(i)%e     = sqrt( edge(i)%ev(1)**2 + edge(i)%ev(2)**2 )
  edge(i)%ev    = edge(i)%ev / edge(i)%e

  end do edges

!--------------------------------------------------------------------------------
! Construct node neighbor data:
!  pointers to the neighbor nodes(o)
!
!        o     o
!         \   / 
!          \ /
!     o-----*-----o
!          /|
!         / |
!        /  o        *: node in interest
!       o            o: neighbors (edge-connected nghbrs)
!

  do i = 1, nnodes
   node(i)%nnghbrs = 0
  end do
  Loop over edges and distribute the node numbers:

  edges4 : do i = 1, nedges

   n1 = edge(i)%n1
   n2 = edge(i)%n2

! (1) Add node1 to the neighbor list of n2
   node(n1)%nnghbrs = node(n1)%nnghbrs + 1
   call my_alloc_int_ptr(node(n1)%nghbr, node(n1)%nnghbrs)
   node(n1)%nghbr(node(n1)%nnghbrs) = n2

! (2) Add node2 to the neighbor list of n1
   node(n2)%nnghbrs = node(n2)%nnghbrs + 1
   call my_alloc_int_ptr(node(n2)%nghbr, node(n2)%nnghbrs)
   node(n2)%nghbr(node(n2)%nnghbrs) = n1

  end do edges4 

!********************************************************************************
!* Compute the area of the triangle defined by the nodes, 1, 2, 3.
!*
!*              3 (x3,y3)
!*              o 
!*             / \ 
!*            /   \
!* (x1,y1) 1 o-----o 2 (x2,y2)
!*
!* Nodes must be ordered counterclockwise (otherwise it gives negative area)
!*
!********************************************************************************
 function tri_area(x1,x2,x3,y1,y2,y3) result(area)
 use edu2d_constants, only : p2, half
 implicit none
 real(p2), intent(in) :: x1,x2,x3,y1,y2,y3
 real(p2) :: area

  area = half*( x1*(y2-y3) + x2*(y3-y1) + x3*(y1-y2) )

 end function tri_area
