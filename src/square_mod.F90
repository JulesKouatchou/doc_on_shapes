!>
!### MODULE: `square_mod`
!
! Define the square shape and
! create methods to obtain its features.
!
module square_mod

   use rectangle_mod   
   implicit none
   private
   public square
   
   type, extends(rectangle) :: square
   contains
      procedure :: get_area
      procedure :: get_perimeter
   end type square

   interface square
      procedure new_square
   end interface square
   
contains
   
!>
! Initialize a square by passing the
! the length and a name.
   function new_square(length, name) result(a_square)
      real, intent(in) :: length
      character(len=:), allocatable :: name
      type(square) :: a_square
      
      a_square%length = length
      a_square%name = name
      
   end function new_square
   
!>
! Deterime the area of q square.
   real function get_area( this )
      class(square), intent(in) :: this

      get_area = this%length * this%length
   end function get_area

!>
! Deterime the perimeter of q square.
   real function get_perimeter( this )
      class(square), intent(in) :: this

      get_perimeter = 4.0 * this%length
   end function get_perimeter

end module square_mod
