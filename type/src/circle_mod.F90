!>
!### MODULE: `circle_mod`
!
! Define the circle shape and
! create methods to obtain its features.
!
module circle_mod

   use ellipse_mod 
   implicit none
   private
   public circle

   type, extends(ellipse) :: circle
      real :: radius
   contains
      procedure :: get_area
      procedure :: get_perimeter
      procedure :: print_circle_info
   end type circle

   interface circle
      procedure new_circle
   end interface circle
   
contains

!>
! Initialize a circle by passing the radius
! and a name.
   function new_circle(radius, name) result(a_circle)
      real, intent(in) :: radius
      character(len=:), allocatable :: name
      type(circle) :: a_circle
      
      a_circle%radius = radius
      a_circle%name = name
   end function new_circle
   
!> 
! Compute the area of a circle using the formula:
! \( A = \pi \times r^2 \)
!
   real function get_area( this )
      class(circle), intent(in) :: this

      get_area =  PI * this%radius**2
   end function get_area
   
!>
! Compute the perimeter of a circle using the formula:
! \( P = 2 \times \pi \times r \)
!
   real function get_perimeter( this )
      class(circle), intent(in) :: this

      get_perimeter =  2.0 * PI * this%radius
   end function get_perimeter

!> 
! Print information (radius, area, perimeter) of a circle.
   subroutine print_circle_info(this)
      class(circle), intent(in) :: this
      area = circle_area(this)  ! Call the circle_area function
      print'(a15)', 'Circle:'
      print'(a18)', '      Radius = ', this%radius
      print'(a18)', '        Area = ', get_area(this)
      print'(a18)', '   Perimeter = ', get_perimeter(this)
      print *, 'Circle: r = ', this%radius, ' area = ', area
   end subroutine print_circle_info

end module circle_mod
