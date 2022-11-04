!>
!### MODULE: `triangle_mod`
!
! Define the triangle shape and 
! create methods to obtain its features.
!
module triangle_mod

   use polygon_mod   
   implicit none
   private
   public triangle
   
   type, extends(polygon) :: triangle
      real :: a       !! opposite
      real :: b       !! adjacent
      real :: c       !! hypotenus
   contains
      procedure :: get_area
      procedure :: get_perimeter
   end type triangle

   interface triangle
      procedure new_triangle
   end interface triangle
   
contains
   
!>
! Initialize an arbitrary triangle by passing the
! the three sides (opposite, adjacent and hypotenus) and
! a name.
   function new_triangle(a, b, c, name) result(a_triangle)
      real, intent(in) :: a !! opposite
      real, intent(in) :: b !! adjacent
      real, intent(in) :: c !! hypotenuse
      character(len=:), allocatable :: name
      type(triangle) :: a_triangle
      
      a_triangle%a = a
      a_triangle%b = b
      a_triangle%c = c
      a_triangle%name = name
   end function new_triangle
   
!>
! Compute the area of a triangle using the Heron's formula:
!
!$$
!\begin{align} 
!s & =  \frac{a+b+c}{2} \\
!A & =  \sqrt{s(s-a)(s-b)(s-c)}
!\end{align}
!$$
!
   real function get_area( this )
      class(triangle), intent(in) :: this
      real :: s
      s = (this%a + this%b + this%c) / 2.0
      get_area = sqrt(s*(s-this%a)*(s-this%b)*(s-this%c))
   end function get_area
   
!>
! Compute the perimeter of a triangle.
   real function get_perimeter( this )
      class(triangle), intent(in) :: this
      get_perimeter = this%a + this%b + this%c
   end function get_perimeter

!>
! Determine if a triangle is a right one by
! checking if the Pythagorean formula is true:
! $$ c^2 = a^2 + b^2 $$
   logical function is_right_triangle( this )
      class(triangle), intent(in) :: this
      is_right_triangle = .FALSE.
      if ( (this%a**2 + this%b**2) == this%c**2 ) is_right_triangle = .TRUE.
   end function is_right_triangle
   
end module triangle_mod
