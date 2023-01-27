!>
!### MODULE: `shape_class`
!
! Parent class that will be used to define various geometric shapes.
!
! For each define shape, we want to be to determine its area and perimeter.
!
!@todo
! Make the length and width PRIVATE for a square/rectangle.
!@endtodo
!
!@bug
! Will fix as they arise.
!@endbug
!
module shapes_mod
   use MAPL
   implicit none
   private
   public shape
   
   type, abstract :: shape
      !! Base class for geometric shape types
      character(len=:), allocatable :: name
   contains
      procedure(get_shape_area), deferred  :: get_area
      procedure(get_shape_perimeter), deferred  :: get_perimeter
      procedure, non_overridable :: get_name
   end type shape

   abstract interface
      real function get_shape_area( this )
         !! Interface for determining the area.
         import                   :: shape
         class(shape), intent(in) :: this
      end function get_shape_area
   end interface

   abstract interface
      real function get_shape_perimeter( this )
         !! Interface for determining the perimeter.
         import                   :: shape
         class(shape), intent(in) :: this
      end function get_shape_perimeter
   end interface

contains

!> 
! Get the name of any defined shape.
   function get_name( this ) result(name)
      class(shape), intent(in) :: this
      character(len=:), allocatable :: name
      name = this%name
   end function get_name

end module shapes_mod
