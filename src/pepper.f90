module pepper
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, pepper!"
  end subroutine say_hello
end module pepper
