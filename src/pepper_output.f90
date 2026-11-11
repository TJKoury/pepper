!> Text output to the screen or to files: SPICELIB's [[wrline]] and [[clline]], and
!> the program exit of [[byebye]].
!>
!> A device is 'SCREEN' (standard output), 'NULL' (nothing is written) or
!> the name of a file. A file that is not open yet is opened as a new file
!> (with a unit from NEWUNIT=); lines are appended to it while it stays open,
!> and [[clline]] closes it. These routines report their own I/O problems on
!> standard output, as SPICELIB's do (they are used to report errors, so
!> they cannot signal any).
module pepper_output

   use pepper_kinds, only: ip

   implicit none

   private
   public :: wrline, clline, byebye

contains

   !> Write a line to a device: the screen, a file, or nowhere.
   subroutine wrline(device, line)
      character(*), intent(in) :: device  !! 'SCREEN', 'NULL', or the name of a file
      character(*), intent(in) :: line  !! the line (written without its trailing blanks)
      character(len(device)) :: dev
      integer :: unit, ios
      logical :: opened, exists
      dev = adjustl(device)
      if (upper(dev) == 'NULL') return
      if (upper(dev) == 'SCREEN') then
         write (*, '(a)', iostat=ios) trim(line)
         return
      end if
      inquire (file=trim(dev), opened=opened, exist=exists, number=unit, iostat=ios)
      if (ios /= 0) then
         write (*, '(a)') 'SPICE(INQUIREFAILED)', 'WRLINE: File = ' // trim(dev)
         return
      end if
      if (.not. (opened .and. exists)) then
         open (newunit=unit, file=trim(dev), status='new', action='write', iostat=ios)
         if (ios /= 0) then
            write (*, '(a)') 'SPICE(FILEOPENFAILED)', ' ', &
               'WRLINE: An error occurred while attempting to open ' // trim(dev) // '.'
            return
         end if
      end if
      write (unit, '(a)', iostat=ios) trim(line)
      if (ios /= 0) write (*, '(a)') 'WRLINE: An error occurred while attempting to WRITE to ' // &
         trim(dev) // '.'
   end subroutine wrline

   !> Close a file that wrline wrote to.
   subroutine clline(device)
      character(*), intent(in) :: device  !! the name of the file
      integer :: unit, ios
      logical :: opened
      inquire (file=trim(adjustl(device)), opened=opened, number=unit, iostat=ios)
      if (ios /= 0) then
         write (*, '(a)') 'SPICE(INQUIREFAILED)', 'CLLINE:  File = ' // trim(device)
         return
      end if
      if (opened) close (unit)
   end subroutine clline

   !> End the program with a status: 'SUCCESS' (exit status 0) or anything
   !> else (exit status 1). Nothing is printed.
   subroutine byebye(status)
      character(*), intent(in) :: status  !! 'SUCCESS' or 'FAILURE'
      if (upper(adjustl(status)) == 'SUCCESS') then
         stop 0, quiet=.true.
      else
         error stop 1, quiet=.true.
      end if
   end subroutine byebye

   !> Upper case.
   pure function upper(s) result(u)
      character(*), intent(in) :: s  !! a string
      character(len(s)) :: u
      integer :: i
      u = s
      do i = 1, len(s)
         if (s(i:i) >= 'a' .and. s(i:i) <= 'z') u(i:i) = achar(iachar(s(i:i)) - 32)
      end do
   end function upper

end module pepper_output
