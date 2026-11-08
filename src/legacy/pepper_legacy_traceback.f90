! Legacy interface to SPICELIB's traceback routines (the TRCPKG entry
! points). PEPPER has no traceback: these do nothing, and the traceback
! is always empty. They exist so that existing callers (such as the tspice
! tests) still link.

      !> Check in a module name (no-op: there is no traceback).
      subroutine chkin ( module )
      implicit none
      character(*), intent(in) :: module  !! module name
      end subroutine chkin

      !> Check out a module name (no-op: there is no traceback).
      subroutine chkout ( module )
      implicit none
      character(*), intent(in) :: module  !! module name
      end subroutine chkout

      !> Depth of the traceback: always 0.
      subroutine trcdep ( depth )
      use pepper_kinds, only: ip
      implicit none
      integer(ip), intent(out) :: depth  !! number of names in the traceback (0)
      depth = 0
      end subroutine trcdep

      !> Maximum depth of the traceback reached: always 0.
      subroutine trcmxd ( depth )
      use pepper_kinds, only: ip
      implicit none
      integer(ip), intent(out) :: depth  !! maximum traceback depth (0)
      depth = 0
      end subroutine trcmxd

      !> Name at a position in the traceback: always blank.
      subroutine trcnam ( index, name )
      use pepper_kinds, only: ip
      implicit none
      integer(ip), intent(in) :: index  !! position in the traceback
      character(*), intent(out) :: name  !! module name (blank)
      name = ' '
      end subroutine trcnam

      !> The traceback as a string: always blank.
      subroutine qcktrc ( trace )
      implicit none
      character(*), intent(out) :: trace  !! traceback (blank)
      trace = ' '
      end subroutine qcktrc

      !> Freeze the traceback (no-op).
      subroutine freeze
      implicit none
      end subroutine freeze

      !> Turn off tracing (no-op).
      subroutine trcoff
      implicit none
      end subroutine trcoff
