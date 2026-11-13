!> The legacy interface's routines of the subsystem errsys (40 routines):
!> SPICELIB's names and arguments, on the default context. Programs use
!> them through [[pepper_legacy]].
module pepper_legacy_errsys

   use pepper_instance, only: pepper_context
   use pepper_legacy_context, only: legacy_context, legacy_spice

   implicit none

   private
   public :: sigerr, setmsg, errch, errdp, errint, errfnm, errhan, reset, getmsg, erract, errdev, &
      errprt, outmsg, failed, return, seterr, accept, allowd, getsms, putsms, getlms, putlms, &
      getdev, putdev, getact, putact, setprt, msgsel, prtpkg, getlun, fndlun, reslun, frelun, &
      zzshsh, zzhash, zzhash2, zzphsh, wrline, clline, byebye

contains

      !> Signal an error.
      !>
      !> Legacy interface: acts on the legacy context.
      subroutine sigerr ( msg )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: msg  !! the short message
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%sigerr ( msg )
      end subroutine sigerr

      !> Set the long error message.
      !>
      !> Legacy interface: acts on the legacy context.
      subroutine setmsg ( msg )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: msg  !! the long message
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%setmsg ( msg )
      end subroutine setmsg

      !> Replace a marker in the long error message with a string.
      !>
      !> Legacy interface: acts on the legacy context.
      subroutine errch ( marker, string )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: marker  !! the marker
      character(*) :: string  !! the replacement
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%errch ( marker, string )
      end subroutine errch

      !> Replace a marker in the long error message with a number.
      !>
      !> Legacy interface: acts on the legacy context.
      subroutine errdp ( marker, dpnum )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: marker  !! the marker
      real(wp) :: dpnum  !! the number
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%errdp ( marker, dpnum )
      end subroutine errdp

      !> Replace a marker in the long error message with an integer.
      !>
      !> Legacy interface: acts on the legacy context.
      subroutine errint ( marker, intnum )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: marker  !! the marker
      integer(ip) :: intnum  !! the integer
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%errint ( marker, intnum )
      end subroutine errint

      !> Replace a marker in the long error message with the name of the file connected to a unit.
      !>
      !> Legacy interface: acts on the legacy context.
      subroutine errfnm ( marker, unit )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: marker  !! the marker
      integer(ip) :: unit  !! the logical unit
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%errfnm ( marker, unit )
      end subroutine errfnm

      !> Replace a marker in the long error message with the name of the file with a handle.
      !>
      !> Legacy interface: acts on the legacy context.
      subroutine errhan ( marker, handle )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: marker  !! the marker
      integer(ip) :: handle  !! the DAF or DAS handle
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%errhan ( marker, handle )
      end subroutine errhan

      !> Clear the error status and the messages.
      !>
      !> Legacy interface: acts on the legacy context.
      subroutine reset
      use pepper_kinds, only: ip, wp
      implicit none
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%reset ()
      end subroutine reset

      !> Get an error message.
      !>
      !> Legacy interface: acts on the legacy context.
      subroutine getmsg ( option, msg )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: option  !! 'SHORT', 'LONG' or 'EXPLAIN'
      character(*) :: msg  !! the message
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%getmsg ( option, msg )
      end subroutine getmsg

      !> Get or set the error action.
      !>
      !> Legacy interface: acts on the legacy context.
      subroutine erract ( op, action )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: op  !! 'GET' or 'SET'
      character(*) :: action  !! the action
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%erract ( op, action )
      end subroutine erract

      !> Get or set the error output device.
      !>
      !> Legacy interface: acts on the legacy context.
      subroutine errdev ( op, device )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: op  !! 'GET' or 'SET'
      character(*) :: device  !! the device
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%errdev ( op, device )
      end subroutine errdev

      !> Get or set the parts of the error reports.
      !>
      !> Legacy interface: acts on the legacy context.
      subroutine errprt ( op, list )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: op  !! 'GET' or 'SET'
      character(*) :: list  !! the list of report parts
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%errprt ( op, list )
      end subroutine errprt

      !> Write an error report.
      !>
      !> Legacy interface: acts on the legacy context.
      subroutine outmsg ( list )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: list  !! the parts of the report
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%outmsg ( list )
      end subroutine outmsg

      !> True if an error was signaled.
      !>
      !> Legacy interface: acts on the legacy context.
      logical function failed ()
      use pepper_kinds, only: ip, wp
      implicit none
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      failed = ctx%failed ()
      end function failed

      !> True if an error was signaled and the action is RETURN.
      !>
      !> Legacy interface: acts on the legacy context.
      logical function return ()
      use pepper_kinds, only: ip, wp
      implicit none
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      return = ctx%return ()
      end function return

      !> Set the error status.
      !>
      !> Legacy interface: acts on the legacy context.
      logical function seterr ( status )
      use pepper_kinds, only: ip, wp
      implicit none
      logical :: status  !! the error status
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      seterr = ctx%seterr ( status )
      end function seterr

      !> Allow or forbid changes of the long message.
      !>
      !> Legacy interface: acts on the legacy context.
      logical function accept ( ok )
      use pepper_kinds, only: ip, wp
      implicit none
      logical :: ok  !! changes of the long message are allowed
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      accept = ctx%accept ( ok )
      end function accept

      !> True if changes of the long message are allowed.
      !>
      !> Legacy interface: acts on the legacy context.
      logical function allowd ()
      use pepper_kinds, only: ip, wp
      implicit none
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      allowd = ctx%allowd ()
      end function allowd

      !> Get the short message (SPICELIB's GETSMS).
      !>
      !> Legacy interface: acts on the legacy context.
      subroutine getsms ( msg )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: msg  !! the short message
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%getsms ( msg )
      end subroutine getsms

      !> Store the short message (SPICELIB's PUTSMS).
      !>
      !> Legacy interface: acts on the legacy context.
      subroutine putsms ( msg )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: msg  !! the short message
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%putsms ( msg )
      end subroutine putsms

      !> Get the long message (SPICELIB's GETLMS).
      !>
      !> Legacy interface: acts on the legacy context.
      subroutine getlms ( msg )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: msg  !! the long message
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%getlms ( msg )
      end subroutine getlms

      !> Store the long message (SPICELIB's PUTLMS).
      !>
      !> Legacy interface: acts on the legacy context.
      subroutine putlms ( msg )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: msg  !! the long message
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%putlms ( msg )
      end subroutine putlms

      !> Get the device (SPICELIB's GETDEV).
      !>
      !> Legacy interface: acts on the legacy context.
      subroutine getdev ( msg )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: msg  !! the device
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%getdev ( msg )
      end subroutine getdev

      !> Store the device (SPICELIB's PUTDEV).
      !>
      !> Legacy interface: acts on the legacy context.
      subroutine putdev ( msg )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: msg  !! the device
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%putdev ( msg )
      end subroutine putdev

      !> Get the error action code (SPICELIB's GETACT).
      !>
      !> Legacy interface: acts on the legacy context.
      subroutine getact ( action )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: action  !! the action code
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%getact ( action )
      end subroutine getact

      !> Store the error action code (SPICELIB's PUTACT).
      !>
      !> Legacy interface: acts on the legacy context.
      subroutine putact ( action )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: action  !! the action code
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%putact ( action )
      end subroutine putact

      !> Select the parts of the error reports.
      !>
      !> Legacy interface: acts on the legacy context.
      logical function setprt ( short, expl, long, trace, dfault )
      use pepper_kinds, only: ip, wp
      implicit none
      logical :: short  !! the short message
      logical :: expl  !! the explanation
      logical :: long  !! the long message
      logical :: trace  !! the traceback
      logical :: dfault  !! the default note
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      setprt = ctx%setprt ( short, expl, long, trace, dfault )
      end function setprt

      !> True if a part of the error reports is selected.
      !>
      !> Legacy interface: acts on the legacy context.
      logical function msgsel ( type )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: type  !! the part
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      msgsel = ctx%msgsel ( type )
      end function msgsel

      !> Umbrella of SETPRT and MSGSEL: calling it directly is an error, as in SPICELIB.
      !>
      !> Legacy interface: acts on the legacy context.
      logical function prtpkg ( short, long, expl, trace, dfault, type )
      use pepper_kinds, only: ip, wp
      implicit none
      logical :: short, long, expl, trace, dfault  !! (unused)
      character(*) :: type  !! (unused)
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%setmsg ( 'PRTPKG:  You have called an entry point which has no run-time function; this may indicate a program bug.  Please check the PRTPKG documentation.' )
      call ctx%sigerr ( 'SPICE(BOGUSENTRY)' )
      prtpkg = .false.
      end function prtpkg

      !> A free logical unit (error if none).
      !>
      !> Legacy interface: acts on the legacy context.
      subroutine getlun ( unit )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: unit  !! the unit
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%getlun ( unit )
      end subroutine getlun

      !> A free logical unit (0 if none).
      !>
      !> Legacy interface: acts on the legacy context.
      subroutine fndlun ( unit )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: unit  !! the unit
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%fndlun ( unit )
      end subroutine fndlun

      !> Reserve a logical unit.
      !>
      !> Legacy interface: acts on the legacy context.
      subroutine reslun ( unit )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: unit  !! the unit
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%reslun ( unit )
      end subroutine reslun

      !> Free a reserved logical unit.
      !>
      !> Legacy interface: acts on the legacy context.
      subroutine frelun ( unit )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: unit  !! the unit
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%frelun ( unit )
      end subroutine frelun

      !> Set the divisor of ZZHASH.
      !>
      !> Legacy interface: acts on the legacy context.
      integer(ip) function zzshsh ( m )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: m  !! the divisor
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzshsh = ctx%zzshsh ( m )
      end function zzshsh

      !> The hash value of a word.
      !>
      !> Legacy interface: acts on the legacy context.
      integer(ip) function zzhash ( word )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: word  !! the word
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzhash = ctx%zzhash ( word )
      end function zzhash

      !> The hash value of a word, with a divisor.
      !>
      !> Legacy interface: acts on the legacy context.
      integer(ip) function zzhash2 ( word, m2 )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: word  !! the word
      integer(ip) :: m2  !! the divisor
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzhash2 = ctx%zzhash2 ( word, m2 )
      end function zzhash2

      !> Umbrella of ZZSHSH, ZZHASH and ZZHASH2: does nothing, as in SPICELIB.
      !>
      !> Legacy interface: acts on the legacy context.
      integer(ip) function zzphsh ( word, m, m2 )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: word  !! (unused)
      integer(ip) :: m, m2  !! (unused)
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzphsh = 0
      end function zzphsh

      !> Write a line to a device ('SCREEN', 'NULL' or a file).
      !>
      !> Legacy interface: calls wrline of module pepper_output.
      subroutine wrline ( device, line )
      use pepper_output, only: pepper_wrline => wrline
      implicit none
      character(*) :: device  !! the device
      character(*) :: line  !! the line
      call pepper_wrline ( device, line )
      end subroutine wrline

      !> Close a file written by WRLINE.
      !>
      !> Legacy interface: calls clline of module pepper_output.
      subroutine clline ( device )
      use pepper_output, only: pepper_clline => clline
      implicit none
      character(*) :: device  !! the file
      call pepper_clline ( device )
      end subroutine clline

      !> End the program: status 'SUCCESS' or 'FAILURE'.
      !>
      !> Legacy interface: calls byebye of module pepper_output.
      subroutine byebye ( status )
      use pepper_output, only: pepper_byebye => byebye
      implicit none
      character(*) :: status  !! 'SUCCESS' or 'FAILURE'
      call pepper_byebye ( status )
      end subroutine byebye

end module pepper_legacy_errsys
