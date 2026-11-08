!$Procedure STDIO ( Standard IO )

      subroutine stdio ( name, unit )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Return the logical unit associated with some standard input or
!     standard output.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     INPUT-OUTPUT
!
! $ Declarations

      implicit none

      character(*)          name
      integer(ip)           unit

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NAME       I   is the name of a logical unit to return.
!     UNIT       O   is the logical unit associated with NAME.
!
! $ Detailed_Input
!
!     NAME     is the "name" of a FORTRAN unit to return.
!              Recognized names are 'STDIN' and 'STDOUT'.
!              The routine is case insensitive to NAME.
!
!              If NAME is not recognized the error
!              SPICE(BADSTDIONAME) is signaled and UNIT is
!              set to -100.
!
! $ Detailed_Output
!
!     UNIT     is the logical unit associated with NAME. If
!              NAME is not recognized, UNIT is set to -100.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If NAME is not recognized, the error SPICE(BADSTDIONAME) is
!         signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This is a low level utility for retrieving the logical units
!     associated with standard input and output. It exists to
!     isolate SPICE based code from compiler writer choices in the
!     implementation of standard input and output.
!
! $ Examples
!
!     Suppose you would like to send a message to standard output
!     and that this message is contained in the array of N character
!     strings MESSGE. The code below would handle the task.
!
!        CALL STDIO ( 'STDOUT', STDOUT )
!
!        DO I = 1, N
!           CALL WRITLN ( MESSGE(I), STDOUT )
!        END DO
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 20-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Spelled out I/O
!        in $Keywords section.
!
!-    SPICELIB Version 1.0.0, 18-SEP-1996 (WLT)
!
!-&


! $ Index_Entries
!
!     logical units associated standard input and output
!
!-&


!
!     Spicelib Functions
!
      logical               return

!
!     Local Variables
!
      integer(ip), parameter :: wdsize = 8

      character(wdsize)     myname

      call ljust ( name,   myname )
      call ucase ( myname, myname )

      if ( myname == 'STDIN' ) then
         unit = 5
      else if ( myname == 'STDOUT' ) then
         unit = 6
      else if ( return() ) then
         return
      else

         call setmsg ( 'The only "names" recognized by STDIO are ' &
         //            '''STDIN'' and ''STDOUT'' you requested a ' &
         //            'unit for ''#''. '     )
         call errch  ( '#', name              )
         call sigerr ( 'SPICE(BADSTDIONAME)'  )
      end if

      return
      end
