!$Procedure IOERR ( I/O error message writer )

      subroutine ioerr ( action, file, iostat )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Set the long error message equal to a standard I/O error message
!     composed from an action, the name of a file, and a value of
!     IOSTAT.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ERROR
!     FILES
!
! $ Declarations

      implicit none

      character*(*)    action
      character*(*)    file
      integer(ip)      iostat

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ACTION     I   Action which caused the error.
!     FILE       I   The name of the file involved.
!     IOSTAT     I   The value of IOSTAT returned by ACTION.
!
! $ Detailed_Input
!
!     ACTION   is the action which caused the error. This may
!              be the name of a basic operation, such as 'OPEN',
!              'READ', or 'WRITE', or may be more sophisticated,
!              for example, 'add an empty cluster header to'.
!
!     FILE     is the name of the file involved in the error.
!              This may be the system or logical name of a file
!              ('USER$DISK:[USER.SUB]TEMP.DAT', 'PLNEPH'), or one
!              of the standard files ('SYS$INPUT', 'SYS$OUTPUT').
!
!     IOSTAT   is the value of IOSTAT returned by ACTION. This
!              is appended to the end of the error message.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The input arguments are inserted into the standard form shown
!     below. Spaces are inserted where needed. Leading and trailing
!     spaces are removed.
!
!     The long error message is set equal to a standard I/O error
!     message, of the form:
!
!                 An error occurred while --------1----------
!                 -------2-------.  The value of IOSTAT returned
!                 was --3--.
!
!               where the values of ACTION, FILE, and IOSTAT are
!               assigned to positions 1, 2 and 3
!               respectively.
!
!     If the length of the entire composed message exceeds 320
!     characters, it is truncated.
!
!     SIGERR must be called following a call to this routine to
!     actually output the resulting long error message to the error
!     output device.
!
! $ Examples
!
!     The following example illustrates the use of IOERR.
!
!           CALL IOERR ( 'adding a new header to',
!                        EPHEM,
!                        24                      )
!
!     The resulting error message would be:
!
!           'An error occurred while adding a new header
!            to LIBDISK:[EPHEM.NESYS]VGR2_T860502.GEF.  The value
!            of IOSTAT returned was 24.'
!
!     Note that the user is not responsible for adding and eliminating
!     spaces to make the string readable. That is all done
!     automatically.
!
!     It is possible to omit the name of the file entirely, as in the
!     following (somewhat frivolous) example.
!
!           CALL IOERR ( 'cleaning a fish',
!                        ' ',
!                        -3                                )
!
!     The resulting error message would be:
!
!           'An error occurred while cleaning a fish.
!            The value of IOSTAT returned was -3.'
!
!     In fact, if the value of IOSTAT is zero, the last part of the
!     message is omitted entirely, as in the following example.
!
!           CALL IOERR ( 'writing the status line to',
!                        'SYS$OUTPUT',
!                        0                                 )
!
!     The resulting error message would be:
!
!           'An error occurred while writing the status
!            line to SYS$OUTPUT.'
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 03-JUN-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (NJB)
!
!-&


! $ Index_Entries
!
!     i/o error message writer
!
!-&


! $ Revisions
!
!-    Beta Version 2.0.0, 20-DEC-1988 (NJB)
!
!        IOERR now sets the long error message equal to the
!        constructed message, rather than returning the constructed
!        message to the caller.  IOERR's argument list has been
!        changed accordingly, and a call to SETMSG has been added.
!        Also, the name of the calling routine no longer appears
!        in the constructed message.
!
!-&


!
!     Local variables
!
      character*10          iochar

      integer(ip)           longln
      parameter           ( longln = 320 )
      character*(longln)    error

!
!     First comes some standard stuff.
!
      error =  'An error occurred while'

!
!     Next comes the action that caused the error, and the file name.
!     There should be at least one space between each of these pieces,
!     but not more than one.
!
      call suffix ( action, 1, error )
      call suffix ( file,   1, error )
      call suffix ( '.',    0, error )

!
!     More standard stuff. If IOSTAT is zero, there is no need for this
!     part of the message.
!
      if ( iostat .ne. 0 ) then

         call suffix ( 'The value of IOSTAT returned was', 2, error )

!
!        IOSTAT must be written to a character variable first.
!        Attempting to write it directly to ERROR could cause a
!        boo-boo if we have already overrun the length of ERROR.
!
         call intstr ( iostat, iochar )

         call suffix ( iochar, 1, error )
         call suffix ( '.',    0, error )

      end if

!
!     The message has been constructed.  Set the long error message
!     equal to the constructed message.
!

      call setmsg ( error )

      return
      end
