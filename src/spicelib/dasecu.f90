!$Procedure DASECU ( DAS extract comments to a logical unit )

      subroutine dasecu ( handle, comlun, comnts )

! $ Abstract
!
!     Extract comments from a previously opened binary DAS file to a
!     previously opened text file attached to a Fortran logical unit.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     None.
!
! $ Declarations

      implicit none

      integer               handle
      integer               comlun
      logical               comnts

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!      HANDLE    I   Handle of a DAS file opened with read access.
!      COMLUN    I   Logical unit of an opened text file.
!      COMNTS    O   Logical flag, indicating comments were found.
!
! $ Detailed_Input
!
!     HANDLE   is the file handle for a binary DAS file that has been
!              opened with read access.
!
!     COMLUN   is the Fortran logical unit of a previously opened text
!              file to which the comments from a binary DAS file are
!              to be written.
!
!              The comments will be placed into the text file beginning
!              at the current location in the file, and continuing
!              until all of the comments have been written.
!
! $ Detailed_Output
!
!     COMNTS   is a logical flag indicating whether or not any comments
!              were found in the comment area of a DAS file. COMNTS will
!              have the value .TRUE. if there were some comments, and
!              the value .FALSE. otherwise.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If an error occurs while reading from the binary DAS file
!         attached to HANDLE, the error is signaled by a routine in the
!         call tree of this routine.
!
!     2)  If an error occurs while writing to the text file attached to
!         COMLUN, the error is signaled by a routine in the call tree of
!         this routine.
!
! $ Files
!
!     See parameters COMLUN and HANDLE in the $Detailed_Inputs section.
!
! $ Particulars
!
!     This routine will extract all of the comments from the comment
!     area of a binary DAS file, placing them into a text file
!     attached to COMLUN, beginning at the current position in the
!     text file. If there are no comments in the DAS file, nothing is
!     written to the text file attached to COMLUN.
!
! $ Examples
!
!     Let
!
!        HANDLE   be the DAS file handle of a previously opened binary
!                 DAS file.
!
!        COMLUN   be the Fortran logical unit of a previously opened
!                 text file that is to accept the comments from the
!                 DAS comment area.
!
!     The subroutine call
!
!        CALL DASECU ( HANDLE, COMLUN, COMNTS )
!
!     will extract the comments from the comment area of the binary
!     DAS file attached to HANDLE, if there are any, and write them
!     to the logical unit COMLUN. Upon successful completion, the
!     value of COMNTS will be .TRUE. if there were some comments
!     in the comment area and .FALSE. otherwise.
!
! $ Restrictions
!
!     1)  The maximum length of a single line comment in the comment
!         area is specified by the parameter LINLEN defined below.
!         Currently this value is 255 characters.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 20-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 05-JAN-1993 (KRG)
!
!-&


! $ Index_Entries
!
!     extract comments from a DAS file to a logical unit
!
!-&


!
!     SPICELIB functions
!
      logical               failed
      logical               return

!
!     Local parameters
!
!     Set the value for the maximum length of a text line.
!
      integer               linlen
      parameter           ( linlen = 255 )
!
!     Set the size of the comment buffer.
!
      integer               bufsiz
      parameter           ( bufsiz = 22 )
!
!     Local variables
!
      character*(linlen)    combuf(bufsiz)

      integer               numcom

      logical               eoc
      logical               gotsom


!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'DASECU' )
      end if
!
!     Verify that the DAS file attached to HANDLE is opened for reading.
!
      call dassih ( handle, 'READ' )

      if ( failed() ) then

         call chkout ( 'DASECU' )
         return

      end if
!
!     Initialize some things before the loop.
!
      numcom = 0
      eoc    = .false.
      gotsom = .false.

      do while ( .not. eoc )
!
!        While we have not reached the end of the comments, get some
!        more.
!
         call dasec ( handle, bufsiz, numcom, combuf, eoc )

         if ( failed() ) then

            call chkout( 'DASECU' )
            return

         end if

         if ( numcom .gt. 0 ) then
!
!           If NUMCOM .GT. 0 then we did get some comments, and we need
!           to write them out, but first, set the flag indicating that
!           we got some comments.
!
            if ( .not. gotsom ) then

               gotsom = .true.

            end if

            call writla ( numcom, combuf, comlun )

            if ( failed() ) then

               call chkout( 'DASECU' )
               return

            end if

         end if

      end do
!
!     Set the output flag indicating whether or not we got any comments.
!
      comnts = gotsom

      call chkout ( 'DASECU' )
      return
      end
