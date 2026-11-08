!$Procedure ERRFNM ( Insert filename into long error message text )

      subroutine errfnm ( marker, unit )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Substitute the first occurrence of a marker in the current long
!     error message with the name of the file attached to the logical
!     unit number.
!
! $ Required_Reading
!
!     ERROR
!
! $ Keywords
!
!     ERROR
!     STRING
!     UNITS
!
! $ Declarations

      implicit none

      character(*)          marker

      integer(ip)           unit

      integer(ip), parameter :: filen = 128

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     MARKER     I   A substring in the error message that is to be
!                    replaced.
!     UNIT       I   Logical unit number attached to a file.
!     FILEN      P   Maximum length of filename.
!
! $ Detailed_Input
!
!     MARKER   is a character string which marks a position in
!              the long error message where a character string
!              is to be substituted. Leading and trailing blanks
!              in MARKER are not significant.
!
!              Case IS significant;  'XX' is considered to be
!              a different marker from 'xx'.
!
!     UNIT     is the logical unit number attached to a file.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     FILEN    is the maximum file name length that can be
!              accommodated by this routine.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If the logical unit number is not attached to a file, the
!         string inserted into the long error message is:
!
!         '<unavailable from the system>'
!
!     2)  If the FORTRAN INQUIRE statement fails to execute properly,
!         the string inserted into the long error message is:
!
!         '<unavailable from the system>'
!
! $ Files
!
!     See "Detailed_Input" description of the variable UNIT.
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!     1. The following code fragment reads a record from a file
!        then checks to see if the read was successful. If the
!        read failed, an error message is constructed that
!        specifies the record number, the filename and the value
!        of IOSTAT.
!
!        ERRFNM is used to replace the marker in the long error
!        message with the name of the file.
!
!
!        READ ( UNIT, REC=RECNUM, IOSTAT=IOSTAT ) RECORD
!
!         IF ( IOSTAT .NE. 0 ) THEN
!
!            CALL SETMSG ( 'Error reading record number # from ' //
!      .                   'file FILENAME. The value of IOSTAT ' //
!      .                   'was #.'                              )
!
!            CALL ERRINT ( '#',         RECNUM   )
!            CALL ERRFNM ( 'FILENAME',  UNIT     )
!            CALL ERRINT ( '#',         IOSTAT   )
!            CALL SIGERR ( 'SPICE(READFAILURE)'  )
!            CALL CHKOUT ( 'SAMPLE'              )
!            RETURN
!
!         END IF
!
!
!         If the unit is attached to the file SAMPLE.DAT, RECNUM
!         is 15 and IOSTAT is 36, and the INQUIRE statement in
!         this routine executed successfully, the long error
!         message is:
!
!           'Error reading record number 15 from file SAMPLE.DAT.
!            The value of IOSTAT was 36.'
!
!
!         If the unit is not attached to a file or if the INQUIRE
!         statement in this routine failed to execute successfully,
!         the long error message is:
!
!           'Error reading record number 15 from file
!           <unavailable from the system>. The value of IOSTAT
!           was 36.'
!
!
!     2. Note that the case of the marker is significant. The following
!        code fragment contains a call to ERRFNM using a marker
!        that does not appear in the long error message.
!
!
!        READ ( UNIT, REC=RECNUM, IOSTAT=IOSTAT ) RECORD
!
!         IF ( IOSTAT .NE. 0 ) THEN
!
!            CALL SETMSG ( 'Error reading record number # from ' //
!      .                   'file FILENAME. The value of IOSTAT ' //
!      .                   'was #.'                              )
!
!            CALL ERRINT ( '#',         RECNUM   )
!            CALL ERRFNM ( 'filename',  UNIT     )
!            CALL ERRINT ( '#',         IOSTAT   )
!            CALL SIGERR ( 'SPICE(READFAILURE)'  )
!            CALL CHKOUT ( 'SAMPLE'              )
!            RETURN
!
!         END IF
!
!
!         If the marker is not found, ERRFNM does not substitute
!         the filename for the marker. The long error message in
!         this case is:
!
!           'Error reading record number 15 from file FILENAME.
!            The value of IOSTAT was 36.'
!
! $ Restrictions
!
!     1)  The filename length is restricted by the parameter FILEN.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 02-JUN-2021 (JDR)
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
!-    SPICELIB Version 1.0.0, 05-APR-1991 (HAN)
!
!-&


! $ Index_Entries
!
!     insert filename into long error message
!
!-&


!
!     Local variables
!
      character(filen)      name

      integer(ip)           iostat




!
!     Initialize the variables.
!
      name = ' '

!
!     Get the name of the file attached to the logical unit number.
!

      inquire ( unit   = unit, &
                name   = name, &
                iostat = iostat )


!
!     If the INQUIRE statement executed successfully and the unit
!     was attached to a file, we have a filename.
!
!     If the INQUIRE statement didn't execute successfully the value
!     of IOSTAT is not equal to zero. If the unit is not connected to
!     a file the filename is blank. If either of these two things
!     are true, we must construct a string indicating that the
!     filename was unavailable from the system.
!

      if ( ( iostat /= 0 ) .or. ( name == ' ' ) ) then

         name = '<unavailable from the system>'

      end if

!
!     Let the error handling routine take it from here.
!
      call errch ( marker, name )

      return
      end
