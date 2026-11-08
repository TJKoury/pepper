!$Procedure ERRHAN ( Insert DAF/DAS file name into long error message )

      SUBROUTINE ERRHAN ( MARKER, HANDLE )

! $ Abstract
!
!     Substitute the first occurrence of a marker in the current long
!     error message with the file name associated with a given
!     DAF/DAS handle.
!
! $ Required_Reading
!
!     ERROR
!
! $ Keywords
!
!     DAF
!     DAS
!     ERROR
!     STRING
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'zzddhman.inc'

      CHARACTER*(*)         MARKER
      INTEGER               HANDLE

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     MARKER     I   A substring in the long error message to be
!                    replaced.
!     HANDLE     I   DAF/DAS handle associated with a file.
!     FILEN      P   Maximum length of filename.
!
! $ Detailed_Input
!
!     MARKER   is a character string that marks a position in
!              the long error message where a file name is to be
!              substituted. Leading and trailing blanks in MARKER
!              are not significant.
!
!              Case IS significant; 'XX' is considered to be
!              a different marker from 'xx'.
!
!     HANDLE   is the DAF/DAS handle associated with the file of
!              interest. HANDLE must be associated with a currently
!              loaded DAF or DAS file.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     FILEN    is the maximum file name length that can be
!              accommodated by this routine. Currently this
!              parameter is defined in the include file
!              zzddhman.inc.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If HANDLE refers to a scratch DAS file, the string inserted
!         into the long error message is
!
!            'DAS SCRATCH FILE'
!
!     2)  If HANDLE is not associated with a loaded DAF or DAS file,
!         the string inserted into the long error message is:
!
!            '<No name found for handle #>'
!
!         where the handle number is substituted for the marker '#'.
!
! $ Files
!
!     See "Detailed_Input" description of the variable HANDLE.
!
! $ Particulars
!
!     This routine provides a convenient and error-free mechanism
!     for inserting a DAF or DAS file name into an error message,
!     given the file handle associated with the file of interest.
!
! $ Examples
!
!     1) Create an error message pertaining to an SPK file
!        designated by HANDLE, then signal an error.
!
!           CALL SETMSG ( 'SPK file # contains a type 3 segment ' //
!          .              'with invalid polynomial degree #. '    //
!          .              'Segment index in file is #.'            )
!           CALL ERRHAN ( '#',  HANDLE                             )
!           CALL ERRINT ( '#',  DEGREE                             )
!           CALL ERRINT ( '#',  I                                  )
!           CALL SIGERR ( 'SPICE(INVALIDDEGREE)'                   )
!
! $ Restrictions
!
!     1)  The supported filename length is limited by the parameter
!         FILEN.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    SPICELIB Version 1.0.2, 02-JUN-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.1, 05-FEB-2015 (NJB)
!
!        Removed header comments about restrictions
!        on using this routine for DAS files.
!
!-    SPICELIB Version 1.0.0, 04-JAN-2002 (NJB)
!
!-&


! $ Index_Entries
!
!     insert DAF or DAS filename into long error message
!
!-&


!
!     Local parameters
!
      INTEGER               MAXNUM
      PARAMETER           ( MAXNUM = 32 )

!
!     Local variables
!
      CHARACTER*(FILEN)     FNAME
      CHARACTER*(MAXNUM)    NUMSTR

      INTEGER               INTARC
      INTEGER               INTBFF
      INTEGER               INTAMH

      LOGICAL               FOUND

!
!     Get the name of the file designated by the input handle.
!
      CALL ZZDDHNFO ( HANDLE, FNAME, INTARC, INTBFF, INTAMH, FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL INTSTR ( HANDLE, NUMSTR )

         FNAME = '<No name found for handle '
         CALL SUFFIX ( NUMSTR, 1, FNAME )
         CALL SUFFIX ( '>',    0, FNAME )

      END IF

!
!     Insert the file name string into the long error message.
!
      CALL ERRCH ( MARKER, FNAME )

      RETURN
      END
