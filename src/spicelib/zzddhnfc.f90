!$Procedure ZZDDHNFC ( DDH, return native BFF format code )

      SUBROUTINE ZZDDHNFC ( NATBFF )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Return the integer code for the native binary file format
!     of the host system.
!
! $ Required_Reading
!
!     DAF
!     DAS
!
! $ Keywords
!
!     ASSIGNMENT
!     DAS
!     FILES
!
! $ Declarations

      IMPLICIT NONE
      INCLUDE 'zzddhman.inc'

      INTEGER               NATBFF

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     NATBFF     I   Native binary file format code.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     NATBFF         is the native binary file format code for the host
!                    system on which this routine is called.
!
! $ Parameters
!
!     See the INCLUDE file zzddhman.inc for a description of
!     binary file format codes.
!
! $ Exceptions
!
!     1)  If an error occurs while attempting to obtain the host
!         binary file format code, the error will be diagnosed by a
!         routine in the call tree of this routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Routines outside of SPICELIB will normally have no need to call
!     this routine.
!
!     This routine centralizes the logic needed to obtain the host
!     system's binary file format code.
!
! $ Examples
!
!     See usage in DASGRD.
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 05-FEB-2015 (NJB)
!
!-&

! $ Index_Entries
!
!     return native binary file format code
!
!-&


!
!     SPICELIB functions
!
      INTEGER               ISRCHC
      LOGICAL               RETURN

!
!     Local variables
!
      CHARACTER*(STRSIZ)    STRBFF ( NUMBFF )
      CHARACTER*(STRSIZ)    TMPSTR

      INTEGER               I
      INTEGER               SAVBFF

      LOGICAL               FIRST

!
!     Saved variables
!
      SAVE                  FIRST
      SAVE                  SAVBFF
      SAVE                  STRBFF

!
!     Initial values
!
      DATA                  FIRST / .TRUE. /


!
!     This routine checks in on the first pass only.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF


      IF ( FIRST ) THEN

         CALL CHKIN ( 'ZZDDHNFC' )

!
!        Populate STRBFF, the buffer that contains the labels
!        for each binary file format.
!
         DO I = 1, NUMBFF
            CALL ZZDDHGSD ( 'BFF', I, STRBFF(I) )
         END DO

!
!        Fetch the native binary file format and determine its
!        integer code.
!
         CALL ZZPLATFM ( 'FILE_FORMAT', TMPSTR )
         CALL UCASE    ( TMPSTR,        TMPSTR )

         SAVBFF = ISRCHC ( TMPSTR, NUMBFF, STRBFF )

         IF ( SAVBFF .EQ. 0 ) THEN

            CALL SETMSG ( 'The binary file format, ''#'', is not ' &
            //            'supported by this version of the toolkit. ' &
            //            'This is a serious problem, contact NAIF.'   )
            CALL ERRCH  ( '#', TMPSTR                                  )
            CALL SIGERR ( 'SPICE(BUG)'                                 )
            CALL CHKOUT ( 'ZZDDHNFC'                                   )
            RETURN

         END IF

!
!        Do not perform initialization tasks again.
!
         FIRST = .FALSE.

         CALL CHKOUT ( 'ZZDDHNFC' )

      END IF


      NATBFF = SAVBFF

      RETURN
      END

