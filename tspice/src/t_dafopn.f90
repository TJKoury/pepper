!$Procedure T_DAFOPN ( Open a new DAF file for testing )

      SUBROUTINE T_DAFOPN ( FNAME, OUTBFF, UNIT )
      use pepper_inc_zzddhman

! $ Abstract
!
!     Open a new DAF file.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TEST ROUTINE
!
! $ Declarations

      IMPLICIT NONE


      CHARACTER*(*)         FNAME
      INTEGER               OUTBFF
      INTEGER               UNIT

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FNAME      I   Name of the DAF to open.
!     OUTBFF     I   Binary file format code used to create file.
!     UNIT       O   Logical unit used to open the file.
!
! $ Detailed_Input
!
!     FNAME      is the name of the DAF that is to be created.
!
!     OUTBFF     is an integer code that indicates the binary file
!                format targeted for OUTPUT.  Acceptable values
!                are the parameters:
!
!                   BIGI3E
!                   LTLI3E
!                   VAXGFL
!                   VAXDFL
!
!                as defined in the include file 'zzddhman.inc'.
!
! $ Detailed_Output
!
!     UNIT       is the logical unit used to open FNAME.
!
! $ Parameters
!
!     None.
!
! $ Files
!
!     This module opens FNAME with NEW status.
!
! $ Exceptions
!
!     1) SPICE(FILEOPENFAILED) is signaled if the OPEN statement fails.
!
! $ Particulars
!
!     This routine simply opens a new DAF and returns the logical
!     unit associated with the DAF.
!
!     Note: This code is not a model of perfection.  It is hacked
!     together from pieces of code written elsewhere to support
!     creation of non-native binaries for testing purposes only.
!
! $ Examples
!
!     See F_ZZDGDR for usage.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     F.S. Turner     (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 28-OCT-2001 (FST)
!
!
!-&

!
!     SPICELIB Functions
!
      INTEGER               RTRIM
      LOGICAL               RETURN

!
!     Local Variables
!
      INTEGER               IOSTAT

!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'T_DAFOPN' )
      END IF

!
!     First kill FNAME.
!
      CALL KILFIL ( FNAME )

!
!     Now fetch a logical unit.
!
      CALL GETLUN ( UNIT )

!
!     Open the file.
!
      OPEN ( UNIT   = UNIT, &
             FILE   = FNAME(1:RTRIM(FNAME)), &
             ACCESS = 'DIRECT', &
             RECL   = RECL, &
             STATUS = 'NEW', &
             IOSTAT = IOSTAT                 )

!
!     Check IOSTAT.
!
      IF ( IOSTAT .NE. 0 ) THEN
         CALL SETMSG ( 'Unable to open file, #.  IOSTAT was #.' )
         CALL ERRCH  ( '#', FNAME                               )
         CALL ERRINT ( '#', IOSTAT                              )
         CALL SIGERR ( 'SPICE(FILEOPENFAILED)'                  )
         CALL CHKOUT ( 'T_DAFOPN'                               )
         RETURN
      END IF

      CALL CHKOUT ( 'T_DAFOPN' )
      RETURN

      END
