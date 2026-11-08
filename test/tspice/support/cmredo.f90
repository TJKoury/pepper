!$Procedure      CMREDO ( COMMND loop trap )

      SUBROUTINE CMREDO ( COMMND, FROM, TRAP )

! $ Abstract
!
!    This routine examines COMMND and checks to see if it
!    should be sent to the COMMND loop stuff so that it
!    can be re-evaluated.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     COMMAND LOOP
!
! $ Declarations

      IMPLICIT NONE
      CHARACTER*(*)         COMMND
      INTEGER               FROM
      LOGICAL               TRAP

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     COMMND    I   A COMMND string to be checked for special syntax
!     TRAP       O   Indicates whether the string has special form
!
! $ Detailed_Input
!
!     COMMND    is a string that represents some COMMND.
!
!
! $ Detailed_Output
!
!     TRAP       is a logical indicating whether the string was special
!                and was put on the COMMND buffer.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine examines the input COMMND to see if it is one
!     of the following.
!
!     EDIT number
!     RECALL ALL
!     RECALL number
!     START  word
!     STOP
!     EXIT
!
!
! $ Examples
!
!     Later,
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!       None.
!
! $ Author_and_Institution
!
!       W.L. Taber      (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.1, 06-DEC-2021 (BVS)
!
!        Filled in $Index_Entries, cleaning up non-printing characters.
!
!        Re-ordered header sections.
!
!-    SPICELIB Version 2.0.0, 23-AUG-1995 (WLT)
!
!        Updated the routine so that EDIT *, DO * and RECALL *
!        are trapped.
!
!
!-&

! $ Index_Entries
!
!     command loop trap
!
!-&
      INTEGER               RTRIM
      LOGICAL               M2WMCH


      INTEGER               NONE
      PARAMETER           ( NONE   = 0 )

      INTEGER               COMBUF
      PARAMETER           ( COMBUF = NONE   + 1 )

      INTEGER               KEYBRD
      PARAMETER           ( KEYBRD = COMBUF + 1 )

      INTEGER               INPFIL
      PARAMETER           ( INPFIL = KEYBRD + 1 )

      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

      CHARACTER*(WDSIZE)    START
      CHARACTER*(WDSIZE)    EXIT
      CHARACTER*(WDSIZE)    STOP
      CHARACTER*(WDSIZE)    FRSTWD
      CHARACTER*(WDSIZE)    SCNDWD

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 300 )

      CHARACTER*(LNSIZE)    REST

      INTEGER               B1
      INTEGER               B2
      INTEGER               E1
      INTEGER               E2

      LOGICAL               FIRST
      SAVE

      DATA                  FIRST /.TRUE./
!

      IF ( FIRST ) THEN
         CALL TRNLAT ( 'STOP',      STOP   )
         CALL TRNLAT ( 'EXIT',      EXIT   )
         CALL TRNLAT ( 'START',     START  )

         FIRST = .FALSE.
      END IF



      CALL NEXTWD ( COMMND, FRSTWD, REST )
      CALL NEXTWD ( REST,   SCNDWD, REST )

      CALL UCASE  ( FRSTWD, FRSTWD )
      CALL UCASE  ( SCNDWD, SCNDWD )

      B1 = 1
      B2 = 1
      E1 = RTRIM  ( FRSTWD )
      E2 = RTRIM  ( SCNDWD )


      IF ( REST .NE. ' ' ) THEN
         TRAP = .FALSE.
         RETURN
      END IF

      IF ( FRSTWD .EQ. ' ' ) THEN
         TRAP = .FALSE.
         RETURN
      END IF


      IF ( FRSTWD .EQ. START ) THEN

         TRAP = .TRUE.
         CALL PUTCOM ( COMMND, FROM )
         RETURN

      ELSE IF (       FRSTWD .EQ. EXIT &
                .AND. SCNDWD .EQ. ' ' &
                .AND. FROM   .NE. KEYBRD ) THEN

         TRAP = .TRUE.
         CALL PUTCOM ( COMMND, FROM )
         RETURN

      ELSE IF (       FRSTWD .EQ. STOP &
                .AND. SCNDWD .EQ. ' ' &
                .AND. FROM   .NE. KEYBRD ) THEN

         TRAP = .TRUE.
         CALL PUTCOM ( COMMND, FROM )
         RETURN


      ELSE IF ( FROM .NE. KEYBRD ) THEN

         TRAP = .FALSE.
         RETURN

      ELSE IF (       SCNDWD .EQ. ' ' &
                .AND. .NOT. M2WMCH ( FRSTWD, B1, E1, 'RECALL' ) ) THEN

         TRAP = .FALSE.
         RETURN

      ELSE IF (      M2WMCH ( FRSTWD, B1, E1, 'RECALL'     ) &
               .AND. M2WMCH ( SCNDWD, B2, E2, '@int(1:20)' ) ) THEN

         TRAP = .TRUE.
         CALL PUTCOM ( COMMND, FROM )

      ELSE IF (      M2WMCH ( FRSTWD, B1, E1, 'RECALL' ) &
               .AND. M2WMCH ( SCNDWD, B2, E2, 'ALL'    ) ) THEN

         TRAP = .TRUE.
         CALL PUTCOM ( COMMND, FROM )

      ELSE IF (      M2WMCH ( FRSTWD, B1, E1, 'EDIT'     ) &
               .AND. M2WMCH ( SCNDWD, B2, E2, '@int(1:20)' ) ) THEN

         TRAP = .TRUE.
         CALL PUTCOM ( COMMND, FROM )

      ELSE IF (      M2WMCH ( FRSTWD, B1, E1, 'DO'     ) &
               .AND. M2WMCH ( SCNDWD, B2, E2, '@int(1:20)' ) ) THEN

         TRAP = .TRUE.
         CALL PUTCOM ( COMMND, FROM )

      ELSE IF (      M2WMCH ( FRSTWD, B1, E1, 'RECALL' ) &
               .AND. SCNDWD .NE. ' ' &
               .AND. REST   .EQ. ' ' ) THEN

         TRAP = .TRUE.
         CALL PUTCOM ( COMMND, FROM )

      ELSE IF (      M2WMCH ( FRSTWD, B1, E1, 'EDIT'     ) &
               .AND. SCNDWD .NE. ' ' &
               .AND. REST   .EQ. ' ' ) THEN

         TRAP = .TRUE.
         CALL PUTCOM ( COMMND, FROM )

      ELSE IF (      M2WMCH ( FRSTWD, B1, E1, 'DO'     ) &
               .AND. SCNDWD .NE. ' ' &
               .AND. REST   .EQ. ' ' ) THEN

         TRAP = .TRUE.
         CALL PUTCOM ( COMMND, FROM )

      ELSE

         TRAP = .FALSE.

      END IF


      RETURN
      END
