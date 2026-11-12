      SUBROUTINE FLGRPT ( NITEMS, NAMES, VALUES, MYIO )
      use pepper_legacy
!
!     This routine takes an array of names and an array of associated
!     value strings and produces a flagged set of outputs.  This
!     routine signals no errors.
!
      IMPLICIT NONE
      INTEGER               NITEMS
      CHARACTER*(*)         NAMES ( * )
      CHARACTER*(*)         VALUES( * )
!
!     The routine MYIO is a routine that is supplied by the user
!     that can handle io of text lines without any action by the
!     routine that calls it.
!
! $ Version
!
!     Inspekt Routine version 2.0.0, 7-APR-1995 (WLT)
!
!        Unused variables LEFT and RIGHT were removed.
!
      EXTERNAL MYIO

!
!     Spicelib functions
!


      INTEGER               STYLEN
      PARAMETER           ( STYLEN = 200 )

      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               L
      INTEGER               WIDTH

      LOGICAL               FREE ( 0 : 128 )

      CHARACTER*(1)         HARD
      CHARACTER*(1)         LETTER
      CHARACTER*(STYLEN)    STYLE




      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'FLGRPT' )
!
!     First find the widest of the names:
!
      WIDTH = 0

      DO I = 1, NITEMS

         IF ( RTRIM(NAMES(I)) .GT. WIDTH ) THEN
            WIDTH = RTRIM(NAMES(I))
         END IF

      END DO
!
!     Now for each of the NAME/VALUE pairs construct a style
!     string using NAMES and run the VALUES through NICEPR_1.
!
      DO I = 1, NITEMS
!
!        First we need to find a character that is not used
!        in the NAMES(I)/VALUES(I) pair.  We will use this as
!        a hardspace in our style string.
!
         DO J = 33, 127
            FREE(J) = .TRUE.
         END DO

         DO J = 1, WIDTH
            FREE( ICHAR(NAMES(I)(J:J))  ) = .FALSE.
         END DO

         DO J = 1, LEN(VALUES(1))
            FREE( ICHAR(VALUES(I)(J:J)) ) = .FALSE.
         END DO

         J = 33
         DO WHILE ( .NOT. FREE(J) .AND. J .LT. 127 )
            J = J + 1
         END DO

         HARD = CHAR(J)


!
!        Set up the style we are going to use for this
!        value
!
         CALL NSPMRG ( STYLE )

         CALL SUFFIX ( 'HARDSPACE', 1, STYLE )
         CALL SUFFIX (  HARD,       1, STYLE )
         CALL SUFFIX ( 'FLAG',      1, STYLE )

         L = RTRIM(STYLE) + 2

         DO K = 1, WIDTH

            LETTER = NAMES(I)(K:K)

            IF (LETTER .EQ. ' ' ) THEN
               STYLE(L:L) = HARD
            ELSE
               STYLE(L:L) = LETTER
            END IF

            L = L + 1

         END DO

         STYLE(L:L) = ':'
         L          =  L + 1
         STYLE(L:L) =  HARD

!
!        Ok.  Now just ship the stuff to the output routines.
!
         IF (       ( NAMES (I) .EQ. ' ' ) &
              .AND. ( VALUES(I) .EQ. ' ' ) ) THEN

             STYLE(L-1:L-1) = HARD
             CALL NICEPR_1  ( HARD,    STYLE(1:L), MYIO )

         ELSE IF ( VALUES(I) .EQ. ' ' ) THEN
            STYLE(L-1:L-1) = HARD
            CALL NICEPR_1  ( HARD,     STYLE(1:L), MYIO )
         ELSE
            CALL NICEPR_1 ( VALUES(I), STYLE(1:L), MYIO )
         END IF


      END DO

      CALL CHKOUT ( 'FLGRPT' )
      RETURN
      END
