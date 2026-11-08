
      SUBROUTINE T_CRCKDS ( SEGNO, INST, START, STOP, DESCR, GOTAV )
      IMPLICIT NONE

! $ Abstract
!
!     Create a new CK descriptor having specified ID code, angular
!     velocity flag, and time bounds.  The segment number is also
!     encoded in the integer components of the decriptor having
!     indices 2:NI.
!
!        18-NOV-2001 (NJB)
!
!     Support routine for TSPICE test family
!
!       f_ckbsr
!-&

      INTEGER               SEGNO
      INTEGER               INST
      DOUBLE PRECISION      START
      DOUBLE PRECISION      STOP
      DOUBLE PRECISION      DESCR ( * )
      LOGICAL               GOTAV

!
!     SPICELIB functions
!

!
!     Local parameters
!
      INTEGER               ND
      PARAMETER           ( ND     = 2 )

      INTEGER               NI
      PARAMETER           ( NI     = 6 )

      INTEGER               SCALE
      PARAMETER           ( SCALE  = 10000 )

!
!     Local variables
!
      DOUBLE PRECISION      DC ( ND )

      INTEGER               I
      INTEGER               IC ( NI )


      CALL CHKIN ( 'T_CRCKDS' )


      CALL CLEARI ( NI, IC )
      CALL CLEARD ( ND, DC )

      IC(1) = INST

      DO I = 2, NI
         IC(I) = SCALE * SEGNO + I
      END DO

      IF ( GOTAV ) THEN
         IC(4) = 1
      ELSE
         IC(4) = 0
      END IF

      DC(1) = START
      DC(2) = STOP

      CALL DAFPS ( ND, NI, DC, IC, DESCR )

      CALL CHKOUT ( 'T_CRCKDS' )
      END



