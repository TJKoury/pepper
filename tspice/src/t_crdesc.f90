      SUBROUTINE T_CRDESC ( TYPE, SEGNO, BODY, START, STOP, DESCR )
      use pepper_legacy

! $ Abstract
!
!     Create a new DAF descriptor of a specified kernel type
!     having specified ID code and time bounds.  The segment
!     number is also encoded in the integer components of
!     the decriptor having indices 2:NI.
!
!        30-OCT-2001 (NJB)
!
!     Support routine for TSPICE test families
!
!       f_pckbsr
!       f_spkbsr
!
!-&

      IMPLICIT NONE

      CHARACTER*(*)         TYPE
      INTEGER               SEGNO
      INTEGER               BODY
      DOUBLE PRECISION      START
      DOUBLE PRECISION      STOP
      DOUBLE PRECISION      DESCR ( * )

!
!     SPICELIB functions
!


!
!     Local parameters
!
      INTEGER               ND
      PARAMETER           ( ND     = 2 )

      INTEGER               MAXNI
      PARAMETER           ( MAXNI  = 6 )

      INTEGER               SCALE
      PARAMETER           ( SCALE  = 10000 )

!
!     Local variables
!
      DOUBLE PRECISION      DC     ( ND )

      INTEGER               I
      INTEGER               IC ( MAXNI )
      INTEGER               NI



      CALL CHKIN ( 'T_CRDESC' )


      IF (  EQSTR( TYPE, 'SPK' )  )  THEN

         NI = 6

      ELSE IF (  EQSTR( TYPE, 'CK' )  )  THEN

         NI = 6

      ELSE

         NI = 5

      END IF

      CALL CLEARI ( NI, IC )
      CALL CLEARD ( ND, DC )

      IC(1) = BODY

      DO I = 2, NI
         IC(I) = SCALE * SEGNO + I
      END DO

      IF (  EQSTR( TYPE, 'CK' )  )  THEN
         IC(4) = 1
      END IF

      DC(1) = START
      DC(2) = STOP

      CALL DAFPS ( ND, NI, DC, IC, DESCR )

      CALL CHKOUT ( 'T_CRDESC' )
      END



