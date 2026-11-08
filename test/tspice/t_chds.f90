
      SUBROUTINE T_CHDS ( NAME, ARRAY, COMP, EXP, SIZE, TOL, OK )
!
!     Check segment descriptors for BSR test families.
!
!        10-NOV-2001 (NJB)
!

      IMPLICIT NONE

      CHARACTER*(*)         NAME
      DOUBLE PRECISION      ARRAY ( * )
      CHARACTER*(*)         COMP
      DOUBLE PRECISION      EXP   ( * )
      INTEGER               SIZE
      DOUBLE PRECISION      TOL
      LOGICAL               OK

!
!     Local parameters
!
      INTEGER               NDC
      PARAMETER           ( NDC    = 2 )

      INTEGER               MAXIC
      PARAMETER           ( MAXIC  = 6 )
!
!     Local variables
!
      DOUBLE PRECISION      DC  ( NDC )
      DOUBLE PRECISION      XDC ( NDC )

      INTEGER               NIC
      INTEGER               IC  ( MAXIC )
      INTEGER               XIC ( MAXIC )



      NIC = ( SIZE - 2 ) * 2

      CALL DAFUS ( ARRAY, NDC, NIC, DC,  IC  )
      CALL DAFUS ( EXP,   NDC, NIC, XDC, XIC )
!
!     Check the d.p. components.
!
      CALL CHCKAD ( NAME, DC, COMP, XDC, NDC, TOL, OK )

      IF ( .NOT. OK ) THEN
         RETURN
      END IF

!
!     Check the integer components.
!
      CALL CHCKAI ( NAME, IC, COMP, XIC, NIC, OK )

      RETURN
      END

