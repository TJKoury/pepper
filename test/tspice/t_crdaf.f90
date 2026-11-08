      SUBROUTINE T_CRDAF ( TYPE, NAME, NSEG, IDS, TBEGS, TENDS, SEGIDS )

! $ Abstract
!
!     Create a new DAF with specifed segments.
!
!        07-NOV-2001 (NJB)
!
!     Support routine for TSPICE test families
!
!       f_ckbsr
!       f_pckbsr
!       f_spkbsr
!
!-&

      IMPLICIT NONE

      CHARACTER*(*)         TYPE
      CHARACTER*(*)         NAME
      INTEGER               NSEG
      INTEGER               IDS    ( * )
      DOUBLE PRECISION      TBEGS  ( * )
      DOUBLE PRECISION      TENDS  ( * )
      CHARACTER*(*)         SEGIDS ( * )


!
!     SPICELIB functions
!

!
!     Local parameters
!
      INTEGER               DSCSIZ
      PARAMETER           ( DSCSIZ = 5 )

      INTEGER               TYPLEN
      PARAMETER           ( TYPLEN = 3 )

!
!     Local variables
!
      CHARACTER*(TYPLEN)    LOCTYP

      DOUBLE PRECISION      DESCR  ( DSCSIZ )

      INTEGER               HANDLE
      INTEGER               I


      CALL CHKIN ( 'T_CRDAF' )

      CALL LJUST ( TYPE,   LOCTYP )
      CALL UCASE ( LOCTYP, LOCTYP )

      IF ( LOCTYP .EQ. 'SPK' )  THEN

         CALL SPKOPN ( NAME, ' ', 0, HANDLE )

      ELSE IF (  LOCTYP .EQ.  'CK' )  THEN

         CALL CKOPN ( NAME, ' ', 0, HANDLE )

      ELSE

         CALL PCKOPN ( NAME, ' ', 0, HANDLE )

      END IF


      DO I = 1, NSEG

         CALL T_CRDESC ( LOCTYP, I, IDS(I), TBEGS(I), TENDS(I), DESCR )

         CALL DAFBNA ( HANDLE, DESCR, SEGIDS(I) )
         CALL DAFENA

      END DO

      CALL DAFCLS ( HANDLE )

      CALL CHKOUT ( 'T_CRDAF' )
      END

