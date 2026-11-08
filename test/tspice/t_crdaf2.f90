!$Procedure  T_CRDAF2 ( Create Supporting DAF Files )

      SUBROUTINE T_CRDAF2 ( TYPE, NAME, IFNAME, NSEG, IDS, TBEGS, &
                                                   TENDS, SEGIDS )

! $ Abstract
!
!     Create a new DAF with specifed segments and user-provided
!     internal file name.
!
!        07-NOV-2001 (NJB)
!        26-APR-2012 (BVS)
!
!     Support routine for TSPICE test families
!
!       f_pckbsr
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TEST FAMILY
!
! $ Declarations


      IMPLICIT NONE

      CHARACTER*(*)         TYPE
      CHARACTER*(*)         NAME
      CHARACTER*(*)         IFNAME
      INTEGER               NSEG
      INTEGER               IDS    ( * )
      DOUBLE PRECISION      TBEGS  ( * )
      DOUBLE PRECISION      TENDS  ( * )
      CHARACTER*(*)         SEGIDS ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     TYPE       I   'SPK', 'CK', or 'PCK'.
!     NAME       I   File name.
!     IFNAME     I   Internal file name.
!     NSEG       I   Number of segments.
!     IDS        I   Body IDs.
!     TBEGS      I   Start times.
!     TENDS      I   Stop times.
!     SEGIDS     I   Segment IDs.
!
! $ Detailed_Input
!
!     See Brief_I/O.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     Some routines in the call tree of this routine may signal errors.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine creates SPK, CK, or binary PCK file to support
!     testing.
!
! $ Examples
!
!     None.
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
!     N.J. Bachman     (JPL)
!     B.V. Semenov     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 26-APR-2012 (NJB) (BVS)
!
!        Based on T_CRDAF.
!
!-&

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


      CALL CHKIN ( 'T_CRDAF2' )

      CALL LJUST ( TYPE,   LOCTYP )
      CALL UCASE ( LOCTYP, LOCTYP )

      IF ( LOCTYP .EQ. 'SPK' )  THEN

         CALL SPKOPN ( NAME, IFNAME, 0, HANDLE )

      ELSE IF (  LOCTYP .EQ.  'CK' )  THEN

         CALL CKOPN  ( NAME, IFNAME, 0, HANDLE )

      ELSE

         CALL PCKOPN ( NAME, IFNAME, 0, HANDLE )

      END IF


      DO I = 1, NSEG

         CALL T_CRDESC ( LOCTYP, I, IDS(I), TBEGS(I), TENDS(I), DESCR )

         CALL DAFBNA ( HANDLE, DESCR, SEGIDS(I) )
         CALL DAFENA

      END DO

      CALL DAFCLS ( HANDLE )

      CALL CHKOUT ( 'T_CRDAF2' )
      END

