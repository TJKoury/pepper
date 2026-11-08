!$Procedure F_ZZBODVCD ( ZZBODVCD tests )

      SUBROUTINE F_ZZBODVCD ( OK )

! $ Abstract
!
!     Exercise the routine ZZBODVCD.
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

      INCLUDE 'zzctr.inc'

      LOGICAL               OK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   logical indicating test status.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OK         is a logical that indicates the test status to the
!                caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     This routine does not generate any errors. Routines in its
!     call tree may generate errors that are either intentional and
!     trapped or unintentional and need reporting.  The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the SPICELIB routine ZZBODVCD.
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
!
! $ Version
!
!-    TSPICE Version 1.0.0, 09-MAY-2016 (NJB)
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               EXISTS

!
!     Local Parameters
!
      CHARACTER*(*)         PCK0
      PARAMETER           ( PCK0   = 'zzbodvcd.tpc' )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 255 )

      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = 10000 )

      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 32 )

!
!     Local Variables
!
      CHARACTER*(NAMLEN)    ITEM
      CHARACTER*(LNSIZE)    TITLE

      DOUBLE PRECISION      DPBUFF ( BUFSIZ )
      DOUBLE PRECISION      XBUFF  ( BUFSIZ )

      INTEGER               BODYID
      INTEGER               N
      INTEGER               XN

      INTEGER               VARCTR ( CTRSIZ )
      INTEGER               XCTR   ( CTRSIZ )

      LOGICAL               UPDATE

!
!     Saved values
!
!     Save variables in order to avoid stack room problems.
!
      SAVE

!
!     Initial values
!

!
!     Open the test family.
!
      CALL TOPEN ( 'F_ZZBODVCD' )



!**********************************************************************
!
!     Normal cases
!
!**********************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Set-up: create PCK.' )

      IF ( EXISTS(PCK0) ) THEN

         CALL DELFIL ( PCK0 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF


      CALL T_PCK08 ( PCK0, .TRUE., .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Initialize the user counters.
!
      CALL ZZCTRUIN ( VARCTR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZCTRUIN ( XCTR )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Fetch moon radii.'

      CALL TCASE ( TITLE )


      ITEM   = 'RADII'
      BODYID = 399

!
!     Get expected values.
!
      CALL BODVCD ( BODYID, ITEM, 3, XN, XBUFF )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ZZBODVCD ( BODYID, ITEM, 3, VARCTR, N, DPBUFF )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check counter.
!
      CALL ZZPCTRCK ( XCTR, UPDATE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'UPDATE', UPDATE, .TRUE., OK )

      CALL CHCKAI ( 'VARCTR', VARCTR, '=', XCTR, CTRSIZ, OK )

!
!     Check data size and values.
!
      CALL CHCKSI ( 'N', N, '=', XN, 0, OK )

      CALL CHCKAD ( ITEM, DPBUFF, '=', XBUFF, XN, 0.D0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Fetch moon radii a second time. Counter should ' &
      //       'remain unchanged.'

      CALL TCASE ( TITLE )


      CALL ZZBODVCD ( BODYID, ITEM, 3, VARCTR, N, DPBUFF )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check counter.
!
      CALL CHCKSL ( 'UPDATE', UPDATE, .TRUE., OK )

      CALL CHCKAI ( 'VARCTR', VARCTR, '=', XCTR, CTRSIZ, OK )

!
!     Check data size and values.
!
      CALL CHCKSI ( 'N', N, '=', XN, 0, OK )

      CALL CHCKAD ( ITEM, DPBUFF, '=', XBUFF, XN, 0.D0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Touch the pool; fetch moon radii a third time. ' &
      //       'Counter should be updated.'

      CALL DVPOOL ( 'BODY599_PM' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZBODVCD ( BODYID, ITEM, 3, VARCTR, N, DPBUFF )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check counter.
!
      CALL ZZPCTRCK ( XCTR, UPDATE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'UPDATE', UPDATE, .TRUE., OK )

!
!     Check counter.
!
      CALL CHCKSL ( 'UPDATE', UPDATE, .TRUE., OK )

      CALL CHCKAI ( 'VARCTR', VARCTR, '=', XCTR, CTRSIZ, OK )

!
!     Check data size and values.
!
      CALL CHCKSI ( 'N', N, '=', XN, 0, OK )

      CALL CHCKAD ( ITEM, DPBUFF, '=', XBUFF, XN, 0.D0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Verify that input values are returned if the ' &
      //       'pool wasn''t touched.'

      CALL TCASE ( TITLE )


      CALL CLEARD ( BUFSIZ, DPBUFF )
      CALL CLEARD ( BUFSIZ, XBUFF  )
      N = 0

      CALL ZZBODVCD ( BODYID, ITEM, 3, VARCTR, N, DPBUFF )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check counter.
!
      CALL CHCKAI ( 'VARCTR', VARCTR, '=', XCTR, CTRSIZ, OK )

!
!     Check data size and values.
!
      CALL CHCKSI ( 'N', N, '=', 0, 0, OK )

      CALL CHCKAD ( ITEM, DPBUFF, '=', XBUFF, BUFSIZ, 0.D0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Clean up.' )

      CALL DELFIL ( PCK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


