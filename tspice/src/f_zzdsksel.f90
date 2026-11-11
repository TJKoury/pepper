!$Procedure      F_ZZDSKSEL ( Test the entry points of ZZDSKSEL )

      SUBROUTINE F_ZZDSKSEL ( OK )
      use pepper_inc_dla
      use pepper_inc_dskdsc

! $ Abstract
!
!     Perform a collection of rudimentary tests on the
!     ZZDSKSEL collection of entry points.
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
!
!     PEPPER: the size of this test's surface lists (SPICELIB's MAXSRF of
!     dsk.inc; the library's surface lists have no limit).
!
      INTEGER               MAXSRF
      PARAMETER           ( MAXSRF = 100 )



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
!     The umbrella routine ZZDSKSEL contains the following
!     entry points:
!
!
!     listed below.
!
!         Match body ID only
!         ------------------
!
!           Initialization:       ZZDSKSBD
!           Segment comparison:   ZZDSKBDC
!
!
!     The remaining entry points are included to support the SMAP
!     Alpha DSK Toolkit. They may be useful for DSK type 4 in a
!     future SPICELIB version.
!
!
!         Match body ID, surface list, and time
!         -------------------------------------
!
!           Initialization:       ZZDSKSIT
!           Segment comparison:   ZZDSKCIT
!
!
!         Match body ID, time, and coordinates
!         ------------------------------------
!
!           Initialization:       ZZDSKUSC
!           Segment comparison:   ZZDSKUMC
!
!
!         Match body ID, surface ID, frame ID, coordinate system,
!         coordinate parameters, time, and coordinates
!         ------------------------------------------------------
!
!           Initialization:       ZZDSKMSC
!           Segment comparison:   ZZDSKMMC
!
!
!         Match body ID, surface ID, frame ID, data class, time,
!         and whether coverage includes a specified position vector
!         ---------------------------------------------------------
!
!           Initialization:       ZZDSKSRC
!           Segment comparison:   ZZDSKMRC
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
!-    TSPICE Version 1.0.0 20-MAY-2016 (NJB)
!
!-&



!
!     SPICELIB functions
!
      DOUBLE PRECISION      JYEAR
      DOUBLE PRECISION      RPD

      LOGICAL               ZZDSKBDC
      LOGICAL               ZZDSKCIT
      LOGICAL               ZZDSKSBD
      LOGICAL               ZZDSKSEL
      LOGICAL               ZZDSKSIT
      LOGICAL               ZZDSKUMC
      LOGICAL               ZZDSKUSC

!
!     Local parameters
!
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

!
!     Local variables
!
      DOUBLE PRECISION      BTIME
      DOUBLE PRECISION      COR1
      DOUBLE PRECISION      COR2
      DOUBLE PRECISION      CORPAR ( NSYPAR )
      DOUBLE PRECISION      DSKDSC ( DSKDSZ )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ETIME
      DOUBLE PRECISION      POS    ( 3 )

      INTEGER               BODYID
      INTEGER               CORSYS
      INTEGER               DCLASS
      INTEGER               DLADSC ( DLADSZ )
      INTEGER               FRAMID
      INTEGER               HANDLE
      INTEGER               NSURF
      INTEGER               SURFID
      INTEGER               SRFLST ( MAXSRF )

      LOGICAL               ISMTCH
      LOGICAL               LGRETV

!
!     Saved variables
!
      SAVE




!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ZZDSKSEL' )


!***********************************************************************
!
!     ZZDSKSEL umbrella tests
!
!***********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Direct call to ZZDSKSEL' )

      LGRETV = ZZDSKSEL ( SURFID, NSURF,  SRFLST, &
                          BODYID, DCLASS, CORSYS, CORPAR, &
                          COR1,   COR2,   FRAMID, POS, &
                          ET,     HANDLE, DLADSC, DSKDSC )

      CALL CHCKXC ( .TRUE., 'SPICE(BOGUSENTRY)', OK )


!***********************************************************************
!
!     ZZDSKSBD/ZZDSKBDC tests
!
!***********************************************************************

!***********************************************************************
!
!     ZZDSKSBD/ZZDSKBDC normal cases
!
!***********************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDSKSBD/ZZDSKBDC: match case' )

!
!     Initialize handle and DLA descriptor. Neither are
!     used for this comparison.
!
      HANDLE = 0
      CALL CLEARI ( DLADSZ, DLADSC )


      BODYID = 599

      LGRETV = ZZDSKSBD( BODYID )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The DSK descriptor must have a matching body ID code.
!
      CALL CLEARD ( DSKDSZ, DSKDSC )

!
!     The body ID index in a DSK descriptor is called 'CTRIDX'.
!
      DSKDSC( CTRIDX ) = DBLE( BODYID )


      ISMTCH =  ZZDSKBDC ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISMTCH', ISMTCH, .TRUE., OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDSKSBD/ZZDSKBDC: non-match case' )


      DSKDSC( CTRIDX ) = DBLE( BODYID + 1 )

      ISMTCH =  ZZDSKBDC ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISMTCH', ISMTCH, .FALSE., OK )

!
!     No error cases exist for these routines.
!


!***********************************************************************
!
!     ZZDSKSIT/ZZDSKCIT tests
!
!***********************************************************************

!***********************************************************************
!
!     ZZDSKSIT/ZZDSKCIT normal cases
!
!***********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDSKSBD/ZZDSKBDC: match case, empty ' &
      //           'surface list.' )

!
!     Initialize handle and DLA descriptor. Neither are
!     used for this comparison.
!
      HANDLE = 0
      CALL CLEARI ( DLADSZ, DLADSC )


      BODYID = 599
      NSURF  = 0

      CALL CLEARI ( MAXSRF, SRFLST )

      ET = 10.D0

      LGRETV = ZZDSKSIT( BODYID, NSURF, SRFLST, ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The DSK descriptor must have a matching body ID code.
!
      CALL CLEARD ( DSKDSZ, DSKDSC )

      DSKDSC( CTRIDX ) = DBLE( BODYID )

!
!     Set time bounds. The bounds must bracket ET.
!
      BTIME            = -50*JYEAR()
      ETIME            =  50*JYEAR()

      DSKDSC( BTMIDX ) = BTIME
      DSKDSC( ETMIDX ) = ETIME

!
!     The body ID index in a DSK descriptor is called 'CTRIDX'.
!

      ISMTCH =  ZZDSKCIT ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISMTCH', ISMTCH, .TRUE., OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDSKSBD/ZZDSKBDC: match case, non-empty ' &
      //           'surface list.' )

!
!     Initialize handle and DLA descriptor. Neither are
!     used for this comparison.
!
      HANDLE = 0
      CALL CLEARI ( DLADSZ, DLADSC )


      BODYID    = 599

      CALL CLEARI ( MAXSRF, SRFLST )
      NSURF     = 2
      SRFLST(1) = -1
      SRFLST(2) = -2


      ET        = 10.D0

      LGRETV = ZZDSKSIT( BODYID, NSURF, SRFLST, ET )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     The DSK descriptor must have a matching body ID code.
!
      CALL CLEARD ( DSKDSZ, DSKDSC )

!
!     Set the surface ID code.
!
      DSKDSC( SRFIDX ) = SRFLST(1)

!
!     Set time bounds. The bounds must bracket ET.
!
      DSKDSC( CTRIDX ) = DBLE( BODYID )

      BTIME            = -50*JYEAR()
      ETIME            =  50*JYEAR()

      DSKDSC( BTMIDX ) = BTIME
      DSKDSC( ETMIDX ) = ETIME

!
!     Look for a match.
!
      ISMTCH =  ZZDSKCIT ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISMTCH (-1)', ISMTCH, .TRUE., OK )

!
!     Try again with the other surface.
!
      DSKDSC( SRFIDX ) = SRFLST(2)


      ISMTCH =  ZZDSKCIT ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISMTCH (-2)', ISMTCH, .TRUE., OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDSKSBD/ZZDSKBDC: non-match case, body ' &
      //           'mismatch.' )


!
!     Save body ID.
!
      BODYID = NINT( DSKDSC(CTRIDX) )

      DSKDSC( CTRIDX ) = 0


      ISMTCH =  ZZDSKCIT ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISMTCH', ISMTCH, .FALSE., OK )
!
!     Restore body ID.
!
      DSKDSC( CTRIDX ) = DBLE( BODYID )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDSKSBD/ZZDSKBDC: non-match case, time ' &
      //           'too early.' )


      LGRETV = ZZDSKSIT( BODYID, NSURF, SRFLST, BTIME-1.D0 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      ISMTCH =  ZZDSKCIT ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISMTCH', ISMTCH, .FALSE., OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDSKSBD/ZZDSKBDC: non-match case, time ' &
      //           'too late.' )


      LGRETV = ZZDSKSIT( BODYID, NSURF, SRFLST, ETIME+1.D0 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      ISMTCH =  ZZDSKCIT ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISMTCH', ISMTCH, .FALSE., OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDSKSBD/ZZDSKBDC: non-match case, surface ' &
      //           'not in list.' )



      LGRETV = ZZDSKSIT( BODYID, 1, -3, ET )
      CALL CHCKXC ( .FALSE., ' ', OK )


      ISMTCH =  ZZDSKCIT ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISMTCH', ISMTCH, .FALSE., OK )


!***********************************************************************
!
!     ZZDSKSIT/ZZDSKCIT error cases
!
!***********************************************************************

!
! --- Case: ------------------------------------------------------
!
!     PEPPER: no case for a surface list longer than MAXSRF: ZZDSKSIT
!     takes any number of surfaces (SPICELIB signaled
!     SPICE(VALUEOUTOFRANGE)).






!***********************************************************************
!
!     ZZDSKUSC/ZZDSKUMC tests
!
!***********************************************************************

!***********************************************************************
!
!     ZZDSKUSC/ZZDSKUMC normal cases
!
!***********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDSKUSC/ZZDSKUMC: match case.' )


!
!     Initialize handle and DLA descriptor. Neither are
!     used for this comparison.
!
      HANDLE = 0
      CALL CLEARI ( DLADSZ, DLADSC )


      BODYID = 599

      ET     = 10.D0

      COR1   = 30.D0 * RPD()
      COR2   = -5.D0 * RPD()

      LGRETV = ZZDSKUSC( BODYID, ET, COR1, COR2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The DSK descriptor must have a matching body ID code.
!
      CALL CLEARD ( DSKDSZ, DSKDSC )

      DSKDSC( CTRIDX ) = DBLE( BODYID )

!
!     Set time bounds. The bounds must bracket ET.
!
      BTIME            = -50*JYEAR()
      ETIME            =  50*JYEAR()

      DSKDSC( BTMIDX ) = BTIME
      DSKDSC( ETMIDX ) = ETIME

!
!     Set coordinate system and coordinate bounds.
!
      DSKDSC( SYSIDX ) = DBLE( LATSYS )

      DSKDSC( MN1IDX ) = -90.D0 * RPD()
      DSKDSC( MX1IDX ) =  90.D0 * RPD()

      DSKDSC( MN2IDX ) = -45.D0 * RPD()
      DSKDSC( MX2IDX ) =  45.D0 * RPD()

      ISMTCH =  ZZDSKUMC ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISMTCH', ISMTCH, .TRUE., OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDSKUSC/ZZDSKUMC: non-match case, body ' &
      //           'mismatch.' )


!
!     Save body ID.
!
      BODYID = NINT( DSKDSC(CTRIDX) )

      DSKDSC( CTRIDX ) = 0


      ISMTCH =  ZZDSKUMC ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISMTCH', ISMTCH, .FALSE., OK )
!
!     Restore body ID.
!
      DSKDSC( CTRIDX ) = DBLE( BODYID )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDSKUSC/ZZDSKUMC: non-match case, time ' &
      //           'too early.' )


      LGRETV = ZZDSKUSC( BODYID, BTIME-1, COR1, COR2 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      ISMTCH =  ZZDSKUMC ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISMTCH', ISMTCH, .FALSE., OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDSKUSC/ZZDSKUMC: non-match case, time ' &
      //           'too late.' )


      LGRETV = ZZDSKUSC( BODYID, ETIME+1, COR1, COR2 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      ISMTCH =  ZZDSKUMC ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISMTCH', ISMTCH, .FALSE., OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDSKUSC/ZZDSKUMC: non-match case, coordinate ' &
      //           '1 too small' )


      LGRETV = ZZDSKUSC( BODYID, ET, DSKDSC(MN1IDX)-0.1D0, COR2 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      ISMTCH =  ZZDSKUMC ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISMTCH', ISMTCH, .FALSE., OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDSKUSC/ZZDSKUMC: non-match case, coordinate ' &
      //           '1 too large' )


      LGRETV = ZZDSKUSC( BODYID, ET, DSKDSC(MX1IDX)+0.1D0, COR2 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      ISMTCH =  ZZDSKUMC ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISMTCH', ISMTCH, .FALSE., OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDSKUSC/ZZDSKUMC: non-match case, coordinate ' &
      //           '2 too small' )


      LGRETV = ZZDSKUSC( BODYID, ET, COR1, DSKDSC(MN2IDX)-0.1D0 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      ISMTCH =  ZZDSKUMC ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISMTCH', ISMTCH, .FALSE., OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDSKUSC/ZZDSKUMC: non-match case, coordinate ' &
      //           '2 too large' )


      LGRETV = ZZDSKUSC( BODYID, ET, COR1, DSKDSC(MX2IDX)+0.1D0 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      ISMTCH =  ZZDSKUMC ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISMTCH', ISMTCH, .FALSE., OK )

!
!     No error cases exist for these routines.
!


!***********************************************************************
!
!     ZZDSKMSC/ZZDSKMMC tests
!
!***********************************************************************

!***********************************************************************
!
!     ZZDSKMSC/ZZDSKMMC normal cases
!
!***********************************************************************

!
!     TBD
!




      CALL T_SUCCESS ( OK )
      RETURN
      END
