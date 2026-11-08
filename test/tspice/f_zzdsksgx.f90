!$Procedure F_ZZDSKSGX ( ZZDSKSGX tests )

      SUBROUTINE F_ZZDSKSGX ( OK )

! $ Abstract
!
!     Exercise the routine ZZDSKSGX.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
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

      INCLUDE 'dla.inc'
      INCLUDE 'dskdsc.inc'
      INCLUDE 'dsktol.inc'

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
!     This routine tests the SPICELIB routine ZZDSKSGX.
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
!-    TSPICE Version 1.0.0, 17-JUN-2016 (NJB)
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               EXISTS

!
!     Local Parameters
!
      CHARACTER*(*)         DSK0
      PARAMETER           ( DSK0   = 'zzdsksgx_0.bds' )

      CHARACTER*(*)         PCK0
      PARAMETER           ( PCK0   = 'zzdsksgx.tpc' )


      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 32 )


!
!     Local Variables
!

      CHARACTER*(NAMLEN)    FRNAME

      DOUBLE PRECISION      D
      DOUBLE PRECISION      DC     ( 1 )
      DOUBLE PRECISION      DSKDSC ( DSKDSZ )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      RAYDIR ( 3 )
      DOUBLE PRECISION      VERTEX ( 3 )
      DOUBLE PRECISION      XPT    ( 3 )
      DOUBLE PRECISION      XXPT   ( 3 )

      INTEGER               BODYID
      INTEGER               DLADSC ( DLADSZ )
      INTEGER               DTYPE
      INTEGER               HANDLE
      INTEGER               IC     ( 1 )
      INTEGER               NLAT
      INTEGER               NLON
      INTEGER               PLID
      INTEGER               SURFID

      LOGICAL               FOUND


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
      CALL TOPEN ( 'F_ZZDSKSGX' )



!**********************************************************************
!
!     Normal cases
!
!**********************************************************************


!
!     The following simple cases are meant to assist debugging.
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Set-up: create tessellated ellipsoid DSK.'  )


!
!     Create, load, and discard generic test PCK.
!
      CALL TSTPCK ( PCK0, .TRUE., .FALSE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create DSK.
!
!     TARGET = 'MARS'
      FRNAME = 'IAU_MARS'
      BODYID = 499
      SURFID = 1

      NLON   = 40
      NLAT   = 20

      IF ( EXISTS(DSK0) ) THEN
         CALL DELFIL ( DSK0 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL T_ELDS2Z ( BODYID, SURFID, FRNAME, NLON, NLAT, DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Mars segment: check intercept.' )


      CALL DASOPR ( DSK0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DLABFS ( HANDLE, DLADSC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'Descr FOUND', FOUND, .TRUE., OK )

      CALL DSKGD ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DTYPE =   NINT( DSKDSC( TYPIDX ) )
      ET    = ( DSKDSC( BTMIDX ) + DSKDSC( ETMIDX ) ) / 2

      D     = 1.D6
      CALL VPACK  ( D, 2*D, 3*D, VERTEX )
      CALL VMINUS ( VERTEX,      RAYDIR )

      CALL ZZDSKSGX ( HANDLE, DLADSC, DTYPE, ET, VERTEX, &
                      RAYDIR, XPT,    DC,    IC, FOUND  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'xpt FOUND', FOUND, .TRUE., OK )

!
!     Compare results to those from DSKX02.
!
      CALL DSKX02 ( HANDLE, DLADSC, VERTEX, RAYDIR, PLID, XXPT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'xxpt FOUND', FOUND, .TRUE., OK )

!
!     We expect an exact match for the intercept.
!
      CALL CHCKAD ( 'XPT', XPT, '=', XXPT, 3, 0.D0, OK )

!
!     Check the integer info component. (The d.p. component
!     is undefined.)
!
      CALL CHCKAI ( 'IC', IC, '=', PLID, 1, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Mars segment: non-intercept case.' )

!
!     Just point the ray away from the target.
!
      CALL ZZDSKSGX ( HANDLE, DLADSC, DTYPE, ET, VERTEX, &
                      VERTEX, XPT,    DC,    IC, FOUND  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'xpt FOUND', FOUND, .FALSE., OK )


!**********************************************************************
!
!     Error cases
!
!**********************************************************************



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid file handle'  )

      CALL ZZDSKSGX ( HANDLE+1, DLADSC, DTYPE, ET, VERTEX, &
                      RAYDIR,   XPT,    DC,    IC, FOUND  )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDHANDLE)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad data type'  )

      CALL ZZDSKSGX ( HANDLE+1, DLADSC, -1,  ET, VERTEX, &
                      RAYDIR,   XPT,    DC,  IC, FOUND  )
      CALL CHCKXC ( .TRUE., 'SPICE(TYPENOTSUPPORTED)', OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Clean up.'  )

!
!     Close DSK file; open was performed using DASOPR.
!
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


