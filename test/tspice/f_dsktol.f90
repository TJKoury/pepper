!$Procedure F_DSKTOL ( DSKTOL tests )

      SUBROUTINE F_DSKTOL ( OK )

! $ Abstract
!
!     Exercise the SPICELIB routine DSKTOL.
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
!     This routine tests the SPICELIB DSK support routine DSKTOL.
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
!-    TSPICE Version 1.0.0, 01-AUG-2016 (NJB)
!
!-&

!
!     SPICELIB functions
!


!
!     Local Parameters
!

!
!     Local Variables
!
      DOUBLE PRECISION      DPVAL
      DOUBLE PRECISION      NEWVAL

!
!     Saved values
!
!     Initial values
!


!
!     Open the test family.
!
      CALL TOPEN ( 'F_DSKTOL' )


!***********************************************************************
!
!     DSKGTL tests
!
!***********************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'DSKGTL test: get expansion fraction' )

      CALL DSKGTL ( KEYXFR, DPVAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect exact equality, since we're comparing against
!     a value declared in an include file common to this
!     test family and the DSKTOL package.
!
      CALL CHCKSD ( 'XFRACT', DPVAL, '=', XFRACT, 0.D0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'DSKGTL test: get greedy segment parameter' )

      CALL DSKGTL ( KEYSGR, DPVAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect exact equality, since we're comparing against
!     a value declared in an include file common to this
!     test family and the DSKTOL package.
!
      CALL CHCKSD ( 'SGREED', DPVAL, '=', SGREED, 0.D0, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'DSKGTL test: get segment pad margin' )

      CALL DSKGTL ( KEYSPM, DPVAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect exact equality, since we're comparing against
!     a value declared in an include file common to this
!     test family and the DSKTOL package.
!
      CALL CHCKSD ( 'SGPADM', DPVAL, '=', SGPADM, 0.D0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'DSKGTL test: get surface-point membership margin' )

      CALL DSKGTL ( KEYPTM, DPVAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect exact equality, since we're comparing against
!     a value declared in an include file common to this
!     test family and the DSKTOL package.
!
      CALL CHCKSD ( 'PTMEMM', DPVAL, '=', PTMEMM, 0.D0, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'DSKGTL test: get angular rounding margin' )

      CALL DSKGTL ( KEYAMG, DPVAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect exact equality, since we're comparing against
!     a value declared in an include file common to this
!     test family and the DSKTOL package.
!
      CALL CHCKSD ( 'ANGMRG', DPVAL, '=', ANGMRG, 0.D0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'DSKGTL test: get longitude alias margin' )

      CALL DSKGTL ( KEYLAL, DPVAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect exact equality, since we're comparing against
!     a value declared in an include file common to this
!     test family and the DSKTOL package.
!
      CALL CHCKSD ( 'LONALI', DPVAL, '=', LONALI, 0.D0, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'DSKGTL test: keyword out of range' )

      CALL DSKGTL ( 0, DPVAL )
      CALL CHCKXC ( .TRUE., 'SPICE(INDEXOUTOFRANGE)', OK )

      CALL DSKGTL ( 7, DPVAL )
      CALL CHCKXC ( .TRUE., 'SPICE(INDEXOUTOFRANGE)', OK )




!***********************************************************************
!
!     DSKSTL tests
!
!***********************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'DSKSTL test: set expansion fraction' )

      NEWVAL = 1.D-3

      CALL DSKSTL ( KEYXFR, NEWVAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DSKGTL ( KEYXFR, DPVAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'XFRACT', DPVAL, '=', NEWVAL, 0.D0, OK )

!
!     Restore default.
!
      CALL DSKSTL ( KEYXFR, XFRACT )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'DSKSTL test: set greedy parameter' )

      NEWVAL = 1.D-2

      CALL DSKSTL ( KEYSGR, NEWVAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DSKGTL ( KEYSGR, DPVAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'SGREED', DPVAL, '=', NEWVAL, 0.D0, OK )

!
!     Restore default.
!
      CALL DSKSTL ( KEYSGR, SGREED )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'DSKSTL test: set pad margin' )

      NEWVAL = 1.D-1

      CALL DSKSTL ( KEYSPM, NEWVAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DSKGTL ( KEYSPM, DPVAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'SGPADM', DPVAL, '=', NEWVAL, 0.D0, OK )

!
!     Restore default.
!
      CALL DSKSTL ( KEYSPM, SGPADM )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'DSKSTL test: set plate membership margin' )

      NEWVAL = 1.D-3

      CALL DSKSTL ( KEYPTM, NEWVAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DSKGTL ( KEYPTM, DPVAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'PTMEMM', DPVAL, '=', NEWVAL, 0.D0, OK )

!
!     Restore default.
!
      CALL DSKSTL ( KEYPTM, PTMEMM )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'DSKSTL test: try to set angular rounding margin' )

!
!     This parameter is not adjustable.
!
      NEWVAL = 1.D-3

      CALL DSKSTL ( KEYAMG, NEWVAL )
      CALL CHCKXC ( .TRUE., 'SPICE(IMMUTABLEVALUE)', OK )

!
!     Make sure no change occurred.
!
      CALL DSKGTL ( KEYAMG, DPVAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect exact equality, since we're comparing against
!     a value declared in an include file common to this
!     test family and the DSKTOL package.
!
      CALL CHCKSD ( 'ANGMRG', DPVAL, '=', ANGMRG, 0.D0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'DSKSTL test: try to set longitude alias margin' )

!
!     This parameter is not adjustable.
!
      NEWVAL = 1.D-3

      CALL DSKSTL ( KEYLAL, NEWVAL )
      CALL CHCKXC ( .TRUE., 'SPICE(IMMUTABLEVALUE)', OK )

!
!     Make sure no change occurred.
!
      CALL DSKGTL ( KEYLAL, DPVAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect exact equality, since we're comparing against
!     a value declared in an include file common to this
!     test family and the DSKTOL package.
!
      CALL CHCKSD ( 'LONALI', DPVAL, '=', LONALI, 0.D0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'DSKSTL test: keyword out of range' )

      CALL DSKSTL ( 0, DPVAL )
      CALL CHCKXC ( .TRUE., 'SPICE(INDEXOUTOFRANGE)', OK )

      CALL DSKSTL ( 7, DPVAL )
      CALL CHCKXC ( .TRUE., 'SPICE(INDEXOUTOFRANGE)', OK )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


