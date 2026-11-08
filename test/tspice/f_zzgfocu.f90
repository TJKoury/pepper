!$Procedure      F_ZZGFOCU ( ZZGFOCU family tests )

      SUBROUTINE F_ZZGFOCU ( OK )

! $ Abstract
!
!     This routine tests *some functionality* of the SPICELIB routine
!
!        ZZGFOCU
!
!     To fully exercise ZZGFOCU, the test family
!
!        F_GFOCLT
!
!     must be run in addition to this family.
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
!     This routine tests the low-level SPICELIB
!     geometry utility package ZZGFOCU.
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
!-    TSPICE Version 4.1.0, 25-AUG-2021 (EDW)
!
!        Edited all error strings on radii values, and number of values
!        returned from kernel pool, to correspond to us-of/changes-in
!        ZZGFTREB.
!
!-    TSPICE Version 4.0.0, 22-FEB-2016 (NJB)
!
!        Updated expected keywords for invalid shape cases.
!
!-    TSPICE Version 3.1.0, 14-APR-2008 (NJB)
!
!        Kernel clean-up now consists only of unloading
!        the SPK file via SPKUEF and then deleting the
!        file.
!
!-    TSPICE Version 3.0.0, 12-APR-2008 (NJB)
!
!        Updated to use new names ZZGFOCU and F_ZZGFOCU.
!        "Header" such as it is now passes checkit tests.
!
!-    TSPICE Version 2.0.0, 29-MAR-2008 (NJB)
!
!        Updated to exercise new ZZGFOCIN error checks.
!
!-    TSPICE Version 1.1.0, 13-MAR-2008 (NJB)
!
!        Removed unnecessary variable STEP.
!
!-    TSPICE Version 1.0.0, 16-NOV-2007 (NJB)
!
!-&

!
!     SPICELIB functions
!

!
!     Local parameters
!
      CHARACTER*(*)         PCK
      PARAMETER           ( PCK = 'nat.tpc' )

      CHARACTER*(*)         SPK
      PARAMETER           ( SPK = 'nat.bsp' )


!
!     Local variables
!
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      NEWRAD ( 3 )
      DOUBLE PRECISION      RADII  ( 3 )

      INTEGER               HANDLE
      INTEGER               N

      LOGICAL               OCSTAT

!
!     Save everything.
!
      SAVE

!
!     Initial values
!

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ZZGFOCU' )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Setup: create and load SPK, PCK, LSK files.' )

      CALL NATSPK ( SPK, .TRUE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL NATPCK ( PCK, .TRUE., .FALSE. )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ',   OK )


!*********************************************************************
!*
!*    Error cases
!*
!*********************************************************************


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'ZZGFOCU: bogus entry' )

      ET = 0.D0

      CALL ZZGFOCU ( 'Any',  'ALPHAx',     'point',      'ALPHAFIXED', &
                     'BETA', 'ellipsoid',  'BETAFIXED',  'SUN', &
                     'LT', ET, OCSTAT     )
      CALL CHCKXC ( .TRUE., 'SPICE(BOGUSENTRY)',   OK )




!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'ZZGFOCIN: Unrecognized targets or observer' )

      CALL ZZGFOCIN ( 'Any',  'ALPHAx',     'point',      'ALPHAFIXED', &
                      'BETA', 'ellipsoid',  'BETAFIXED',  'SUN', &
                      'LT'     )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )


      CALL ZZGFOCIN ( 'Any',   'ALPHA',      'point',      'ALPHAFIXED', &
                      'BETAx', 'ellipsoid',  'BETAFIXED',  'SUN', &
                      'LT'     )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )


      CALL ZZGFOCIN ( 'Any',   'ALPHA',      'point',      'ALPHAFIXED', &
                      'BETA',  'ellipsoid',  'BETAFIXED',  'SUNx', &
                      'LT'     )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )




!
!---- Case -------------------------------------------------------------
!

      CALL TCASE ( 'ZZGFOCIN: non-positive radii for back target' )


      CALL BODVRD ( 'ALPHA', 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL MOVED ( RADII, 3, NEWRAD )
      NEWRAD(1) = 0.D0

      CALL PDPOOL ( 'BODY1000_RADII', 3, NEWRAD )

      CALL ZZGFOCIN ( 'Any',   'BETA',       'point',       'BETAFIXED', &
                      'ALPHA', 'ellipsoid',  'ALPHAFIXED',  'SUN', &
                      'LT'                                            )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)',   OK )



      CALL MOVED ( RADII, 3, NEWRAD )
      NEWRAD(2) = -1.D0

      CALL PDPOOL ( 'BODY1000_RADII', 3, NEWRAD )

      CALL ZZGFOCIN ( 'Any',   'BETA',       'point',       'BETAFIXED', &
                      'ALPHA', 'ellipsoid',  'ALPHAFIXED',  'SUN', &
                      'LT'                                             )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)',   OK )



      CALL MOVED ( RADII, 3, NEWRAD )
      NEWRAD(3) = -1.D0

      CALL PDPOOL ( 'BODY1000_RADII', 3, NEWRAD )

      CALL ZZGFOCIN ( 'Any',   'BETA',       'point',       'BETAFIXED', &
                      'ALPHA', 'ellipsoid',  'ALPHAFIXED',  'SUN', &
                      'LT'                                            )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)',   OK )


!
!     Restore original radii.
!
      CALL PDPOOL ( 'BODY1000_RADII', 3, RADII )
      CALL CHCKXC ( .FALSE., ' ',   OK )


!
!---- Case -------------------------------------------------------------
!

      CALL TCASE ( 'ZZGFOCIN: non-positive radii for front target' )


      CALL BODVRD ( 'BETA', 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL MOVED ( RADII, 3, NEWRAD )
      NEWRAD(1) = 0.D0

      CALL PDPOOL ( 'BODY2000_RADII', 3, NEWRAD )

      CALL ZZGFOCIN ( 'Any',   'BETA',       'ellipsoid',   'BETAFIXED', &
                      'ALPHA', 'ellipsoid',  'ALPHAFIXED',  'SUN', &
                      'LT'                                             )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)',   OK )



      CALL MOVED ( RADII, 3, NEWRAD )
      NEWRAD(2) = -1.D0

      CALL PDPOOL ( 'BODY2000_RADII', 3, NEWRAD )

      CALL ZZGFOCIN ( 'Any',   'BETA',       'ellipsoid',   'BETAFIXED', &
                      'ALPHA', 'ellipsoid',  'ALPHAFIXED',  'SUN', &
                      'LT'                                             )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)',   OK )



      CALL MOVED ( RADII, 3, NEWRAD )
      NEWRAD(3) = -1.D0

      CALL PDPOOL ( 'BODY2000_RADII', 3, NEWRAD )

      CALL ZZGFOCIN ( 'Any',   'BETA',       'ellipsoid',   'BETAFIXED', &
                      'ALPHA', 'ellipsoid',  'ALPHAFIXED',  'SUN', &
                      'LT'                                             )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)',   OK )


!
!     Restore original radii.
!
      CALL PDPOOL ( 'BODY2000_RADII', 3, RADII )
      CALL CHCKXC ( .FALSE., ' ',   OK )


!
!---- Case -------------------------------------------------------------
!

      CALL TCASE ( 'ZZGFOCIN: bad radius count for back target' )


      CALL BODVRD ( 'ALPHA', 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL MOVED ( RADII, 3, NEWRAD )
      NEWRAD(1) = 0.D0

      CALL PDPOOL ( 'BODY1000_RADII', 2, NEWRAD )

      CALL ZZGFOCIN ( 'Any',   'BETA',       'point',       'BETAFIXED', &
                      'ALPHA', 'ellipsoid',  'ALPHAFIXED',  'SUN', &
                      'LT'                                             )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)',   OK )


!
!     Restore original radii.
!
      CALL PDPOOL ( 'BODY1000_RADII', 3, RADII )
      CALL CHCKXC ( .FALSE., ' ',   OK )




!
!---- Case -------------------------------------------------------------
!

      CALL TCASE ( 'ZZGFOCIN: bad radius count for front target' )


      CALL BODVRD ( 'BETA', 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL MOVED ( RADII, 3, NEWRAD )
      NEWRAD(1) = 0.D0

      CALL PDPOOL ( 'BODY2000_RADII', 2, NEWRAD )

      CALL ZZGFOCIN ( 'Any',   'BETA',       'ellipsoid',   'BETAFIXED', &
                      'ALPHA', 'ellipsoid',  'ALPHAFIXED',  'SUN', &
                      'LT'                                             )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)',   OK )


!
!     Restore original radii.
!
      CALL PDPOOL ( 'BODY2000_RADII', 3, RADII )
      CALL CHCKXC ( .FALSE., ' ',   OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'ZZGFOCIN: blank frames for ellipsoidal targets' )

      CALL ZZGFOCIN ( 'Any',  'ALPHA',      'ELLIPSOID',  ' ', &
                      'BETA', 'ellipsoid',  'BETAFIXED',  'SUN', &
                      'LT'     )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)',   OK )


      CALL ZZGFOCIN ( 'Any',  'ALPHA',      'ELLIPSOID',  'ALPHAFIXED', &
                      'BETA', 'ellipsoid',  ' ',  'SUN', &
                      'LT'     )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)',   OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'ZZGFOCIN: bad frame centers for ellipsoidal ' &
      //           'targets' )

      CALL ZZGFOCIN ( 'Any',  'ALPHA',      'ELLIPSOID',  'BETAFIXED', &
                      'BETA', 'ellipsoid',  'BETAFIXED',  'SUN', &
                      'LT'     )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)',   OK )


      CALL ZZGFOCIN ( 'Any',  'ALPHA',      'ELLIPSOID',  'ALPHAFIXED', &
                      'BETA', 'ellipsoid',  'ALPHAFIXED', 'SUN', &
                      'LT'     )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)',   OK )


!
!---- Case -------------------------------------------------------------
!

      CALL TCASE ( 'ZZGFOCIN: bad shape combination' )



      CALL ZZGFOCIN ( 'Any',   'BETA',   'point',   'BETAFIXED', &
                      'ALPHA', 'point',  'ALPHAFIXED',  'SUN', &
                      'LT'                                             )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSHAPECOMBO)',   OK )



!---- Case -------------------------------------------------------------
!

      CALL TCASE ( 'ZZGFOCIN: bad target shape' )



      CALL ZZGFOCIN ( 'Any',   'BETA',   'sphere',      'BETAFIXED', &
                      'ALPHA', 'point',  'ALPHAFIXED',  'SUN', &
                      'LT'                                             )
      CALL CHCKXC ( .TRUE., 'SPICE(BADMETHODSYNTAX)',   OK )


      CALL ZZGFOCIN ( 'Any',   'BETA',   'ELLIPSOID',   'BETAFIXED', &
                      'ALPHA', 'sphere',  'ALPHAFIXED', 'SUN', &
                      'LT'                                             )
      CALL CHCKXC ( .TRUE., 'SPICE(BADMETHODSYNTAX)',   OK )



!
!---- Case -------------------------------------------------------------
!

      CALL TCASE ( 'ZZGFIIOC: ellipsoid case: participants coincide' )



!
!     Front ellipsoid target is observer.
!
      CALL ZZGFOCIN ( 'Any',   'BETA',       'ellipsoid',   'BETAFIXED', &
                      'ALPHA', 'ellipsoid',  'ALPHAFIXED',  'BETA', &
                      'LT'                                             )
      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)',   OK )

!      ET = 0.D0

!      CALL ZZGFIOCC ( ET, OCSTAT )
!      CALL CHCKXC ( .TRUE., 'SPICE(NOTDISJOINT)',   OK )


!
!     Back ellipsoid target is observer.
!
      CALL ZZGFOCIN ( 'Any',   'BETA',       'ellipsoid',   'BETAFIXED', &
                      'ALPHA', 'ellipsoid',  'ALPHAFIXED',  'ALPHA', &
                      'LT'                                             )
      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)',   OK )


!      ET = 0.D0

!      CALL ZZGFIOCC ( ET, OCSTAT )
!      CALL CHCKXC ( .TRUE., 'SPICE(NOTDISJOINT)',   OK )



!
!     Front ellipsoid target is back target.
!
      CALL ZZGFOCIN ( 'Any',   'BETA',       'ellipsoid',  'BETAFIXED', &
                      'BETA', 'ellipsoid',  'BETAFIXED',   'SUN', &
                      'LT'                                             )
      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)',   OK )

!      CALL CHCKXC ( .FALSE., ' ',   OK )
!      CALL CHCKXC ( .FALSE., ' ',   OK )

!      ET = 0.D0

!      CALL ZZGFIOCC ( ET, OCSTAT )
!      CALL CHCKXC ( .TRUE., 'SPICE(NOTDISJOINT)',   OK )




!
!---- Case -------------------------------------------------------------
!

      CALL TCASE ( 'ZZGFIIOC: point target case: participants ' &
      //           'coincide' )



!
!     Front point target is observer.
!
      CALL ZZGFOCIN ( 'Any',   'BETA',       'point',       'BETAFIXED', &
                      'ALPHA', 'ellipsoid',  'ALPHAFIXED',  'BETA', &
                      'LT'                                             )
      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)',   OK )


!
!     Back point target is observer.
!
      CALL ZZGFOCIN ( 'Any',   'BETA',   'ellipsoid',  'BETAFIXED', &
                      'ALPHA', 'point',  'ALPHAFIXED',   'ALPHA', &
                      'LT'                                             )
      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)',   OK )


!
!     Front point target is back target.
!
      CALL ZZGFOCIN ( 'Any',   'BETA',       'point',      'BETAFIXED', &
                      'BETA', 'ellipsoid',  'BETAFIXED',   'SUN', &
                      'LT'                                             )
      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)',   OK )



!
!     Back point target is front target.
!
      CALL ZZGFOCIN ( 'Any',   'BETA',   'ellipsoid',  'BETAFIXED', &
                      'BETA',  'point',  'BETAFIXED',   'SUN', &
                      'LT'                                             )
      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)',   OK )





!
!---- Case -------------------------------------------------------------
!

      CALL TCASE ( 'Bad aberration correction values.' )

      CALL ZZGFOCIN ( 'Any',   'ALPHA',      'point',     'NULL', &
                      'BETA',  'ELLIPSOID',  'BETAFIXED', 'SUN', &
                      'S'                                       )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )



      CALL ZZGFOCIN ( 'Any',   'ALPHA',      'point',     'NULL', &
                      'BETA',  'ELLIPSOID',  'BETAFIXED', 'SUN', &
                      'XS'                                       )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )



      CALL ZZGFOCIN ( 'Any',   'ALPHA',      'point',     'NULL', &
                      'BETA',  'ELLIPSOID',  'BETAFIXED', 'SUN', &
                      'RLT'                                       )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )



      CALL ZZGFOCIN ( 'Any',   'ALPHA',      'point',     'NULL', &
                      'BETA',  'ELLIPSOID',  'BETAFIXED', 'SUN', &
                      'XRLT'                                     )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )


      CALL ZZGFOCIN ( 'Any',   'ALPHA',      'point',     'NULL', &
                      'BETA',  'ELLIPSOID',  'BETAFIXED', 'SUN', &
                      'z'                                       )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )






!
!---- Case -------------------------------------------------------------
!

      CALL TCASE ( 'Clean up. Unload and delete kernels.' )

!
!     Clean up the SPK file.
!
      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL DELFIL ( SPK )
      CALL CHCKXC ( .FALSE., ' ',   OK )


      CALL T_SUCCESS ( OK )

      RETURN
      END


