C$Procedure      F_ZZGFOCU ( ZZGFOCU family tests )

      SUBROUTINE F_ZZGFOCU ( OK )

C$ Abstract
C
C     This routine tests *some functionality* of the SPICELIB routine
C     
C        ZZGFOCU
C
C     To fully exercise ZZGFOCU, the test family 
C
C        F_GFOCLT
C
C     must be run in addition to this family.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     TEST FAMILY
C
C$ Declarations
 
      IMPLICIT NONE

      LOGICAL               OK
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OK         O   logical indicating test status.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     OK         is a logical that indicates the test status to the
C                caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting.  The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the low-level SPICELIB 
C     geometry utility package ZZGFOCU.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman     (JPL)
C
C$ Version
C
C-    TSPICE Version 4.1.0, 25-AUG-2021 (EDW)
C
C        Edited all error strings on radii values, and number of values
C        returned from kernel pool, to correspond to us-of/changes-in 
C        ZZGFTREB.
C
C-    TSPICE Version 4.0.0, 22-FEB-2016 (NJB)
C
C        Updated expected keywords for invalid shape cases.
C
C-    TSPICE Version 3.1.0, 14-APR-2008 (NJB)
C
C        Kernel clean-up now consists only of unloading
C        the SPK file via SPKUEF and then deleting the
C        file.
C
C-    TSPICE Version 3.0.0, 12-APR-2008 (NJB)
C
C        Updated to use new names ZZGFOCU and F_ZZGFOCU.
C        "Header" such as it is now passes checkit tests.
C
C-    TSPICE Version 2.0.0, 29-MAR-2008 (NJB)
C
C        Updated to exercise new ZZGFOCIN error checks.
C
C-    TSPICE Version 1.1.0, 13-MAR-2008 (NJB)
C
C        Removed unnecessary variable STEP.
C
C-    TSPICE Version 1.0.0, 16-NOV-2007 (NJB)
C
C-&
 
C
C     SPICELIB functions
C

C     
C     Local parameters
C
      CHARACTER*(*)         PCK 
      PARAMETER           ( PCK = 'nat.tpc' )

      CHARACTER*(*)         SPK
      PARAMETER           ( SPK = 'nat.bsp' )


C
C     Local variables
C
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      NEWRAD ( 3 )
      DOUBLE PRECISION      RADII  ( 3 )

      INTEGER               HANDLE
      INTEGER               N

      LOGICAL               OCSTAT

C
C     Save everything.
C
      SAVE
 
C
C     Initial values
C
 
C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ZZGFOCU' )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Setup: create and load SPK, PCK, LSK files.' )

      CALL NATSPK ( SPK, .TRUE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ',   OK )
 
      CALL NATPCK ( PCK, .TRUE., .FALSE. )
      CALL CHCKXC ( .FALSE., ' ',   OK )
      
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ',   OK )


C*********************************************************************
C*
C*    Error cases
C*
C*********************************************************************


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'ZZGFOCU: bogus entry' )

      ET = 0.D0

      CALL ZZGFOCU ( 'Any',  'ALPHAx',     'point',      'ALPHAFIXED',
     .               'BETA', 'ellipsoid',  'BETAFIXED',  'SUN',
     .               'LT', ET, OCSTAT     )
      CALL CHCKXC ( .TRUE., 'SPICE(BOGUSENTRY)',   OK )




C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'ZZGFOCIN: Unrecognized targets or observer' )

      CALL ZZGFOCIN ( 'Any',  'ALPHAx',     'point',      'ALPHAFIXED',
     .                'BETA', 'ellipsoid',  'BETAFIXED',  'SUN',
     .                'LT'     )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )


      CALL ZZGFOCIN ( 'Any',   'ALPHA',      'point',      'ALPHAFIXED',
     .                'BETAx', 'ellipsoid',  'BETAFIXED',  'SUN',
     .                'LT'     )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )


      CALL ZZGFOCIN ( 'Any',   'ALPHA',      'point',      'ALPHAFIXED',
     .                'BETA',  'ellipsoid',  'BETAFIXED',  'SUNx',
     .                'LT'     )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )




C
C---- Case -------------------------------------------------------------
C

      CALL TCASE ( 'ZZGFOCIN: non-positive radii for back target' )


      CALL BODVRD ( 'ALPHA', 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL MOVED ( RADII, 3, NEWRAD )
      NEWRAD(1) = 0.D0

      CALL PDPOOL ( 'BODY1000_RADII', 3, NEWRAD )

      CALL ZZGFOCIN ( 'Any',   'BETA',       'point',       'BETAFIXED',
     .                'ALPHA', 'ellipsoid',  'ALPHAFIXED',  'SUN',
     .                'LT'                                            )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)',   OK )



      CALL MOVED ( RADII, 3, NEWRAD )
      NEWRAD(2) = -1.D0

      CALL PDPOOL ( 'BODY1000_RADII', 3, NEWRAD )

      CALL ZZGFOCIN ( 'Any',   'BETA',       'point',       'BETAFIXED',
     .                'ALPHA', 'ellipsoid',  'ALPHAFIXED',  'SUN',
     .                'LT'                                             )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)',   OK )

 

      CALL MOVED ( RADII, 3, NEWRAD )
      NEWRAD(3) = -1.D0

      CALL PDPOOL ( 'BODY1000_RADII', 3, NEWRAD )

      CALL ZZGFOCIN ( 'Any',   'BETA',       'point',       'BETAFIXED',
     .                'ALPHA', 'ellipsoid',  'ALPHAFIXED',  'SUN',
     .                'LT'                                            )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)',   OK )


C
C     Restore original radii.
C
      CALL PDPOOL ( 'BODY1000_RADII', 3, RADII )
      CALL CHCKXC ( .FALSE., ' ',   OK )


C
C---- Case -------------------------------------------------------------
C

      CALL TCASE ( 'ZZGFOCIN: non-positive radii for front target' )


      CALL BODVRD ( 'BETA', 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL MOVED ( RADII, 3, NEWRAD )
      NEWRAD(1) = 0.D0

      CALL PDPOOL ( 'BODY2000_RADII', 3, NEWRAD )

      CALL ZZGFOCIN ( 'Any',   'BETA',       'ellipsoid',   'BETAFIXED',
     .                'ALPHA', 'ellipsoid',  'ALPHAFIXED',  'SUN',
     .                'LT'                                             )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)',   OK )



      CALL MOVED ( RADII, 3, NEWRAD )
      NEWRAD(2) = -1.D0

      CALL PDPOOL ( 'BODY2000_RADII', 3, NEWRAD )

      CALL ZZGFOCIN ( 'Any',   'BETA',       'ellipsoid',   'BETAFIXED',
     .                'ALPHA', 'ellipsoid',  'ALPHAFIXED',  'SUN',
     .                'LT'                                             )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)',   OK )



      CALL MOVED ( RADII, 3, NEWRAD )
      NEWRAD(3) = -1.D0

      CALL PDPOOL ( 'BODY2000_RADII', 3, NEWRAD )

      CALL ZZGFOCIN ( 'Any',   'BETA',       'ellipsoid',   'BETAFIXED',
     .                'ALPHA', 'ellipsoid',  'ALPHAFIXED',  'SUN',
     .                'LT'                                             )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)',   OK )


C
C     Restore original radii.
C
      CALL PDPOOL ( 'BODY2000_RADII', 3, RADII )
      CALL CHCKXC ( .FALSE., ' ',   OK )


C
C---- Case -------------------------------------------------------------
C

      CALL TCASE ( 'ZZGFOCIN: bad radius count for back target' )


      CALL BODVRD ( 'ALPHA', 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL MOVED ( RADII, 3, NEWRAD )
      NEWRAD(1) = 0.D0

      CALL PDPOOL ( 'BODY1000_RADII', 2, NEWRAD )

      CALL ZZGFOCIN ( 'Any',   'BETA',       'point',       'BETAFIXED',
     .                'ALPHA', 'ellipsoid',  'ALPHAFIXED',  'SUN',
     .                'LT'                                             )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)',   OK )


C
C     Restore original radii.
C
      CALL PDPOOL ( 'BODY1000_RADII', 3, RADII )
      CALL CHCKXC ( .FALSE., ' ',   OK )




C
C---- Case -------------------------------------------------------------
C

      CALL TCASE ( 'ZZGFOCIN: bad radius count for front target' )


      CALL BODVRD ( 'BETA', 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL MOVED ( RADII, 3, NEWRAD )
      NEWRAD(1) = 0.D0

      CALL PDPOOL ( 'BODY2000_RADII', 2, NEWRAD )

      CALL ZZGFOCIN ( 'Any',   'BETA',       'ellipsoid',   'BETAFIXED',
     .                'ALPHA', 'ellipsoid',  'ALPHAFIXED',  'SUN',
     .                'LT'                                             )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)',   OK )


C
C     Restore original radii.
C
      CALL PDPOOL ( 'BODY2000_RADII', 3, RADII )
      CALL CHCKXC ( .FALSE., ' ',   OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'ZZGFOCIN: blank frames for ellipsoidal targets' )

      CALL ZZGFOCIN ( 'Any',  'ALPHA',      'ELLIPSOID',  ' ',
     .                'BETA', 'ellipsoid',  'BETAFIXED',  'SUN',
     .                'LT'     )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)',   OK )


      CALL ZZGFOCIN ( 'Any',  'ALPHA',      'ELLIPSOID',  'ALPHAFIXED',
     .                'BETA', 'ellipsoid',  ' ',  'SUN',
     .                'LT'     )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)',   OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'ZZGFOCIN: bad frame centers for ellipsoidal '
     .//           'targets' )

      CALL ZZGFOCIN ( 'Any',  'ALPHA',      'ELLIPSOID',  'BETAFIXED',
     .                'BETA', 'ellipsoid',  'BETAFIXED',  'SUN',
     .                'LT'     )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)',   OK )


      CALL ZZGFOCIN ( 'Any',  'ALPHA',      'ELLIPSOID',  'ALPHAFIXED',
     .                'BETA', 'ellipsoid',  'ALPHAFIXED', 'SUN',
     .                'LT'     )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)',   OK )


C
C---- Case -------------------------------------------------------------
C

      CALL TCASE ( 'ZZGFOCIN: bad shape combination' )



      CALL ZZGFOCIN ( 'Any',   'BETA',   'point',   'BETAFIXED',
     .                'ALPHA', 'point',  'ALPHAFIXED',  'SUN',
     .                'LT'                                             )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSHAPECOMBO)',   OK )



C---- Case -------------------------------------------------------------
C

      CALL TCASE ( 'ZZGFOCIN: bad target shape' )



      CALL ZZGFOCIN ( 'Any',   'BETA',   'sphere',      'BETAFIXED',
     .                'ALPHA', 'point',  'ALPHAFIXED',  'SUN',
     .                'LT'                                             )
      CALL CHCKXC ( .TRUE., 'SPICE(BADMETHODSYNTAX)',   OK )


      CALL ZZGFOCIN ( 'Any',   'BETA',   'ELLIPSOID',   'BETAFIXED',
     .                'ALPHA', 'sphere',  'ALPHAFIXED', 'SUN',
     .                'LT'                                             )
      CALL CHCKXC ( .TRUE., 'SPICE(BADMETHODSYNTAX)',   OK )



C
C---- Case -------------------------------------------------------------
C

      CALL TCASE ( 'ZZGFIIOC: ellipsoid case: participants coincide' )



C
C     Front ellipsoid target is observer.
C
      CALL ZZGFOCIN ( 'Any',   'BETA',       'ellipsoid',   'BETAFIXED',
     .                'ALPHA', 'ellipsoid',  'ALPHAFIXED',  'BETA',
     .                'LT'                                             )
      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)',   OK )

C      ET = 0.D0

C      CALL ZZGFIOCC ( ET, OCSTAT )
C      CALL CHCKXC ( .TRUE., 'SPICE(NOTDISJOINT)',   OK )


C
C     Back ellipsoid target is observer.
C
      CALL ZZGFOCIN ( 'Any',   'BETA',       'ellipsoid',   'BETAFIXED',
     .                'ALPHA', 'ellipsoid',  'ALPHAFIXED',  'ALPHA',
     .                'LT'                                             )
      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)',   OK )


C      ET = 0.D0

C      CALL ZZGFIOCC ( ET, OCSTAT )
C      CALL CHCKXC ( .TRUE., 'SPICE(NOTDISJOINT)',   OK )



C
C     Front ellipsoid target is back target.
C
      CALL ZZGFOCIN ( 'Any',   'BETA',       'ellipsoid',  'BETAFIXED',
     .                'BETA', 'ellipsoid',  'BETAFIXED',   'SUN',
     .                'LT'                                             )
      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)',   OK )

C      CALL CHCKXC ( .FALSE., ' ',   OK )
C      CALL CHCKXC ( .FALSE., ' ',   OK )

C      ET = 0.D0

C      CALL ZZGFIOCC ( ET, OCSTAT )
C      CALL CHCKXC ( .TRUE., 'SPICE(NOTDISJOINT)',   OK )




C
C---- Case -------------------------------------------------------------
C

      CALL TCASE ( 'ZZGFIIOC: point target case: participants '
     .//           'coincide' )



C
C     Front point target is observer.
C
      CALL ZZGFOCIN ( 'Any',   'BETA',       'point',       'BETAFIXED',
     .                'ALPHA', 'ellipsoid',  'ALPHAFIXED',  'BETA',
     .                'LT'                                             )
      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)',   OK )


C
C     Back point target is observer.
C
      CALL ZZGFOCIN ( 'Any',   'BETA',   'ellipsoid',  'BETAFIXED',
     .                'ALPHA', 'point',  'ALPHAFIXED',   'ALPHA',
     .                'LT'                                             )
      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)',   OK )


C
C     Front point target is back target.
C
      CALL ZZGFOCIN ( 'Any',   'BETA',       'point',      'BETAFIXED',
     .                'BETA', 'ellipsoid',  'BETAFIXED',   'SUN',
     .                'LT'                                             )
      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)',   OK )



C
C     Back point target is front target.
C
      CALL ZZGFOCIN ( 'Any',   'BETA',   'ellipsoid',  'BETAFIXED',
     .                'BETA',  'point',  'BETAFIXED',   'SUN',
     .                'LT'                                             )
      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)',   OK )





C
C---- Case -------------------------------------------------------------
C

      CALL TCASE ( 'Bad aberration correction values.' )

      CALL ZZGFOCIN ( 'Any',   'ALPHA',      'point',     'NULL',
     .                'BETA',  'ELLIPSOID',  'BETAFIXED', 'SUN',
     .                'S'                                       )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )



      CALL ZZGFOCIN ( 'Any',   'ALPHA',      'point',     'NULL',
     .                'BETA',  'ELLIPSOID',  'BETAFIXED', 'SUN',
     .                'XS'                                       )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )



      CALL ZZGFOCIN ( 'Any',   'ALPHA',      'point',     'NULL',
     .                'BETA',  'ELLIPSOID',  'BETAFIXED', 'SUN',
     .                'RLT'                                       )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )



      CALL ZZGFOCIN ( 'Any',   'ALPHA',      'point',     'NULL',
     .                'BETA',  'ELLIPSOID',  'BETAFIXED', 'SUN',
     .                'XRLT'                                     )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )


      CALL ZZGFOCIN ( 'Any',   'ALPHA',      'point',     'NULL',
     .                'BETA',  'ELLIPSOID',  'BETAFIXED', 'SUN',
     .                'z'                                       )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )






C
C---- Case -------------------------------------------------------------
C

      CALL TCASE ( 'Clean up. Unload and delete kernels.' )

C
C     Clean up the SPK file.
C      
      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL DELFIL ( SPK )
      CALL CHCKXC ( .FALSE., ' ',   OK )


      CALL T_SUCCESS ( OK )

      RETURN
      END


