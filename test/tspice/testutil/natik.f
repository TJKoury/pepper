C$Procedure   NATIK ( Create an IK for Nat's solar system )

      SUBROUTINE NATIK ( IK, SPK, PCK, LOADIK, KEEPIK )

C$ Abstract
C
C     Create and if appropriate load an IK for Nat's
C     solar system.
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
C     TESTING
C
C$ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         IK
      CHARACTER*(*)         SPK
      CHARACTER*(*)         PCK
      LOGICAL               LOADIK
      LOGICAL               KEEPIK

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     IK         I   Name of the I-kernel to create.
C     SPK        I   Name of an existing Nat's solar system SPK.
C     PCK        I   Name of an existing Nat's solar system PCK.
C     LOADIK     I   Load the I-kernel if .TRUE.
C     KEEPIK     I   Keep the I-kernel if .TRUE., else delete it.
C
C$ Detailed_Input
C
C     IK          is the name of an IK to create and load if LOADIK is
C                 set to .TRUE. If an IK of the same name already
C                 exists, the existing file is deleted.
C
C     SPK         is the name of an existing Nat's solar system SPK.
C                 This SPK is required to define dynamic two-vector
C                 frames which require the positions of bodies Alpha
C                 and Beta with respect to the Sun.
C
C     PCK         is the name of an existing Nat's solar system PCK.
C                 This PCK is required to define dynamic two-vector
C                 frames which require the positions of bodies Alpha
C                 and Beta with respect to the Sun.
C
C     LOADIK      is a logical that indicates whether or not the IK
C                 file should be loaded after it is created.  If LOADIK
C                 has the value .TRUE. the IK is loaded after it is
C                 created.  Otherwise it is left un-opened.
C
C     KEEPIK      is a logical that indicates whether or not the IK
C                 file should be deleted after it is loaded.  If KEEPIK
C                 is .TRUE. the file is not deleted.  If KEEPIK is
C                 .FALSE. the file is deleted after it is loaded.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) Any I/O errors occurring in the creation of the IK file
C        will be signaled by routines in the call tree of this
C        routine.
C
C     Since this routine is normally used within the TSPICE system,
C     it's up to the caller to call CHCKXC to catch errors signaled by
C     this routine.
C
C$ Files
C
C     This routine creates an IK containing FOV specifications for four
C     "instruments." The instruments' frame and FOV specifications are
C     designed so that appearances of body Beta in the FOVs coincide
C     with transits of Beta across Alpha, thus yielding simple,
C     predictable
C     event timing.
C
C     The instruments' names are
C
C        ALPHA_CIRCLE_NONE
C        ALPHA_DIAMOND_NONE
C        ALPHA_ELLIPSE_NONE
C        ALPHA_RECTANGLE_NONE
C
C     The first component of each name is the body tracked by the
C     instrument's reference frame.
C
C     The second component is the FOV shape; each shape specifier
C     supported by GETFOV is supported here. The instrument
C     ALPHA_DIAMOND_NONE uses the 'POLYGON' shape specifier.
C
C     The third component of each name is the aberration correction
C     applied to the position vectors used in the frame specification.
C
C     The reference frames associated with the instruments are,
C     respectively
C
C        ALPHA_VIEW_XY
C        ALPHA_VIEW_XZ
C        ALPHA_VIEW_XY
C        ALPHA_VIEW_XY
C
C     Note that the frame of ALPHA_DIAMOND_NONE has been selected
C     so that body Beta orbits in the X-Z plane of that frame.
C
C$ Particulars
C
C     This routine creates an IK file for use in testing. The
C     file is registered with the TSPICE system via a call to
C     TFILES.
C
C$ Examples
C
C     The normal way to use this routine is shown below.
C
C     CALL NATSPK ( 'nat.bsp', .TRUE., HANDLE  )
C     CALL NATPCK ( 'nat.tpc', .TRUE., .TRUE.  )
C     CALL NATIK  ( 'nat.ti',  .TRUE., .FALSE. )
C
C        [perform some tests and computations]
C
C     CALL SPKUEF ( HANDLE )
C     CALL KILFIL ( 'nat.bsp' )
C
C     If these calls are made within a TSPICE test family, the
C     kernels will be deleted at the time the test family returns.
C     In other settings, the caller will need to clean up the
C     PCK file via an explicit deletion.
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
C     N.J. Bachman    (JPL)
C
C$ Version
C
C-    Test Utilities Version 2.0.0, 08-OCT-2021 (NJB)
C
C        Changed boresight length of instrument ALPHA_ELLIPSE_NONE
C        to 2. This suffices to demonstrate an error in the N0066
C        version of ZZGFFVIN. Corrected several comment typos.
C
C        Bug fix: removed unnecessary REPMD call used to create
C        a portion of the IK setting a boundary vector for instrument
C        ALPHA_CIRCLE_NONE. This call had no effect but was correctly
C        identified by Valgrind.
C
C-    Test Utilities 1.0.0, 29-JUL-2008 (NJB)
C
C-&

C$ Index_Entries
C
C     Create a "Nat's solar system" IK file
C
C-&





C
C     Test Utility Functions
C
      CHARACTER*(16)        BEGDAT
      CHARACTER*(16)        BEGTXT

C
C     Local Parameters
C
      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               NLINES
      PARAMETER           ( NLINES = 100 )

      INTEGER               UBEL
      PARAMETER           ( UBEL   = 9 )

      INTEGER               MAXVRT
      PARAMETER           ( MAXVRT = 10 )

C
C     Local variables
C
      CHARACTER*(LNSIZE)    TX     ( NLINES )
      CHARACTER*(BDNMLN)    INSNAM

      DOUBLE PRECISION      BSITE  ( 3 )
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      BNDS   ( 3, MAXVRT )
      DOUBLE PRECISION      BVEC   ( 3, MAXVRT )
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      LIMB   ( UBEL )
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      POS    ( 3 )
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      SMAJOR ( 3 )
      DOUBLE PRECISION      SMINOR ( 3 )
      DOUBLE PRECISION      VIEWPT ( 3 )
      DOUBLE PRECISION      XFORM  ( 3, 3 )
      DOUBLE PRECISION      VTEMP  ( 3 )

      INTEGER               HANDLE
      INTEGER               I
      INTEGER               INST
      INTEGER               IXBCB
      INTEGER               IXBSB
      INTEGER               IXINB
      INTEGER               IXINE
      INTEGER               J
      INTEGER               L
      INTEGER               N
      INTEGER               NL
      INTEGER               NV
      INTEGER               UNIT


      CALL CHKIN ( 'NATIK' )

C
C     Delete any existing file of the same name.
C
      CALL KILFIL ( IK )

C
C     Before creating the IK, we derive the parameters
C     needed to create the instrument FOVs we'll specify there.
C
C     Load the Nat's solar system SPK and PCK.
C
      CALL SPKLEF ( SPK, HANDLE )
      CALL LDPOOL ( PCK )

      CALL BODVRD ( 'ALPHA', 'RADII', 3, N, RADII )

C
C     Compute the limb of body Alpha as seen from the Sun
C     at 0 seconds past J2000 TDB. Do not use aberration
C     corrections.
C
      ET0 = 0.D0

      CALL SPKPOS ( 'ALPHA', ET0, 'ALPHAFIXED', 'NONE', 'SUN', POS, LT )

      CALL VMINUS ( POS, VIEWPT )

      CALL EDLIMB ( RADII(1), RADII(2), RADII(3), VIEWPT, LIMB )

C
C     Create boundary vectors for an elliptical FOV in the
C     ALPHAFIXED frame.
C
      CALL EL2CGV ( LIMB, CENTER, SMAJOR, SMINOR )





C**********************************************************************
C
C     Create FOV specification and instrument name-ID mapping
C     for the instrument "Alpha_Ellipse_NONE."
C
C**********************************************************************

C
C     This FOV is fixed to the dynamic frame ALPHA_VIEW_XY.
C     Thus the instrument "tracks" body Alpha. As seen from the
C     Sun, with aberration corrections turned off,
C     the FOV coincides with the limb of Alpha.
C

      INSNAM  = 'ALPHA_ELLIPSE_NONE'
      INST    = -1500000

C
C     Look up the transformation from the ALPHAFIXED frame to the
C     ALPHA_VIEW_XY frame. The latter rotates with the orbital motion
C     of planet Alpha.
C
      CALL PXFORM ( 'ALPHAFIXED', 'ALPHA_VIEW_XY', ET0, XFORM )

C
C     Compute the boresight vector.
C
      CALL MXV  ( XFORM, POS, VTEMP )
      CALL VHAT ( VTEMP,      BSITE )

C
C     New!
C
      CALL VSCLIP( 2.0D0, BSITE )


      CALL VLCOM3 ( 1.D0,  CENTER,
     .              1.D0,  SMAJOR,
     .             -1.D0,  VIEWPT,  BVEC(1,1)  )


      CALL VLCOM3 ( 1.D0,  CENTER,
     .              1.D0,  SMINOR,
     .             -1.D0,  VIEWPT,  BVEC(1,2)  )


      DO I = 1, 2
         CALL MXV ( XFORM, BVEC(1,I), BNDS(1,I) )
      END DO

C
C     As obnoxious as the practice may seem, we'll use the variable
C     L instead of hard-coded constants to index buffer elements.
C     Hard-coded indices are a big problem when changes are needed.
C
      L       = 1
      TX(L)   = ' '
      L       = L+1
      TX(L)   = ' KPL/IK'
      L       = L+1
      TX(L)   = ' '
      L       = L+1
      TX(L)   = BEGDAT()
      L       = L+1
      TX(L)   = ' '
      L       = L+1

      TX(L)   = 'NAIF_BODY_NAME += ''#'' '
      CALL REPMC ( TX(L), '#', INSNAM, TX(L) )
      L       = L+1

      TX(L)   = 'NAIF_BODY_CODE += #'
      CALL REPMI ( TX(L), '#', INST,   TX(L) )
      L       = L+1

      TX(L)   = ' '
      L       = L+1

      IXINB   = L
      TX(L)   = 'INS#_FOV_SHAPE            = ''ELLIPSE'' '
      L       = L+1
      TX(L)  = 'INS#_FOV_FRAME            = ''ALPHA_VIEW_XY'' '
      L       = L+1
      IXBSB   = L
      TX(L)  = 'INS#_BORESIGHT            = ( *'
      L       = L+1
      TX(L)   = '                                     *'
      L       = L+1
      TX(L)   = '                                     * )'
      L       = L+1
      IXBCB   = L
      TX(L)   = 'INS#_FOV_BOUNDARY         = ( *'
      IXINE   = L
      L       = L+1
      TX(L)   = '                                     *'
      L       = L+1
      TX(L)   = '                                     *'
      L       = L+1
      TX(L)   = '                                     *'
      L       = L+1
      TX(L)   = '                                     *'
      L       = L+1
      TX(L)   = '                                     * )'
      L       = L+1
      TX(L)   = ' '
      L       = L+1
      TX(L)   = BEGTXT()
      L       = L+1

C
C     Set the instrument ID in the keywords above.
C
      DO I = IXINB, IXINE
         CALL REPMI ( TX(I), '#', INST, TX(I) )
      END DO

C
C     Set the boresight values.
C
      J = IXBSB-1

      DO I = 1, 3
         CALL REPMD ( TX(J+I), '*', BSITE(I), 14, TX(J+I) )
      END DO

C
C     Set the boundary corner values.
C
      J = IXBCB-1

      DO I = 1, 3
         CALL REPMD ( TX(J+I  ), '*', BNDS(I,1), 14, TX(J+I  ) )
         CALL REPMD ( TX(J+I+3), '*', BNDS(I,2), 14, TX(J+I+3) )
      END DO




C**********************************************************************
C
C     Create FOV specification and instrument name-ID mapping
C     for the instrument "Alpha_Circle_NONE."
C
C**********************************************************************

C
C     This FOV is fixed to the dynamic frame ALPHA_VIEW_XY.
C     Thus the instrument "tracks" body Alpha. As seen from the
C     Sun, with aberration corrections turned off,
C     the FOV is inscribed in the limb of Alpha.
C

      INSNAM  = 'ALPHA_CIRCLE_NONE'
      INST    = -1500001

C
C     Look up the transformation from the ALPHAFIXED frame to the
C     ALPHA_VIEW_XY frame. The latter rotates with the orbital motion
C     of planet Alpha.
C
      CALL PXFORM ( 'ALPHAFIXED', 'ALPHA_VIEW_XY', ET0, XFORM )

C
C     Compute the boresight vector.
C
      CALL MXV  ( XFORM, POS, VTEMP )
      CALL VHAT ( VTEMP,      BSITE )


C
C     We use the semi-minor axis to define the boundary vector,
C     since we want our circle to be inscribed in Alpha's limb.
C
      CALL VLCOM3 ( 1.D0,  CENTER,
     .              1.D0,  SMINOR,
     .             -1.D0,  VIEWPT,  BVEC(1,1)  )


      CALL MXV ( XFORM, BVEC(1,1), BNDS(1,1) )

      TX(L)   = ' '
      L       = L+1
      TX(L)   = ' KPL/TX'
      L       = L+1
      TX(L)   = ' '
      L       = L+1
      TX(L)   = BEGDAT()
      L       = L+1
      TX(L)   = ' '
      L       = L+1

      TX(L)   = 'NAIF_BODY_NAME += ''#'' '
      CALL REPMC ( TX(L), '#', INSNAM, TX(L) )
      L       = L+1

      TX(L)   = 'NAIF_BODY_CODE += #'
      CALL REPMI ( TX(L), '#', INST,   TX(L) )
      L       = L+1

      TX(L)   = ' '
      L       = L+1

      IXINB   = L
      TX(L)   = 'INS#_FOV_SHAPE            = ''CIRCLE'' '
      L       = L+1
      TX(L)  = 'INS#_FOV_FRAME            = ''ALPHA_VIEW_XY'' '
      L       = L+1
      IXBSB   = L
      TX(L)  = 'INS#_BORESIGHT            = ( *'
      L       = L+1
      TX(L)   = '                                     *'
      L       = L+1
      TX(L)   = '                                     * )'
      L       = L+1
      IXBCB   = L
      TX(L)   = 'INS#_FOV_BOUNDARY         = ( *'
      IXINE   = L
      L       = L+1
      TX(L)   = '                                     *'
      L       = L+1
      TX(L)   = '                                     * )'
      L       = L+1
      TX(L)   = ' '
      L       = L+1
      TX(L)   = BEGTXT()
      L       = L+1

C
C     Set the instrument ID in the keywords above.
C
      DO I = IXINB, IXINE
         CALL REPMI ( TX(I), '#', INST, TX(I) )
      END DO

C
C     Set the boresight values.
C
      J = IXBSB-1

      DO I = 1, 3
         CALL REPMD ( TX(J+I), '*', BSITE(I), 14, TX(J+I) )
      END DO

C
C     Set the boundary corner values.
C
      J = IXBCB-1

      DO I = 1, 3
         CALL REPMD ( TX(J+I  ), '*', BNDS(I,1), 14, TX(J+I  ) )
      END DO



C**********************************************************************
C
C     Create FOV specification and instrument name-ID mapping
C     for the instrument "Alpha_Rectangle_NONE."
C
C**********************************************************************

C
C     This FOV is fixed to the dynamic frame ALPHA_VIEW_XY. Thus the
C     instrument "tracks" body Alpha. As seen from the Sun, with
C     aberration corrections turned off, the FOV circumscribes the limb
C     of Alpha.
C

      INSNAM  = 'ALPHA_RECTANGLE_NONE'
      INST    = -1500003
      NV      = 4


C
C     Look up the transformation from the ALPHAFIXED frame to the
C     ALPHA_VIEW_XY frame. The latter rotates with the orbital motion
C     of planet Alpha.
C
      CALL PXFORM ( 'ALPHAFIXED', 'ALPHA_VIEW_XY', ET0, XFORM )

C
C     Compute the boresight vector.
C
      CALL MXV  ( XFORM, POS, VTEMP )
      CALL VHAT ( VTEMP,      BSITE )


C
C     Compute the boundary vectors.
C
      CALL VSUB ( CENTER, VIEWPT, VTEMP )

      CALL VLCOM3 (  1.D0,  SMINOR,
     .               1.D0,  SMAJOR,
     .               1.D0,  VTEMP,  BVEC(1,1)  )

      CALL VLCOM3 ( -1.D0,  SMINOR,
     .               1.D0,  SMAJOR,
     .               1.D0,  VTEMP,  BVEC(1,2)  )

      CALL VLCOM3 ( -1.D0,  SMINOR,
     .              -1.D0,  SMAJOR,
     .               1.D0,  VTEMP,  BVEC(1,3)  )

      CALL VLCOM3 (  1.D0,  SMINOR,
     .              -1.D0,  SMAJOR,
     .               1.D0,  VTEMP,  BVEC(1,4)  )

C
C     Apply the transformation from the ALPHAFIXED frame to the
C     ALPHA_VIEW_XY frame. The latter rotates with the orbital motion
C     of planet Alpha.
C
      CALL MXV  ( XFORM, POS, VTEMP )
      CALL VHAT ( VTEMP,      BSITE )

      DO I = 1, NV
         CALL MXV ( XFORM, BVEC(1,I), BNDS(1,I) )

         CALL VHAT ( BNDS(1,I), VTEMP     )
         CALL VEQU ( VTEMP,     BNDS(1,I) )
      END DO


      TX(L)   = ' '
      L       = L+1
      TX(L)   = ' '
      L       = L+1
      TX(L)   = ' '
      L       = L+1
      TX(L)   = BEGDAT()
      L       = L+1
      TX(L)   = ' '
      L       = L+1

      TX(L)   = 'NAIF_BODY_NAME += ''#'' '
      CALL REPMC ( TX(L), '#', INSNAM, TX(L) )
      L       = L+1

      TX(L)   = 'NAIF_BODY_CODE += #'
      CALL REPMI ( TX(L), '#', INST,   TX(L) )
      L       = L+1

      TX(L)   = ' '
      L       = L+1
      IXINB   = L

      TX(L)   = 'INS#_FOV_SHAPE            = ''RECTANGLE'' '
      L       = L+1
      TX(L)   = 'INS#_FOV_FRAME            = ''ALPHA_VIEW_XY'' '
      L       = L+1
      IXBSB   = L
      TX(L)   = 'INS#_BORESIGHT            = ( *'
      L       = L+1
      TX(L)   = '                                     *'
      L       = L+1
      TX(L)   = '                                     * )'
      L       = L+1
      IXBCB   = L
      TX(L)   = 'INS#_FOV_BOUNDARY         = ( *'
      IXINE   = L
      L       = L+1
      TX(L)   = '                                     *'
      L       = L+1
      TX(L)   = '                                     *'
      L       = L+1
      TX(L)   = '                                     *'
      L       = L+1
      TX(L)   = '                                     *'
      L       = L+1
      TX(L)   = '                                     *'
      L       = L+1
      TX(L)   = '                                     *'
      L       = L+1
      TX(L)   = '                                     *'
      L       = L+1
      TX(L)   = '                                     *'
      L       = L+1
      TX(L)   = '                                     *'
      L       = L+1
      TX(L)   = '                                     *'
      L       = L+1
      TX(L)   = '                                     * )'
      L       = L+1
      TX(L)   = ' '
      L       = L+1
      TX(L)   = BEGTXT()
      L       = L+1

C
C     Set the instrument ID in the keywords above.
C
      DO I = IXINB, IXINE
         CALL REPMI ( TX(I), '#', INST, TX(I) )
      END DO

C
C     Set the boresight values.
C
      J = IXBSB-1

      DO I = 1, 3
         CALL REPMD ( TX(J+I), '*', BSITE(I), 14, TX(J+I) )
      END DO

C
C     Set the boundary corner values.
C
      J = IXBCB-1

      DO I = 1, 3
         CALL REPMD ( TX(J+I  ), '*', BNDS(I,1), 14, TX(J+I  ) )
         CALL REPMD ( TX(J+I+3), '*', BNDS(I,2), 14, TX(J+I+3) )
         CALL REPMD ( TX(J+I+6), '*', BNDS(I,3), 14, TX(J+I+6) )
         CALL REPMD ( TX(J+I+9), '*', BNDS(I,4), 14, TX(J+I+9) )
      END DO


C**********************************************************************
C
C     Create FOV specification and instrument name-ID mapping
C     for the instrument "Alpha_Diamond_NONE."
C
C**********************************************************************

C
C     This FOV is fixed to the dynamic frame ALPHA_VIEW_XZ (note: NOT
C     "_XY"). Thus the instrument "tracks" body Alpha. As seen from the
C     Sun, with aberration corrections turned off, the FOV inscribes
C     the limb of Alpha.
C

      INSNAM  = 'ALPHA_DIAMOND_NONE'
      INST    = -1500004
      NV      = 4


C
C     Look up the transformation from the ALPHAFIXED frame to the
C     ALPHA_VIEW_XY frame. The latter rotates with the orbital motion
C     of planet Alpha.
C
      CALL PXFORM ( 'ALPHAFIXED', 'ALPHA_VIEW_XZ', ET0, XFORM )

C
C     Compute the boresight vector.
C
      CALL MXV  ( XFORM, POS, VTEMP )
      CALL VHAT ( VTEMP,      BSITE )


C
C     Compute the boundary vectors.
C
      CALL VSUB ( CENTER, VIEWPT, VTEMP )

      CALL VLCOM (  1.D0,  SMINOR,
     .              1.D0,  VTEMP,  BVEC(1,1)  )

      CALL VLCOM (  1.D0,  SMAJOR,
     .              1.D0,  VTEMP,  BVEC(1,2)  )

      CALL VLCOM ( -1.D0,  SMINOR,
     .              1.D0,  VTEMP,  BVEC(1,3)  )

      CALL VLCOM ( -1.D0,  SMAJOR,
     .              1.D0,  VTEMP,  BVEC(1,4)  )

C
C     Apply the transformation from the ALPHAFIXED frame to the
C     ALPHA_VIEW_XZ frame. The latter rotates with the orbital motion
C     of planet Alpha.
C
      CALL MXV  ( XFORM, POS, VTEMP )
      CALL VHAT ( VTEMP,      BSITE )

      DO I = 1, NV
         CALL MXV ( XFORM, BVEC(1,I), BNDS(1,I) )

         CALL VHAT ( BNDS(1,I), VTEMP     )
         CALL VEQU ( VTEMP,     BNDS(1,I) )
      END DO


      TX(L)   = ' '
      L       = L+1
      TX(L)   = ' '
      L       = L+1
      TX(L)   = ' '
      L       = L+1
      TX(L)   = BEGDAT()
      L       = L+1
      TX(L)   = ' '
      L       = L+1

      TX(L)   = 'NAIF_BODY_NAME += ''#'' '
      CALL REPMC ( TX(L), '#', INSNAM, TX(L) )
      L       = L+1

      TX(L)   = 'NAIF_BODY_CODE += #'
      CALL REPMI ( TX(L), '#', INST,   TX(L) )
      L       = L+1

      TX(L)   = ' '
      L       = L+1
      IXINB   = L

      TX(L)   = 'INS#_FOV_SHAPE            = ''POLYGON'' '
      L       = L+1
      TX(L)   = 'INS#_FOV_FRAME            = ''ALPHA_VIEW_XZ'' '
      L       = L+1
      IXBSB   = L
      TX(L)   = 'INS#_BORESIGHT            = ( *'
      L       = L+1
      TX(L)   = '                                     *'
      L       = L+1
      TX(L)   = '                                     * )'
      L       = L+1
      IXBCB   = L
      TX(L)   = 'INS#_FOV_BOUNDARY         = ( *'
      IXINE   = L
      L       = L+1
      TX(L)   = '                                     *'
      L       = L+1
      TX(L)   = '                                     *'
      L       = L+1
      TX(L)   = '                                     *'
      L       = L+1
      TX(L)   = '                                     *'
      L       = L+1
      TX(L)   = '                                     *'
      L       = L+1
      TX(L)   = '                                     *'
      L       = L+1
      TX(L)   = '                                     *'
      L       = L+1
      TX(L)   = '                                     *'
      L       = L+1
      TX(L)   = '                                     *'
      L       = L+1
      TX(L)   = '                                     *'
      L       = L+1
      TX(L)   = '                                     * )'
      L       = L+1
      TX(L)   = ' '
      L       = L+1
      TX(L)   = BEGTXT()
      L       = L+1

C
C     Set the instrument ID in the keywords above.
C
      DO I = IXINB, IXINE
         CALL REPMI ( TX(I), '#', INST, TX(I) )
      END DO

C
C     Set the boresight values.
C
      J = IXBSB-1

      DO I = 1, 3
         CALL REPMD ( TX(J+I), '*', BSITE(I), 14, TX(J+I) )
      END DO

C
C     Set the boundary corner values.
C
      J = IXBCB-1

      DO I = 1, 3
         CALL REPMD ( TX(J+I  ), '*', BNDS(I,1), 14, TX(J+I  ) )
         CALL REPMD ( TX(J+I+3), '*', BNDS(I,2), 14, TX(J+I+3) )
         CALL REPMD ( TX(J+I+6), '*', BNDS(I,3), 14, TX(J+I+6) )
         CALL REPMD ( TX(J+I+9), '*', BNDS(I,4), 14, TX(J+I+9) )
      END DO



C**********************************************************************
C
C     Create the IK.
C
C**********************************************************************

C
C     L is incremented after each assignment; decrement it here.
C
      NL = L-1

      CALL TXTOPN ( IK, UNIT )

      DO I = 1, NL
         CALL WRITLN ( TX(I), UNIT )
      END DO

      CLOSE ( UNIT )

C
C     If this file needs to be loaded, do it.
C
      IF ( LOADIK ) THEN
         CALL LDPOOL ( IK )
      END IF

      IF ( KEEPIK ) THEN
C
C        If we are keeping this file, we need to register it
C        with FILREG.
C
         CALL TFILES ( IK )

      ELSE
C
C        Lose the file.
C
         CALL KILFIL ( IK )

      END IF

      IF ( .NOT. LOADIK ) THEN
C
C        Unload the SPK.
C
         CALL SPKUEF ( HANDLE )

      END IF

      CALL CHKOUT ( 'NATIK' )
      RETURN
      END
