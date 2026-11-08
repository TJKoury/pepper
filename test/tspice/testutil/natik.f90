!$Procedure   NATIK ( Create an IK for Nat's solar system )

      SUBROUTINE NATIK ( IK, SPK, PCK, LOADIK, KEEPIK )

! $ Abstract
!
!     Create and if appropriate load an IK for Nat's
!     solar system.
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
!     TESTING
!
! $ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         IK
      CHARACTER*(*)         SPK
      CHARACTER*(*)         PCK
      LOGICAL               LOADIK
      LOGICAL               KEEPIK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     IK         I   Name of the I-kernel to create.
!     SPK        I   Name of an existing Nat's solar system SPK.
!     PCK        I   Name of an existing Nat's solar system PCK.
!     LOADIK     I   Load the I-kernel if .TRUE.
!     KEEPIK     I   Keep the I-kernel if .TRUE., else delete it.
!
! $ Detailed_Input
!
!     IK          is the name of an IK to create and load if LOADIK is
!                 set to .TRUE. If an IK of the same name already
!                 exists, the existing file is deleted.
!
!     SPK         is the name of an existing Nat's solar system SPK.
!                 This SPK is required to define dynamic two-vector
!                 frames which require the positions of bodies Alpha
!                 and Beta with respect to the Sun.
!
!     PCK         is the name of an existing Nat's solar system PCK.
!                 This PCK is required to define dynamic two-vector
!                 frames which require the positions of bodies Alpha
!                 and Beta with respect to the Sun.
!
!     LOADIK      is a logical that indicates whether or not the IK
!                 file should be loaded after it is created.  If LOADIK
!                 has the value .TRUE. the IK is loaded after it is
!                 created.  Otherwise it is left un-opened.
!
!     KEEPIK      is a logical that indicates whether or not the IK
!                 file should be deleted after it is loaded.  If KEEPIK
!                 is .TRUE. the file is not deleted.  If KEEPIK is
!                 .FALSE. the file is deleted after it is loaded.
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
!     1) Any I/O errors occurring in the creation of the IK file
!        will be signaled by routines in the call tree of this
!        routine.
!
!     Since this routine is normally used within the TSPICE system,
!     it's up to the caller to call CHCKXC to catch errors signaled by
!     this routine.
!
! $ Files
!
!     This routine creates an IK containing FOV specifications for four
!     "instruments." The instruments' frame and FOV specifications are
!     designed so that appearances of body Beta in the FOVs coincide
!     with transits of Beta across Alpha, thus yielding simple,
!     predictable
!     event timing.
!
!     The instruments' names are
!
!        ALPHA_CIRCLE_NONE
!        ALPHA_DIAMOND_NONE
!        ALPHA_ELLIPSE_NONE
!        ALPHA_RECTANGLE_NONE
!
!     The first component of each name is the body tracked by the
!     instrument's reference frame.
!
!     The second component is the FOV shape; each shape specifier
!     supported by GETFOV is supported here. The instrument
!     ALPHA_DIAMOND_NONE uses the 'POLYGON' shape specifier.
!
!     The third component of each name is the aberration correction
!     applied to the position vectors used in the frame specification.
!
!     The reference frames associated with the instruments are,
!     respectively
!
!        ALPHA_VIEW_XY
!        ALPHA_VIEW_XZ
!        ALPHA_VIEW_XY
!        ALPHA_VIEW_XY
!
!     Note that the frame of ALPHA_DIAMOND_NONE has been selected
!     so that body Beta orbits in the X-Z plane of that frame.
!
! $ Particulars
!
!     This routine creates an IK file for use in testing. The
!     file is registered with the TSPICE system via a call to
!     TFILES.
!
! $ Examples
!
!     The normal way to use this routine is shown below.
!
!     CALL NATSPK ( 'nat.bsp', .TRUE., HANDLE  )
!     CALL NATPCK ( 'nat.tpc', .TRUE., .TRUE.  )
!     CALL NATIK  ( 'nat.ti',  .TRUE., .FALSE. )
!
!        [perform some tests and computations]
!
!     CALL SPKUEF ( HANDLE )
!     CALL KILFIL ( 'nat.bsp' )
!
!     If these calls are made within a TSPICE test family, the
!     kernels will be deleted at the time the test family returns.
!     In other settings, the caller will need to clean up the
!     PCK file via an explicit deletion.
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
!     N.J. Bachman    (JPL)
!
! $ Version
!
!-    Test Utilities Version 2.0.0, 08-OCT-2021 (NJB)
!
!        Changed boresight length of instrument ALPHA_ELLIPSE_NONE
!        to 2. This suffices to demonstrate an error in the N0066
!        version of ZZGFFVIN. Corrected several comment typos.
!
!        Bug fix: removed unnecessary REPMD call used to create
!        a portion of the IK setting a boundary vector for instrument
!        ALPHA_CIRCLE_NONE. This call had no effect but was correctly
!        identified by Valgrind.
!
!-    Test Utilities 1.0.0, 29-JUL-2008 (NJB)
!
!-&

! $ Index_Entries
!
!     Create a "Nat's solar system" IK file
!
!-&





!
!     Test Utility Functions
!
      CHARACTER*(16)        BEGDAT
      CHARACTER*(16)        BEGTXT

!
!     Local Parameters
!
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

!
!     Local variables
!
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

!
!     Delete any existing file of the same name.
!
      CALL KILFIL ( IK )

!
!     Before creating the IK, we derive the parameters
!     needed to create the instrument FOVs we'll specify there.
!
!     Load the Nat's solar system SPK and PCK.
!
      CALL SPKLEF ( SPK, HANDLE )
      CALL LDPOOL ( PCK )

      CALL BODVRD ( 'ALPHA', 'RADII', 3, N, RADII )

!
!     Compute the limb of body Alpha as seen from the Sun
!     at 0 seconds past J2000 TDB. Do not use aberration
!     corrections.
!
      ET0 = 0.D0

      CALL SPKPOS ( 'ALPHA', ET0, 'ALPHAFIXED', 'NONE', 'SUN', POS, LT )

      CALL VMINUS ( POS, VIEWPT )

      CALL EDLIMB ( RADII(1), RADII(2), RADII(3), VIEWPT, LIMB )

!
!     Create boundary vectors for an elliptical FOV in the
!     ALPHAFIXED frame.
!
      CALL EL2CGV ( LIMB, CENTER, SMAJOR, SMINOR )





!**********************************************************************
!
!     Create FOV specification and instrument name-ID mapping
!     for the instrument "Alpha_Ellipse_NONE."
!
!**********************************************************************

!
!     This FOV is fixed to the dynamic frame ALPHA_VIEW_XY.
!     Thus the instrument "tracks" body Alpha. As seen from the
!     Sun, with aberration corrections turned off,
!     the FOV coincides with the limb of Alpha.
!

      INSNAM  = 'ALPHA_ELLIPSE_NONE'
      INST    = -1500000

!
!     Look up the transformation from the ALPHAFIXED frame to the
!     ALPHA_VIEW_XY frame. The latter rotates with the orbital motion
!     of planet Alpha.
!
      CALL PXFORM ( 'ALPHAFIXED', 'ALPHA_VIEW_XY', ET0, XFORM )

!
!     Compute the boresight vector.
!
      CALL MXV  ( XFORM, POS, VTEMP )
      CALL VHAT ( VTEMP,      BSITE )

!
!     New!
!
      CALL VSCLIP( 2.0D0, BSITE )


      CALL VLCOM3 ( 1.D0,  CENTER, &
                    1.D0,  SMAJOR, &
                   -1.D0,  VIEWPT,  BVEC(1,1)  )


      CALL VLCOM3 ( 1.D0,  CENTER, &
                    1.D0,  SMINOR, &
                   -1.D0,  VIEWPT,  BVEC(1,2)  )


      DO I = 1, 2
         CALL MXV ( XFORM, BVEC(1,I), BNDS(1,I) )
      END DO

!
!     As obnoxious as the practice may seem, we'll use the variable
!     L instead of hard-coded constants to index buffer elements.
!     Hard-coded indices are a big problem when changes are needed.
!
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

!
!     Set the instrument ID in the keywords above.
!
      DO I = IXINB, IXINE
         CALL REPMI ( TX(I), '#', INST, TX(I) )
      END DO

!
!     Set the boresight values.
!
      J = IXBSB-1

      DO I = 1, 3
         CALL REPMD ( TX(J+I), '*', BSITE(I), 14, TX(J+I) )
      END DO

!
!     Set the boundary corner values.
!
      J = IXBCB-1

      DO I = 1, 3
         CALL REPMD ( TX(J+I  ), '*', BNDS(I,1), 14, TX(J+I  ) )
         CALL REPMD ( TX(J+I+3), '*', BNDS(I,2), 14, TX(J+I+3) )
      END DO




!**********************************************************************
!
!     Create FOV specification and instrument name-ID mapping
!     for the instrument "Alpha_Circle_NONE."
!
!**********************************************************************

!
!     This FOV is fixed to the dynamic frame ALPHA_VIEW_XY.
!     Thus the instrument "tracks" body Alpha. As seen from the
!     Sun, with aberration corrections turned off,
!     the FOV is inscribed in the limb of Alpha.
!

      INSNAM  = 'ALPHA_CIRCLE_NONE'
      INST    = -1500001

!
!     Look up the transformation from the ALPHAFIXED frame to the
!     ALPHA_VIEW_XY frame. The latter rotates with the orbital motion
!     of planet Alpha.
!
      CALL PXFORM ( 'ALPHAFIXED', 'ALPHA_VIEW_XY', ET0, XFORM )

!
!     Compute the boresight vector.
!
      CALL MXV  ( XFORM, POS, VTEMP )
      CALL VHAT ( VTEMP,      BSITE )


!
!     We use the semi-minor axis to define the boundary vector,
!     since we want our circle to be inscribed in Alpha's limb.
!
      CALL VLCOM3 ( 1.D0,  CENTER, &
                    1.D0,  SMINOR, &
                   -1.D0,  VIEWPT,  BVEC(1,1)  )


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

!
!     Set the instrument ID in the keywords above.
!
      DO I = IXINB, IXINE
         CALL REPMI ( TX(I), '#', INST, TX(I) )
      END DO

!
!     Set the boresight values.
!
      J = IXBSB-1

      DO I = 1, 3
         CALL REPMD ( TX(J+I), '*', BSITE(I), 14, TX(J+I) )
      END DO

!
!     Set the boundary corner values.
!
      J = IXBCB-1

      DO I = 1, 3
         CALL REPMD ( TX(J+I  ), '*', BNDS(I,1), 14, TX(J+I  ) )
      END DO



!**********************************************************************
!
!     Create FOV specification and instrument name-ID mapping
!     for the instrument "Alpha_Rectangle_NONE."
!
!**********************************************************************

!
!     This FOV is fixed to the dynamic frame ALPHA_VIEW_XY. Thus the
!     instrument "tracks" body Alpha. As seen from the Sun, with
!     aberration corrections turned off, the FOV circumscribes the limb
!     of Alpha.
!

      INSNAM  = 'ALPHA_RECTANGLE_NONE'
      INST    = -1500003
      NV      = 4


!
!     Look up the transformation from the ALPHAFIXED frame to the
!     ALPHA_VIEW_XY frame. The latter rotates with the orbital motion
!     of planet Alpha.
!
      CALL PXFORM ( 'ALPHAFIXED', 'ALPHA_VIEW_XY', ET0, XFORM )

!
!     Compute the boresight vector.
!
      CALL MXV  ( XFORM, POS, VTEMP )
      CALL VHAT ( VTEMP,      BSITE )


!
!     Compute the boundary vectors.
!
      CALL VSUB ( CENTER, VIEWPT, VTEMP )

      CALL VLCOM3 (  1.D0,  SMINOR, &
                     1.D0,  SMAJOR, &
                     1.D0,  VTEMP,  BVEC(1,1)  )

      CALL VLCOM3 ( -1.D0,  SMINOR, &
                     1.D0,  SMAJOR, &
                     1.D0,  VTEMP,  BVEC(1,2)  )

      CALL VLCOM3 ( -1.D0,  SMINOR, &
                    -1.D0,  SMAJOR, &
                     1.D0,  VTEMP,  BVEC(1,3)  )

      CALL VLCOM3 (  1.D0,  SMINOR, &
                    -1.D0,  SMAJOR, &
                     1.D0,  VTEMP,  BVEC(1,4)  )

!
!     Apply the transformation from the ALPHAFIXED frame to the
!     ALPHA_VIEW_XY frame. The latter rotates with the orbital motion
!     of planet Alpha.
!
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

!
!     Set the instrument ID in the keywords above.
!
      DO I = IXINB, IXINE
         CALL REPMI ( TX(I), '#', INST, TX(I) )
      END DO

!
!     Set the boresight values.
!
      J = IXBSB-1

      DO I = 1, 3
         CALL REPMD ( TX(J+I), '*', BSITE(I), 14, TX(J+I) )
      END DO

!
!     Set the boundary corner values.
!
      J = IXBCB-1

      DO I = 1, 3
         CALL REPMD ( TX(J+I  ), '*', BNDS(I,1), 14, TX(J+I  ) )
         CALL REPMD ( TX(J+I+3), '*', BNDS(I,2), 14, TX(J+I+3) )
         CALL REPMD ( TX(J+I+6), '*', BNDS(I,3), 14, TX(J+I+6) )
         CALL REPMD ( TX(J+I+9), '*', BNDS(I,4), 14, TX(J+I+9) )
      END DO


!**********************************************************************
!
!     Create FOV specification and instrument name-ID mapping
!     for the instrument "Alpha_Diamond_NONE."
!
!**********************************************************************

!
!     This FOV is fixed to the dynamic frame ALPHA_VIEW_XZ (note: NOT
!     "_XY"). Thus the instrument "tracks" body Alpha. As seen from the
!     Sun, with aberration corrections turned off, the FOV inscribes
!     the limb of Alpha.
!

      INSNAM  = 'ALPHA_DIAMOND_NONE'
      INST    = -1500004
      NV      = 4


!
!     Look up the transformation from the ALPHAFIXED frame to the
!     ALPHA_VIEW_XY frame. The latter rotates with the orbital motion
!     of planet Alpha.
!
      CALL PXFORM ( 'ALPHAFIXED', 'ALPHA_VIEW_XZ', ET0, XFORM )

!
!     Compute the boresight vector.
!
      CALL MXV  ( XFORM, POS, VTEMP )
      CALL VHAT ( VTEMP,      BSITE )


!
!     Compute the boundary vectors.
!
      CALL VSUB ( CENTER, VIEWPT, VTEMP )

      CALL VLCOM (  1.D0,  SMINOR, &
                    1.D0,  VTEMP,  BVEC(1,1)  )

      CALL VLCOM (  1.D0,  SMAJOR, &
                    1.D0,  VTEMP,  BVEC(1,2)  )

      CALL VLCOM ( -1.D0,  SMINOR, &
                    1.D0,  VTEMP,  BVEC(1,3)  )

      CALL VLCOM ( -1.D0,  SMAJOR, &
                    1.D0,  VTEMP,  BVEC(1,4)  )

!
!     Apply the transformation from the ALPHAFIXED frame to the
!     ALPHA_VIEW_XZ frame. The latter rotates with the orbital motion
!     of planet Alpha.
!
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

!
!     Set the instrument ID in the keywords above.
!
      DO I = IXINB, IXINE
         CALL REPMI ( TX(I), '#', INST, TX(I) )
      END DO

!
!     Set the boresight values.
!
      J = IXBSB-1

      DO I = 1, 3
         CALL REPMD ( TX(J+I), '*', BSITE(I), 14, TX(J+I) )
      END DO

!
!     Set the boundary corner values.
!
      J = IXBCB-1

      DO I = 1, 3
         CALL REPMD ( TX(J+I  ), '*', BNDS(I,1), 14, TX(J+I  ) )
         CALL REPMD ( TX(J+I+3), '*', BNDS(I,2), 14, TX(J+I+3) )
         CALL REPMD ( TX(J+I+6), '*', BNDS(I,3), 14, TX(J+I+6) )
         CALL REPMD ( TX(J+I+9), '*', BNDS(I,4), 14, TX(J+I+9) )
      END DO



!**********************************************************************
!
!     Create the IK.
!
!**********************************************************************

!
!     L is incremented after each assignment; decrement it here.
!
      NL = L-1

      CALL TXTOPN ( IK, UNIT )

      DO I = 1, NL
         CALL WRITLN ( TX(I), UNIT )
      END DO

      CLOSE ( UNIT )

!
!     If this file needs to be loaded, do it.
!
      IF ( LOADIK ) THEN
         CALL LDPOOL ( IK )
      END IF

      IF ( KEEPIK ) THEN
!
!        If we are keeping this file, we need to register it
!        with FILREG.
!
         CALL TFILES ( IK )

      ELSE
!
!        Lose the file.
!
         CALL KILFIL ( IK )

      END IF

      IF ( .NOT. LOADIK ) THEN
!
!        Unload the SPK.
!
         CALL SPKUEF ( HANDLE )

      END IF

      CALL CHKOUT ( 'NATIK' )
      RETURN
      END
