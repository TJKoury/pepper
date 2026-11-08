!$Procedure ZZNATIK (Create an SCLK file)

      SUBROUTINE ZZNATIK( NAMEIK, SPK, PCK )

! $ Abstract
!
!     Create a test IK text kernel named NAMEIK. This IK
!     will correspond only to the Nat's Solar System SPK
!     and PCK
!
! $ Required_Reading
!
!   None.
!
! $ Keywords
!
!   None.
!
! $ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         NAMEIK
      CHARACTER*(*)         SPK
      CHARACTER*(*)         PCK

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!
! $ Detailed_Input
!
!   None.
!
! $ Detailed_Output
!
!   None.
!
! $ Parameters
!
!   None.
!
! $ Exceptions
!
!   None.
!
! $ Files
!
!   None.
!
! $ Particulars
!
!   None.
!
! $ Examples
!
!   None.
!
! $ Restrictions
!
!   None.
!
! $ Literature_References
!
!   None.
!
! $ Author_and_Institution
!
!   None.
!
! $ Version
!
!-    Test Utilities 1.0.0, 18-AUG-2008 (EDW)
!
!        Reimplementation of NATIK removing all code
!        except that needed to write the test IK.
!-&

! $ Index_Entries
!
!   None.
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
      CHARACTER*(LNSIZE)    IK     ( NLINES )
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


      CALL CHKIN ( 'ZZNATIK' )

!
!     Delete any existing file of the same name.
!
      CALL KILFIL ( NAMEIK )

      CALL FURNSH( PCK )
      CALL FURNSH( SPK )

!
!     Before creating the IK, we derive the parameters
!     needed to create the intrument FOVs we'll specify there.
!
!     Load the Nat's solar system SPK and PCK.
!
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
      IK(L)   = ' '
      L       = L+1
      IK(L)   = ' KPL/IK'
      L       = L+1
      IK(L)   = ' '
      L       = L+1
      IK(L)   = BEGDAT()
      L       = L+1
      IK(L)   = ' '
      L       = L+1

      IK(L)   = 'NAIF_BODY_NAME += ''#'' '
      CALL REPMC ( IK(L), '#', INSNAM, IK(L) )
      L       = L+1

      IK(L)   = 'NAIF_BODY_CODE += #'
      CALL REPMI ( IK(L), '#', INST,   IK(L) )
      L       = L+1

      IK(L)   = ' '
      L       = L+1

      IXINB   = L
      IK(L)   = 'INS#_FOV_SHAPE            = ''ELLIPSE'' '
      L       = L+1
      IK(L)  = 'INS#_FOV_FRAME            = ''ALPHA_VIEW_XY'' '
      L       = L+1
      IXBSB   = L
      IK(L)  = 'INS#_BORESIGHT            = ( *'
      L       = L+1
      IK(L)   = '                                     *'
      L       = L+1
      IK(L)   = '                                     * )'
      L       = L+1
      IXBCB   = L
      IK(L)   = 'INS#_FOV_BOUNDARY         = ( *'
      IXINE   = L
      L       = L+1
      IK(L)   = '                                     *'
      L       = L+1
      IK(L)   = '                                     *'
      L       = L+1
      IK(L)   = '                                     *'
      L       = L+1
      IK(L)   = '                                     *'
      L       = L+1
      IK(L)   = '                                     * )'
      L       = L+1
      IK(L)   = ' '
      L       = L+1
      IK(L)   = BEGTXT()
      L       = L+1

!
!     Set the instrument ID in the keywords above.
!
      DO I = IXINB, IXINE
         CALL REPMI ( IK(I), '#', INST, IK(I) )
      END DO

!
!     Set the boresight values.
!
      J = IXBSB-1

      DO I = 1, 3
         CALL REPMD ( IK(J+I), '*', BSITE(I), 14, IK(J+I) )
      END DO

!
!     Set the boundary corner values.
!
      J = IXBCB-1

      DO I = 1, 3
         CALL REPMD ( IK(J+I  ), '*', BNDS(I,1), 14, IK(J+I  ) )
         CALL REPMD ( IK(J+I+3), '*', BNDS(I,2), 14, IK(J+I+3) )
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

      IK(L)   = ' '
      L       = L+1
      IK(L)   = ' KPL/IK'
      L       = L+1
      IK(L)   = ' '
      L       = L+1
      IK(L)   = BEGDAT()
      L       = L+1
      IK(L)   = ' '
      L       = L+1

      IK(L)   = 'NAIF_BODY_NAME += ''#'' '
      CALL REPMC ( IK(L), '#', INSNAM, IK(L) )
      L       = L+1

      IK(L)   = 'NAIF_BODY_CODE += #'
      CALL REPMI ( IK(L), '#', INST,   IK(L) )
      L       = L+1

      IK(L)   = ' '
      L       = L+1

      IXINB   = L
      IK(L)   = 'INS#_FOV_SHAPE            = ''CIRCLE'' '
      L       = L+1
      IK(L)  = 'INS#_FOV_FRAME            = ''ALPHA_VIEW_XY'' '
      L       = L+1
      IXBSB   = L
      IK(L)  = 'INS#_BORESIGHT            = ( *'
      L       = L+1
      IK(L)   = '                                     *'
      L       = L+1
      IK(L)   = '                                     * )'
      L       = L+1
      IXBCB   = L
      IK(L)   = 'INS#_FOV_BOUNDARY         = ( *'
      IXINE   = L
      L       = L+1
      IK(L)   = '                                     *'
      L       = L+1
      IK(L)   = '                                     * )'
      L       = L+1
      IK(L)   = ' '
      L       = L+1
      IK(L)   = BEGTXT()
      L       = L+1

!
!     Set the instrument ID in the keywords above.
!
      DO I = IXINB, IXINE
         CALL REPMI ( IK(I), '#', INST, IK(I) )
      END DO

!
!     Set the boresight values.
!
      J = IXBSB-1

      DO I = 1, 3
         CALL REPMD ( IK(J+I), '*', BSITE(I), 14, IK(J+I) )
      END DO

!
!     Set the boundary corner values.
!
      J = IXBCB-1

      DO I = 1, 3
         CALL REPMD ( IK(J+I  ), '*', BNDS(I,1), 14, IK(J+I  ) )
         CALL REPMD ( IK(J+I+3), '*', BNDS(I,2), 14, IK(J+I+3) )
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


      IK(L)   = ' '
      L       = L+1
      IK(L)   = ' '
      L       = L+1
      IK(L)   = ' '
      L       = L+1
      IK(L)   = BEGDAT()
      L       = L+1
      IK(L)   = ' '
      L       = L+1

      IK(L)   = 'NAIF_BODY_NAME += ''#'' '
      CALL REPMC ( IK(L), '#', INSNAM, IK(L) )
      L       = L+1

      IK(L)   = 'NAIF_BODY_CODE += #'
      CALL REPMI ( IK(L), '#', INST,   IK(L) )
      L       = L+1

      IK(L)   = ' '
      L       = L+1
      IXINB   = L

      IK(L)   = 'INS#_FOV_SHAPE            = ''RECTANGLE'' '
      L       = L+1
      IK(L)   = 'INS#_FOV_FRAME            = ''ALPHA_VIEW_XY'' '
      L       = L+1
      IXBSB   = L
      IK(L)   = 'INS#_BORESIGHT            = ( *'
      L       = L+1
      IK(L)   = '                                     *'
      L       = L+1
      IK(L)   = '                                     * )'
      L       = L+1
      IXBCB   = L
      IK(L)   = 'INS#_FOV_BOUNDARY         = ( *'
      IXINE   = L
      L       = L+1
      IK(L)   = '                                     *'
      L       = L+1
      IK(L)   = '                                     *'
      L       = L+1
      IK(L)   = '                                     *'
      L       = L+1
      IK(L)   = '                                     *'
      L       = L+1
      IK(L)   = '                                     *'
      L       = L+1
      IK(L)   = '                                     *'
      L       = L+1
      IK(L)   = '                                     *'
      L       = L+1
      IK(L)   = '                                     *'
      L       = L+1
      IK(L)   = '                                     *'
      L       = L+1
      IK(L)   = '                                     *'
      L       = L+1
      IK(L)   = '                                     * )'
      L       = L+1
      IK(L)   = ' '
      L       = L+1
      IK(L)   = BEGTXT()
      L       = L+1

!
!     Set the instrument ID in the keywords above.
!
      DO I = IXINB, IXINE
         CALL REPMI ( IK(I), '#', INST, IK(I) )
      END DO

!
!     Set the boresight values.
!
      J = IXBSB-1

      DO I = 1, 3
         CALL REPMD ( IK(J+I), '*', BSITE(I), 14, IK(J+I) )
      END DO

!
!     Set the boundary corner values.
!
      J = IXBCB-1

      DO I = 1, 3
         CALL REPMD ( IK(J+I  ), '*', BNDS(I,1), 14, IK(J+I  ) )
         CALL REPMD ( IK(J+I+3), '*', BNDS(I,2), 14, IK(J+I+3) )
         CALL REPMD ( IK(J+I+6), '*', BNDS(I,3), 14, IK(J+I+6) )
         CALL REPMD ( IK(J+I+9), '*', BNDS(I,4), 14, IK(J+I+9) )
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


      IK(L)   = ' '
      L       = L+1
      IK(L)   = ' '
      L       = L+1
      IK(L)   = ' '
      L       = L+1
      IK(L)   = BEGDAT()
      L       = L+1
      IK(L)   = ' '
      L       = L+1

      IK(L)   = 'NAIF_BODY_NAME += ''#'' '
      CALL REPMC ( IK(L), '#', INSNAM, IK(L) )
      L       = L+1

      IK(L)   = 'NAIF_BODY_CODE += #'
      CALL REPMI ( IK(L), '#', INST,   IK(L) )
      L       = L+1

      IK(L)   = ' '
      L       = L+1
      IXINB   = L

      IK(L)   = 'INS#_FOV_SHAPE            = ''POLYGON'' '
      L       = L+1
      IK(L)   = 'INS#_FOV_FRAME            = ''ALPHA_VIEW_XZ'' '
      L       = L+1
      IXBSB   = L
      IK(L)   = 'INS#_BORESIGHT            = ( *'
      L       = L+1
      IK(L)   = '                                     *'
      L       = L+1
      IK(L)   = '                                     * )'
      L       = L+1
      IXBCB   = L
      IK(L)   = 'INS#_FOV_BOUNDARY         = ( *'
      IXINE   = L
      L       = L+1
      IK(L)   = '                                     *'
      L       = L+1
      IK(L)   = '                                     *'
      L       = L+1
      IK(L)   = '                                     *'
      L       = L+1
      IK(L)   = '                                     *'
      L       = L+1
      IK(L)   = '                                     *'
      L       = L+1
      IK(L)   = '                                     *'
      L       = L+1
      IK(L)   = '                                     *'
      L       = L+1
      IK(L)   = '                                     *'
      L       = L+1
      IK(L)   = '                                     *'
      L       = L+1
      IK(L)   = '                                     *'
      L       = L+1
      IK(L)   = '                                     * )'
      L       = L+1
      IK(L)   = ' '
      L       = L+1
      IK(L)   = BEGTXT()
      L       = L+1

!
!     Set the instrument ID in the keywords above.
!
      DO I = IXINB, IXINE
         CALL REPMI ( IK(I), '#', INST, IK(I) )
      END DO

!
!     Set the boresight values.
!
      J = IXBSB-1

      DO I = 1, 3
         CALL REPMD ( IK(J+I), '*', BSITE(I), 14, IK(J+I) )
      END DO

!
!     Set the boundary corner values.
!
      J = IXBCB-1

      DO I = 1, 3
         CALL REPMD ( IK(J+I  ), '*', BNDS(I,1), 14, IK(J+I  ) )
         CALL REPMD ( IK(J+I+3), '*', BNDS(I,2), 14, IK(J+I+3) )
         CALL REPMD ( IK(J+I+6), '*', BNDS(I,3), 14, IK(J+I+6) )
         CALL REPMD ( IK(J+I+9), '*', BNDS(I,4), 14, IK(J+I+9) )
      END DO



!**********************************************************************
!
!     Create the IK.
!
!**********************************************************************

      CALL UNLOAD( PCK )
      CALL UNLOAD( SPK )

!
!     L is incremented after each assignment; decrement it here.
!
      NL = L-1

      CALL TXTOPN ( NAMEIK, UNIT )

      DO I = 1, NL
         CALL WRITLN ( IK(I), UNIT )
      END DO

      CLOSE ( UNIT )

      CALL CHKOUT ( 'ZZNATIK' )

      RETURN
      END
