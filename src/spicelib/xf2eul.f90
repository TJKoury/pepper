!$Procedure XF2EUL ( State transformation to Euler angles )

      SUBROUTINE XF2EUL ( XFORM, AXISA, AXISB, AXISC, EULANG, UNIQUE )

! $ Abstract
!
!     Convert a state transformation matrix to Euler angles and their
!     derivatives, given a specified set of axes.
!
! $ Required_Reading
!
!     PCK
!     ROTATION
!
! $ Keywords
!
!     ANGLES
!     DERIVATIVES
!     STATE
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      XFORM  ( 6, 6 )
      INTEGER               AXISA
      INTEGER               AXISB
      INTEGER               AXISC
      DOUBLE PRECISION      EULANG ( 6 )
      LOGICAL               UNIQUE

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     XFORM      I   A state transformation matrix.
!     AXISA      I   Axis A of the Euler angle factorization.
!     AXISB      I   Axis B of the Euler angle factorization.
!     AXISC      I   Axis C of the Euler angle factorization.
!     EULANG     O   An array of Euler angles and their derivatives.
!     UNIQUE     O   Indicates if EULANG is a unique representation.
!
! $ Detailed_Input
!
!     XFORM    is a state transformation matrix from some frame FRAME1
!              to another frame FRAME2. Pictorially, XFORM has the
!              structure shown here.
!
!                 .-             -.
!                 |       |       |
!                 |   R   |   0   |
!                 |       |       |
!                 |-------+-------|
!                 |       |       |
!                 | dR/dt |   R   |
!                 |       |       |
!                 `-             -'
!
!              where R is a rotation matrix that varies with respect to
!              time and dR/dt is its time derivative.
!
!              More specifically, if S1 is the state of some object
!              in FRAME1, then S2, the state of the same object
!              relative to FRAME2 is given by
!
!                 S2 = XFORM * S1
!
!              where "*" denotes the matrix vector product.
!
!     AXISA,
!     AXISB,
!     AXISC    are the axes desired for the factorization of R.
!
!              All must be in the range from 1 to 3. Moreover
!              it must be the case that AXISA and AXISB are distinct
!              and that AXISB and AXISC are distinct.
!
!              Every rotation matrix can be represented as a product
!              of three rotation matrices about the principal axes
!              of a reference frame.
!
!                 R =  [ ALPHA ]      [ BETA ]      [ GAMMA ]
!                               AXISA         AXISB          AXISC
!
!              The value 1 corresponds to the X axis.
!              The value 2 corresponds to the Y axis.
!              The value 3 corresponds to the Z axis.
!
! $ Detailed_Output
!
!     EULANG   is the set of Euler angles corresponding to the
!              specified factorization.
!
!              If we represent R as shown here:
!
!                 R =  [ ALPHA ]      [ BETA ]      [ GAMMA ]
!                               AXISA         AXISB          AXISC
!
!              then
!
!                 EULANG(1) = ALPHA
!                 EULANG(2) = BETA
!                 EULANG(3) = GAMMA
!                 EULANG(4) = dALPHA/dt
!                 EULANG(5) = dBETA/dt
!                 EULANG(6) = dGAMMA/dt
!
!              The range of ALPHA and GAMMA is (-pi, pi].
!
!              The range of BETA depends on the exact set of
!              axes used for the factorization. For
!              factorizations in which the first and third axes
!              are the same, the range of BETA is [0, pi].
!
!              For factorizations in which the first and third
!              axes are different, the range of BETA is
!              [-pi/2, pi/2].
!
!              For rotations such that ALPHA and GAMMA are not
!              uniquely determined, ALPHA and dALPHA/dt will
!              always be set to zero; GAMMA and dGAMMA/dt are
!              then uniquely determined.
!
!     UNIQUE   is a logical that indicates whether or not the
!              values in EULANG are uniquely determined. If
!              the values are unique then UNIQUE will be set to
!              .TRUE. If the values are not unique and some
!              components ( EULANG(1) and EULANG(4) ) have been set
!              to zero, then UNIQUE will have the value .FALSE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If any of AXISA, AXISB, or AXISC do not have values in
!
!            { 1, 2, 3 }
!
!         an error is signaled by a routine in the call tree of this
!         routine.
!
!     2)  If AXISB is equal to AXISC or AXISA, an error is signaled by a
!         routine in the call tree of this routine. An arbitrary
!         rotation matrix cannot be expressed using a sequence of Euler
!         angles unless the second rotation axis differs from the other
!         two.
!
!     3)  If the input matrix XFORM is not a rotation matrix, an error
!         is signaled by a routine in the call tree of this routine.
!
!     4)  If EULANG(1) and EULANG(3) are not uniquely determined,
!         EULANG(1) is set to zero, and EULANG(3) is determined.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     A word about notation: the symbol
!
!        [ x ]
!             i
!
!     indicates a coordinate system rotation of x radians about the
!     ith coordinate axis. To be specific, the symbol
!
!        [ x ]
!             1
!
!     indicates a coordinate system rotation of x radians about the
!     first, or x-, axis; the corresponding matrix is
!
!        .-                    -.
!        |  1      0       0    |
!        |                      |
!        |  0    cos(x)  sin(x) |
!        |                      |
!        |  0   -sin(x)  cos(x) |
!        `-                    -'
!
!     Remember, this is a COORDINATE SYSTEM rotation by x radians; this
!     matrix, when applied to a vector, rotates the vector by -x
!     radians, not x radians. Applying the matrix to a vector yields
!     the vector's representation relative to the rotated coordinate
!     system.
!
!     The analogous rotation about the second, or y-, axis is
!     represented by
!
!        [ x ]
!             2
!
!     which symbolizes the matrix
!
!        .-                    -.
!        | cos(x)   0   -sin(x) |
!        |                      |
!        |  0       1      0    |
!        |                      |
!        | sin(x)   0    cos(x) |
!        `-                    -'
!
!     and the analogous rotation about the third, or z-, axis is
!     represented by
!
!        [ x ]
!             3
!
!     which symbolizes the matrix
!
!        .-                    -.
!        |  cos(x)  sin(x)   0  |
!        |                      |
!        | -sin(x)  cos(x)   0  |
!        |                      |
!        |  0        0       1  |
!        `-                    -'
!
!     The input matrix is assumed to be the product of three
!     rotation matrices, each one of the form
!
!        .-                    -.
!        |  1      0       0    |
!        |                      |
!        |  0    cos(r)  sin(r) |     (rotation of r radians about the
!        |                      |      x-axis),
!        |  0   -sin(r)  cos(r) |
!        `-                    -'
!
!
!        .-                    -.
!        | cos(s)   0   -sin(s) |
!        |                      |
!        |  0       1      0    |     (rotation of s radians about the
!        |                      |      y-axis),
!        | sin(s)   0    cos(s) |
!        `-                    -'
!
!     or
!
!        .-                    -.
!        |  cos(t)  sin(t)   0  |
!        |                      |
!        | -sin(t)  cos(t)   0  |     (rotation of t radians about the
!        |                      |      z-axis),
!        |  0        0       1  |
!        `-                    -'
!
!     where the second rotation axis is not equal to the first or
!     third. Any rotation matrix can be factored as a sequence of
!     three such rotations, provided that this last criterion is met.
!
!     This routine is related to the routine EUL2XF which produces
!     a state transformation from an input set of axes, Euler angles
!     and derivatives.
!
!     The two subroutine calls shown here will not change
!     XFORM except for round off errors.
!
!        CALL XF2EUL ( XFORM,  AXISA, AXISB, AXISC, EULANG, UNIQUE )
!        CALL EUL2XF ( EULANG, AXISA, AXISB, AXISC, XFORM          )
!
!     On the other hand the two calls
!
!        CALL EUL2XF ( EULANG, AXISA, AXISB, AXISC, XFORM          )
!        CALL XF2EUL ( XFORM,  AXISA, AXISB, AXISC, EULANG, UNIQUE )
!
!     will leave EULANG unchanged only if the components of EULANG
!     are in the range produced by XF2EUL and the Euler representation
!     of the rotation component of XFORM is unique within that range.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Determine the rate of change of the right ascension and
!        declination of the pole of the moon, from the state
!        transformation matrix that transforms J2000 states to object
!        fixed states.
!
!        Recall that the rotation component of the state transformation
!        matrix is given by
!
!           [W]  [HALFPI-DEC]  [RA+HALFPI]
!              3             1            3
!
!
!        Use the meta-kernel shown below to load the required SPICE
!        kernels.
!
!
!           KPL/MK
!
!           File name: xf2eul_ex1.tm
!
!           This meta-kernel is intended to support operation of SPICE
!           example programs. The kernels shown here should not be
!           assumed to contain adequate or correct versions of data
!           required by SPICE-based user applications.
!
!           In order for an application to use this meta-kernel, the
!           kernels referenced here must be present in the user's
!           current working directory.
!
!           The names and contents of the kernels referenced
!           by this meta-kernel are as follows:
!
!              File name                     Contents
!              ---------                     --------
!              pck00010.tpc                  Planet orientation and
!                                            radii
!              naif0012.tls                  Leapseconds
!
!
!           \begindata
!
!              KERNELS_TO_LOAD = ( 'pck00010.tpc',
!                                  'naif0012.tls'  )
!
!           \begintext
!
!           End of meta-kernel
!
!
!        Example code begins here.
!
!
!              PROGRAM XF2EUL_EX1
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions.
!        C
!              DOUBLE PRECISION      HALFPI
!
!        C
!        C     Local parameters.
!        C
!              CHARACTER*(*)         META
!              PARAMETER           ( META   = 'xf2eul_ex1.tm' )
!
!              CHARACTER*(*)         UTCSTR
!              PARAMETER           ( UTCSTR = 'May 15, 2007' )
!
!        C
!        C     Local variables.
!        C
!              DOUBLE PRECISION      EULANG ( 6    )
!              DOUBLE PRECISION      ET
!              DOUBLE PRECISION      FTMTRX ( 6, 6 )
!
!              INTEGER               I
!              INTEGER               J
!
!              LOGICAL               UNIQUE
!
!        C
!        C     Load SPICE kernels.
!        C
!              CALL FURNSH ( META )
!
!        C
!        C     Convert the input time to seconds past J2000 TDB.
!        C
!              CALL STR2ET ( UTCSTR, ET )
!
!        C
!        C     Get the transformation matrix from J2000 frame to
!        C     IAU_MOON.
!        C
!              CALL SXFORM ( 'J2000', 'IAU_MOON', ET, FTMTRX )
!
!        C
!        C     Convert the transformation matrix to
!        C     Euler angles (3-1-3).
!        C
!              CALL XF2EUL ( FTMTRX, 3, 1, 3, EULANG, UNIQUE )
!
!        C
!        C     Display the results.
!        C
!              IF ( UNIQUE ) THEN
!
!                 WRITE(*,'(2A)') 'UTC: ', UTCSTR
!                 WRITE(*,'(A,F20.16)') 'W       = ', EULANG(1)
!                 WRITE(*,'(A,F20.16)') 'DEC     = ',
!             .                  HALFPI() - EULANG(2)
!                 WRITE(*,'(A,F20.16)') 'RA      = ',
!             .                  EULANG(3) - HALFPI()
!                 WRITE(*,'(A,F20.16)') 'dW/dt   = ', EULANG(4)
!                 WRITE(*,'(A,F20.16)') 'dDEC/dt = ', EULANG(5)
!                 WRITE(*,'(A,F20.16)') 'dRA/dt  = ', EULANG(6)
!
!              ELSE
!
!                 WRITE(*,*) 'The values in EULANG are not uniquely '
!             .          //  'determined.'
!
!              END IF
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        UTC: May 15, 2007
!        W       =  -2.6490877296701645
!        DEC     =   1.1869108599473206
!        RA      =  -1.5496443908099826
!        dW/dt   =   0.0000026578085601
!        dDEC/dt =   0.0000000004021737
!        dRA/dt  =   0.0000000039334471
!
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.0, 26-OCT-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary $Revisions section.
!
!        Added complete code example based on existing fragment.
!        Corrected input argument name in $Exceptions section.
!
!-    SPICELIB Version 2.0.1, 25-APR-2007 (EDW)
!
!        Corrected code in EUL2EF entry point $Examples section, example
!        showed a XF2EUL call:
!
!           CALL XF2EUL ( XFORM,  1, 2, 3, RPYANG )
!
!        The proper form of the call:
!
!           CALL XF2EUL ( XFORM,  1, 2, 3, RPYANG, UNIQUE )
!
!-    SPICELIB Version 2.0.0, 31-OCT-2005 (NJB)
!
!        Entry point EUL2XF was updated to allow axis sequences
!        in which the second angle is not distinct from the first
!        or third.
!
!-    SPICELIB Version 1.0.0, 31-JUL-1995 (WLT)
!
!-&


! $ Index_Entries
!
!     Euler angles and derivatives from state transformation
!
!-&


!
!     Spicelib Functions
!
      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Parameters
!
      INTEGER               ALPHA
      INTEGER               BETA
      INTEGER               GAMMA
      INTEGER               DALPHA
      INTEGER               DBETA
      INTEGER               DGAMMA

      PARAMETER           ( ALPHA  = 1 )
      PARAMETER           ( BETA   = 2 )
      PARAMETER           ( GAMMA  = 3 )
      PARAMETER           ( DALPHA = 4 )
      PARAMETER           ( DBETA  = 5 )
      PARAMETER           ( DGAMMA = 6 )

!
!     Local variables
!
      DOUBLE PRECISION      CA
      DOUBLE PRECISION      D
      DOUBLE PRECISION      DELTA  ( 3, 3 )
      DOUBLE PRECISION      DOMEGA ( 3 )
      DOUBLE PRECISION      DRDT   ( 3, 3 )
      DOUBLE PRECISION      DRDTRT ( 3, 3 )
      DOUBLE PRECISION      LOCANG ( 6 )
      DOUBLE PRECISION      OMEGA  ( 3 )
      DOUBLE PRECISION      R      ( 3, 3 )
      DOUBLE PRECISION      SA
      DOUBLE PRECISION      SOLUTN ( 3, 3 )
      DOUBLE PRECISION      U
      DOUBLE PRECISION      V


      INTEGER               A
      INTEGER               B
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               L
      INTEGER               LOCAXA
      INTEGER               LOCAXB
      INTEGER               LOCAXC
      INTEGER               NEXT   ( 3 )

!
!     Saved variables
!
      SAVE                  DELTA
      SAVE                  NEXT

!
!     Initial values
!
!     Keep in mind that matrices are stored in column order first so
!     the matrix below looks like the transpose of what's needed.  But
!     in fact it is the correct thing.
!

      DATA                  DELTA   /  0.0D0, -1.0D0,  1.0D0, &
                                       1.0D0,  0.0D0, -1.0D0, &
                                      -1.0D0,  1.0D0,  0.0D0  /


      DATA                  NEXT   / 2, 3, 1 /



!
!     The computation of the non-derivative terms EULANG is handled
!     by the SPICE routine M2EUL.  This routine contributes by
!     determining the derivative components of EULANG.
!
!     To understand the code below a rather lengthy derivation is
!     required.  If you're not interested in the details of this
!     derivation skip down to the  IF ( RETURN() ) THEN line of
!     code below.
!
!     First we note that if b is one of the basis vectors i,j, or k
!     or the opposite of one of these (-i, -j, or -k) then
!
!       [ ANGLE ]  * b  = COS( {1 - |<e_n,b>|}*ANGLE )b
!                n
!                       - SIN( ANGLE ) e_n x b
!
!     where <,> denotes the dot product, and x is used to denote the
!     cross product operation and e_1, e_2, and e_3 are the standard
!     basis vectors i, j, and k respectively.
!
!     Using M2EUL we can readily determine the values of ALPHA, BETA
!     and GAMMA such that
!
!
!        R   = [ ALPHA ]  [ BETA ]  [ GAMMA ]
!                       A         B          C
!
!
!    From this equation we have:
!
!        dR/dt =   dALPHA/dt OMEGA [ ALPHA ]  [ BETA ]  [ GAMMA ]
!                                 A         A         B          C
!
!              +   dBETA/dt  [ ALPHA ] OMEGA  [ BETA ]  [ GAMMA ]
!                                     A     B         B          C
!
!              +   dGAMMA/dt [ ALPHA ] [ BETA ]  OMEGA [ GAMMA ]
!                                     A        B      C         C
!
!     where OMEGA   is the cross product matrix.
!                n
!
!
!         [   0      D_3n    -D_2n  ]
!         |  -D_3n    0       D_1n  |
!         [   D_2n  -D_1n      0    ]
!
!
!     (D_ij   denotes the Kronecker delta.)  Note that OMEGA * v
!                                                           n
!     yields -e  x  v  for all vectors v.
!              n
!
!     Multiplying both sides of the equation for dR/dt by the transpose
!     of R yields:
!
!            T
!     dR/dt*R  = dALPHA/dt OMEGA
!                                A
!
!              + dBETA/dt  [ ALPHA ] OMEGA  [ -ALPHA ]
!                                   A     B           A
!
!              + dGAMMA/dt [ ALPHA ] [ BETA ] OMEGA [ -BETA ]  [-ALPHA]
!                                   A        B     C         B         A
!                        T
!     The product dR/dt*R  is a skew symmetric matrix and hence can
!     be represented as a cross product,
!               T
!        dR/dt*R  V  = W x V
!
!     for all vectors V, provided that
!
!                       T
!        W(1) =  dR/dt*R  (3,2)
!
!                       T
!        W(2) =  dR/dt*R  (1,3)
!
!                       T
!        W(3) =  dR/dt*R  (2,1)
!
!     For any vector V, there is a corresponding skew symmetric
!     matrix CROSS{V}  such that CROSS{V} * W  = V x W for all vectors
!     W.  Moreover, if ROT is any rotation, then
!
!                                           T
!           CROSS{ROT(V)} = ROT CROSS{V} ROT
!
!     This can easily be verified by noting that
!
!        ROT(VxU) = ROT(V) X ROT(U)
!
!     From these observations it follows that
!
!
!        W =   -dALPHA/dt e_A
!
!
!          -    dBETA/dt [ALPHA]  e_B
!                               A
!
!          -    dGAMMA/dt [ ALPHA ] [ BETA ] e_C
!                                  A        B
!
!
!        W =   -dALPHA/dt e_A
!
!
!          -    dBETA/dt {    COS ( ALPHA (1 - |<e_A,e_B>|)) e_B
!
!                          -  SIN ( ALPHA ) e_A x e_B }
!
!
!          -    dGAMMA/dt [ ALPHA ] {    COS(BETA(1 - |<e_B,e_C>|)) e_C
!                                  A
!                                     -  SIN (BETA) e_B x e_C }
!
!     But <e_A,e_B> = 0 = <e_B,e_C> so that the above expression
!     simplifies to
!
!        W =   -dALPHA/dt e_A
!
!
!          -    dBETA/dt {COS(ALPHA)e_B -  SIN(ALPHA) e_A x e_B}
!
!
!          -    dGAMMA/dt [ ALPHA ] {COS(BETA)e_C - SIN(BETA)e_B x e_C}
!                                  A
!
!     If we let L = 6 - A - B, then by construction e_L is the third
!     vector needed to complete the basis containing e_A and e_B.
!     Let D be +1 or -1, so that D*e_L = e_A x e_B
!     (note D = <e_L,e_A x e_B> )
!
!     Then applying our rotation formula again and simplifying we have
!
!     W =   -dALPHA/dt e_A
!
!
!       -  dBETA/dt {COS(ALPHA)e_B -  D*SIN(ALPHA) e_L }
!
!
!       -  dGAMMA/dt COS(BETA){ COS(ALPHA(1-<e_A , e_C>))e_C
!                              -SIN(ALPHA)   e_A x e_C }
!
!       +  dGAMMA/dt SIN(BETA){ COS(ALPHA(1-|<e_A,e_B x e_C>|))e_B x e_C
!                              -SIN(ALPHA) e_A x (e_B x e_C )
!
!
!     Now we have two cases: 1) e_A = e_C or 2)  e_C = e_L
!
!     Case 1. e_A = e_C
!     ====================
!
!        W =   -dALPHA/dt e_A
!
!
!          -  dBETA/dt {COS(ALPHA)e_B -  D*SIN(ALPHA) e_L }
!
!
!          -  dGAMMA/dt COS(BETA)e_A
!
!          -  dGAMMA/dt D*SIN(BETA)COS(ALPHA)e_L
!
!          -  dGAMMA/dt SIN(BETA)SIN(ALPHA)e_B
!
!
!        W = e_A{-dALPHA/dt - COS(BETA)dGAMMA/dt}
!          + e_B{ -COS(ALPHA)dBETA/dt -   SIN(ALPHA)SIN(BETA)dGAMMA/dt}
!          + e_L{D*SIN(ALPHA)dBETA/dt - D*COS(ALPHA)SIN(BETA)dGAMMA/dt}
!
!
!        let U =    COS(BETA)
!            V =  D*SIN(BETA)
!
!        then
!
!        W = e_A{-dALPHA/dt                                -U*dGAMMA/dt}
!          + e_B{         -COS(ALPHA)dBETA/dt -D*SIN(ALPHA)*V*dGAMMA/dt}
!          + e_L{        D*SIN(ALPHA)dBETA/dt   -COS(ALPHA)*V*dGAMMA/dt}
!
!
!     Case 2. e_L = e_C
!     ====================
!
!        W =   -dALPHA/dt e_A
!
!
!          -  dBETA/dt {COS(ALPHA)e_B -  D*SIN(ALPHA) e_L }
!
!
!          -  dGAMMA/dt COS(BETA){ COS(ALPHA)e_L
!                                 -D*SIN(ALPHA)e_B }
!
!          +  dGAMMA/dt SIN(BETA) D*e_A
!
!
!       W  = e_A{-dALPHA/dt + D*SIN(BETA)dGAMMA/dt}
!          + e_B{-COS(ALPHA)dBETA/dt  - D*SIN(ALPHA)COS(BETA)dGAMMA/dt}
!          + e_L{D*SIN(ALPHA)dBETA/dt -   COS(ALPHA)COS(BETA)dGAMMA/dt}
!
!
!       Let U = -D*SIN(BETA)
!           V =    COS(BETA)
!
!       then
!
!       W  = e_A{-dALPHA/dt                  -              U*dGAMMA/dt}
!          + e_B{       -COS(ALPHA)*dBETA/dt - D*SIN(ALPHA)*V*dGAMMA/dt}
!          + e_L{      D*SIN(ALPHA)dBETA/dt  -   COS(ALPHA)*V*dGAMMA/dt}
!
!     As we can see from the above, by choosing appropriate assignments
!     for U and V, the two cases can be unified in a single expression.
!
!     Substituting CA and SA for COS(ALPHA) and SIN(ALPHA) and
!     re-writing the last expression in matrix form we have:
!
!
!                          [ -1     0      0 ][ 1  0  U ] [dALPHA/dt]
!      W  = {e_A  e_B  e_L}|  0   -CA  -D*SA || 0  1  0 | |dBETA /dt|
!                          [  0  D*SA    -CA ][ 0  0  V ] [dGAMMA/dt]
!
!
!     If we let E_n stand for the transpose of e_n, then solving for
!     the derivative vector we have:
!
!     [dALPHA/dt]   [ 1 0 -U/V ] [ -1     0     0] [ E_A ]
!     |dBETA /dt| = | 0 1   0  | |  0   -CA  D*SA| | E_B | W
!     [dGAMMA/dt]   [ 0 0  1/V ] [  0 -D*SA   -CA] [ E_L ]
!
!
!     But since the matrix product E_n W is <e_n,W> = W(n) this can
!     be rewritten as
!
!     [dALPHA/dt]   [ -1  U*D*SA/V  U*CA/V ] [ W(A) ]
!     |dBETA /dt| = |  0   -CA      D*SA   | [ W(B) |
!     [dGAMMA/dt]   [  0   -D*SA/V   -CA/V ] [ W(L) ]
!
!
!     Thus we see that there is a relatively elementary computation
!     required to determine the derivatives of the three Euler angles
!     returned by M2EUL.
!
!
!     Standard SPICE exception handling.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'XF2EUL' )

!
!     Get the rotation and derivative of the rotation separately.
!
      DO I = 1,3

         K = I + 3

         DO J = 1,3
            R    (I,J) = XFORM(I,J)
            DRDT (I,J) = XFORM(K,J)
         END DO

      END DO

!
!     We have to do it sooner or later so we take care of getting
!     the various Euler angles now.  This will take care of all the
!     bad axis cases too so we don't have to check here.
!
      CALL M2EUL ( R, AXISA,         AXISB,        AXISC, &
                      EULANG(ALPHA), EULANG(BETA), EULANG(GAMMA) )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'XF2EUL' )
         RETURN
      END IF

!
!     Construct local copies of the axes, determine L and D from the
!     derivation above.
!
      A = AXISA
      B = AXISB
      L = 6 - A - B
      D = DELTA(A,B)

!
!                      t
!     Compute DR/DT * R   and extract OMEGA
!
      CALL MXMT ( DRDT, R, DRDTRT )

!
!     The vector corresponding to DRDTRT is computed as shown below.
!
!        w(1) = drdtrt (3,2)
!        w(2) = drdtrt (1,3)
!        w(3) = drdtrt (2,1)
!
!     However, we need the 3-vector
!
!        w(A)
!        w(B)
!        w(L)
!
!     We'll call this vector omega. It's computed as shown here.
!
!        omega(1) = w(A) = d*drdtrt(L,B)
!        omega(2) = w(B) = d*drdtrt(A,L)
!        omega(3) = w(L) = d*drdtrt(B,A)
!

      OMEGA(1) = D*DRDTRT(L,B)
      OMEGA(2) = D*DRDTRT(A,L)
      OMEGA(3) = D*DRDTRT(B,A)

!
!     Compute the various sines and cosines that we need.
!
      CA       = DCOS( EULANG(ALPHA) )
      SA       = DSIN( EULANG(ALPHA) )

      IF ( AXISA .EQ. AXISC ) THEN
         U  =       DCOS ( EULANG(BETA) )
         V  =   D * DSIN ( EULANG(BETA) )
      ELSE
         U  =  -D * DSIN( EULANG(BETA) )
         V  =       DCOS( EULANG(BETA) )
      END IF

!
!     To avoid floating point overflows we make sure that we
!     can perform a division by V.  We do this by looking at U.
!     If it has absolute value 1, then we set V equal to zero.
!     After all U*U + V*V = 1 if SIN and COS and various arithmetic
!     operations work perfectly.
!
      IF ( DABS(U) .EQ. 1.0D0 ) THEN
         V = 0.0D0
      END IF

!
!     We have to look at the singular case first. Recall from above that
!
!        [ W(A) ]   [ -1     0     -U   ][dALPHA/dt]
!        | W(B) | = |  0   -CA  -D*SA*V ||dBETA /dt|
!        [ W(C) ]   [  0  D*SA    -CA*V ][dGAMMA/dt]
!
!     The singularity arises if V = 0.  In this case the equation
!     becomes:  ( Note that U  is plus or minus 1 so that division
!     by U is the same as multiplication by U. )
!
!        [ OMEGA(1) ]   [ -1     0  -U  ][dALPHA/dt]
!        | OMEGA(2) | = |  0   -CA   0  ||dBETA /dt|
!        [ OMEGA(3) ]   [  0  D*SA   0  ][dGAMMA/dt]
!
      IF ( V .EQ. 0 ) THEN

         UNIQUE         = .FALSE.
         EULANG(DALPHA) =  0.0D0
         EULANG(DGAMMA) = -U*OMEGA(1)

!
!        We solve for EULANG(DBETA) by selecting the more stable of
!        the two available equations.
!
         IF ( DABS(CA) .GT. DABS(SA) ) THEN
            EULANG(DBETA) =   -OMEGA(2)/CA
         ELSE
            EULANG(DBETA) =  D*OMEGA(3)/SA
         END IF


         CALL CHKOUT ( 'XF2EUL' )
         RETURN

      END IF

!
!     The matrix needed to compute the derivatives uniquely
!     exists.  Construct it and carry out the multiplication.
!
!     [dALPHA/dt]   [ -1  U*D*SA/V  U*CA/V ] [ OMEGA(1) ]
!     |dBETA /dt| = |  0   -CA      D*SA   | [ OMEGA(2) |
!     [dGAMMA/dt]   [  0   -D*SA/V   -CA/V ] [ OMEGA(3) ]
!

      UNIQUE      = .TRUE.

      SOLUTN(1,1) =  -1.0D0
      SOLUTN(2,1) =   0.0D0
      SOLUTN(3,1) =   0.0D0

      SOLUTN(1,2) =   U*D*SA/V
      SOLUTN(2,2) =      -CA
      SOLUTN(3,2) =    -D*SA/V

      SOLUTN(1,3) =     U*CA/V
      SOLUTN(2,3) =     D*SA
      SOLUTN(3,3) =      -CA/V

      CALL MXV ( SOLUTN, OMEGA, EULANG(4) )

      CALL CHKOUT ( 'XF2EUL' )
      RETURN


!$Procedure EUL2XF ( Euler angles and derivative to transformation )

      ENTRY      EUL2XF ( EULANG, AXISA, AXISB, AXISC, XFORM )

! $ Abstract
!
!     Compute a state transformation from an Euler angle factorization
!     of a rotation and the derivatives of those Euler angles.
!
! $ Required_Reading
!
!     ROTATION
!
! $ Keywords
!
!     ANGLES
!     DERIVATIVES
!     STATE
!
! $ Declarations
!
!     DOUBLE PRECISION      EULANG ( 6 )
!     INTEGER               AXISA
!     INTEGER               AXISB
!     INTEGER               AXISC
!     DOUBLE PRECISION      XFORM  ( 6, 6 )
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     EULANG     I   An array of Euler angles and their derivatives.
!     AXISA      I   Axis A of the Euler angle factorization.
!     AXISB      I   Axis B of the Euler angle factorization.
!     AXISC      I   Axis C of the Euler angle factorization.
!     XFORM      O   A state transformation matrix.
!
! $ Detailed_Input
!
!     EULANG   is the set of Euler angles corresponding to the
!              specified factorization.
!
!              If we represent R as shown here:
!
!                 R =  [ ALPHA ]      [ BETA ]      [ GAMMA ]
!                               AXISA         AXISB          AXISC
!
!              then
!
!                 EULANG(1) = ALPHA
!                 EULANG(2) = BETA
!                 EULANG(3) = GAMMA
!                 EULANG(4) = dALPHA/dt
!                 EULANG(5) = dBETA/dt
!                 EULANG(6) = dGAMMA/dt
!
!
!     AXISA,
!     AXISB,
!     AXISC    are the axes desired for the factorization of R.
!
!              All must be in the range from 1 to 3. Moreover
!              it must be the case that AXISA and AXISB are distinct
!              and that AXISB and AXISC are distinct.
!
!              Every rotation matrix can be represented as a product
!              of three rotation matrices about the principal axes
!              of a reference frame.
!
!                 R =  [ ALPHA ]      [ BETA ]      [ GAMMA ]
!                               AXISA         AXISB          AXISC
!
!              The value 1 corresponds to the X axis.
!              The value 2 corresponds to the Y axis.
!              The value 3 corresponds to the Z axis.
!
! $ Detailed_Output
!
!     XFORM    is the state transformation matrix corresponding to R
!              and dR/dt as described above. Pictorially,
!
!                 .-             -.
!                 |       |       |
!                 |   R   |   0   |
!                 |       |       |
!                 |-------+-------|
!                 |       |       |
!                 | dR/dt |   R   |
!                 |       |       |
!                 `-             -'
!
!              where R is a rotation matrix that varies with respect to
!              time and dR/dt is its time derivative.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If any of AXISA, AXISB, or AXISC do not have values in
!
!            { 1, 2, 3 }
!
!         an error is signaled by a routine in the call tree of this
!         routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     A word about notation: the symbol
!
!        [ x ]
!             i
!
!     indicates a coordinate system rotation of x radians about the
!     ith coordinate axis. To be specific, the symbol
!
!        [ x ]
!             1
!
!     indicates a coordinate system rotation of x radians about the
!     first, or x-, axis; the corresponding matrix is
!
!        .-                    -.
!        |  1      0       0    |
!        |                      |
!        |  0    cos(x)  sin(x) |
!        |                      |
!        |  0   -sin(x)  cos(x) |
!        `-                    -'
!
!     Remember, this is a COORDINATE SYSTEM rotation by x radians; this
!     matrix, when applied to a vector, rotates the vector by -x
!     radians, not x radians. Applying the matrix to a vector yields
!     the vector's representation relative to the rotated coordinate
!     system.
!
!     The analogous rotation about the second, or y-, axis is
!     represented by
!
!        [ x ]
!             2
!
!     which symbolizes the matrix
!
!        .-                    -.
!        | cos(x)   0   -sin(x) |
!        |                      |
!        |  0       1      0    |
!        |                      |
!        | sin(x)   0    cos(x) |
!        `-                    -'
!
!     and the analogous rotation about the third, or z-, axis is
!     represented by
!
!        [ x ]
!             3
!
!     which symbolizes the matrix
!
!        .-                    -.
!        |  cos(x)  sin(x)   0  |
!        |                      |
!        | -sin(x)  cos(x)   0  |
!        |                      |
!        |  0        0       1  |
!        `-                    -'
!
!     The input matrix is assumed to be the product of three
!     rotation matrices, each one of the form
!
!        .-                    -.
!        |  1      0       0    |
!        |                      |
!        |  0    cos(r)  sin(r) |     (rotation of r radians about the
!        |                      |      x-axis),
!        |  0   -sin(r)  cos(r) |
!        `-                    -'
!
!
!        .-                    -.
!        | cos(s)   0   -sin(s) |
!        |                      |
!        |  0       1      0    |     (rotation of s radians about the
!        |                      |      y-axis),
!        | sin(s)   0    cos(s) |
!        `-                    -'
!
!     or
!
!        .-                    -.
!        |  cos(t)  sin(t)   0  |
!        |                      |
!        | -sin(t)  cos(t)   0  |     (rotation of t radians about the
!        |                      |      z-axis),
!        |  0        0       1  |
!        `-                    -'
!
!     where the second rotation axis is not equal to the first or
!     third. Any rotation matrix can be factored as a sequence of
!     three such rotations, provided that this last criterion is met.
!
!     This routine is intended to provide an inverse for XF2EUL.
!
!     The two subroutine calls shown here will not change
!     XFORM except for round off errors.
!
!        CALL XF2EUL ( XFORM,  AXISA, AXISB, AXISC, EULANG, UNIQUE )
!        CALL EUL2XF ( EULANG, AXISA, AXISB, AXISC, XFORM          )
!
!     On the other hand the two calls
!
!        CALL EUL2XF ( EULANG, AXISA, AXISB, AXISC, XFORM          )
!        CALL XF2EUL ( XFORM,  AXISA, AXISB, AXISC, EULANG, UNIQUE )
!
!     will leave EULANG unchanged only if the components of EULANG
!     are in the range produced by XF2EUL and the Euler representation
!     of the rotation component of XFORM is unique within that range.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Suppose you have a set of Euler angles and their derivatives
!        for a 3 1 3 rotation, and that you would like to determine
!        the equivalent angles and derivatives for a 1 2 3 rotation.
!
!           R = [ALPHA]  [BETA]  [GAMMA]
!                      3       1        3
!
!           R = [ROLL]  [PITCH]  [YAW]
!                     1        2      3
!
!        The following code example will perform the desired
!        computation.
!
!
!        Example code begins here.
!
!
!              PROGRAM EUL2XF_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local variables.
!        C
!              DOUBLE PRECISION      ABGANG ( 6    )
!              DOUBLE PRECISION      RPYANG ( 6    )
!              DOUBLE PRECISION      XFORM  ( 6, 6 )
!
!              LOGICAL               UNIQUE
!
!        C
!        C     Define the initial set of Euler angles.
!        C
!              ABGANG(1) =  0.01D0
!              ABGANG(2) =  0.03D0
!              ABGANG(3) =  0.09D0
!              ABGANG(4) = -0.001D0
!              ABGANG(5) = -0.003D0
!              ABGANG(6) = -0.009D0
!
!        C
!        C     Compute the equivalent angles and derivatives for a
!        C     1-2-3 rotation.
!        C
!              CALL EUL2XF ( ABGANG, 3, 1, 3, XFORM  )
!              CALL XF2EUL ( XFORM,  1, 2, 3, RPYANG, UNIQUE )
!
!              IF ( UNIQUE )  THEN
!
!                 WRITE(*,'(A)') '1-2-3 equivalent rotation to input '
!             .               // '(radians):'
!                 WRITE(*,'(2(A,F13.9))') 'Roll  ', RPYANG(1),
!             .                           ', dRoll/dt  ', RPYANG(4)
!                 WRITE(*,'(2(A,F13.9))') 'Pitch ', RPYANG(2),
!             .                           ', dPitch/dt ', RPYANG(5)
!                 WRITE(*,'(2(A,F13.9))') 'Yaw   ', RPYANG(3),
!             .                           ', dYaw/dt   ', RPYANG(6)
!
!              ELSE
!
!                 WRITE(*,*) 'The values in RPYANG are not uniquely '
!             .          //  'determined.'
!
!              END IF
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        1-2-3 equivalent rotation to input (radians):
!        Roll    0.029998501, dRoll/dt   -0.002999550
!        Pitch  -0.000299950, dPitch/dt   0.000059980
!        Yaw     0.099995501, dYaw/dt    -0.009998650
!
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.0, 26-OCT-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Added complete
!        code example based on existing fragment.
!
!-    SPICELIB Version 2.0.1, 25-APR-2007 (EDW)
!
!        Corrected code in $Examples section, example showed
!        a XF2EUL call:
!
!           CALL XF2EUL ( XFORM,  1, 2, 3, RPYANG )
!
!        The proper form of the call:
!
!           CALL XF2EUL ( XFORM,  1, 2, 3, RPYANG, UNIQUE )
!
!-    SPICELIB Version 2.0.0, 31-OCT-2005 (NJB)
!
!        Restriction that second axis must differ from both the first
!        and third axes was removed.
!
!-    SPICELIB Version 1.0.0, 31-JUL-1995 (WLT)
!
!-&


! $ Index_Entries
!
!     State transformation from Euler angles and derivatives
!
!-&


!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'EUL2XF' )

!
!     We're going to work with a local copy LOCANG of the euler angle
!     state vector EULANG.  We'll also use a local set of axis
!     numbers.
!
      CALL MOVED ( EULANG, 6, LOCANG )

      LOCAXA = AXISA
      LOCAXB = AXISB
      LOCAXC = AXISC

!
!     Parts of the following algorithm depend on the central axis
!     being different from the first and third axes.  We'll adjust
!     the axes and angles to make this so, if necessary.
!
      IF (  ( AXISB .EQ. AXISA ) .OR. ( AXISB .EQ. AXISC )  ) THEN

         IF ( AXISB .EQ. AXISA ) THEN
!
!           The first angle will "absorb" the second, and the
!           second will be set to zero.  All we do here is select
!           the first angle.
!
            I = 1
         ELSE
            I = 3
         END IF
!
!        Absorb the second angle into the selected angle and set the
!        second angle to zero.  The same goes for the angular rates.
!
         LOCANG(I)   = LOCANG(I)   + LOCANG(2)
         LOCANG(2)   = 0.D0

         LOCANG(I+3) = LOCANG(I+3) + LOCANG(5)
         LOCANG(5)   = 0.D0

!
!        Pick a second axis that doesn't match the others.  Since
!        the rotation angle about the second axis is zero, all that
!        matters here is picking a distinct axis.
!
         IF ( AXISC .EQ. NEXT(AXISA) ) THEN
!
!           The first axis is the predecessor of the third, so we pick
!           the successor of the third.
!
            LOCAXB = NEXT( AXISC )

         ELSE
!
!           Either the third axis is the predecessor of the first or
!           matches the first, so the successor of the first is our
!           choice.
!
            LOCAXB = NEXT( AXISA )

         END IF

      END IF

!
!     The following local variables are set:
!
!        LOCANG(*), LOCAXA, LOCAXB, LOCAXC
!
!     These variables describe the input rotation, but the second
!     axis is now guaranteed to differ from the first and third.
!
!     The derivation for everything that is about to happen here
!     is included in the previous entry point.
!
      CALL EUL2M ( LOCANG(ALPHA), LOCANG(BETA), LOCANG(GAMMA), &
                   LOCAXA,        LOCAXB,       LOCAXC,         R )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'EUL2XF' )
         RETURN
      END IF

!
!     Construct local copies of the axes, determine L and D from the
!     derivation above.
!
      A = LOCAXA
      B = LOCAXB
      L = 6 - A - B
      D = DELTA(A,B)

!
!     Compute the various sines and cosines that we need.
!
      CA       = DCOS( LOCANG(ALPHA) )
      SA       = DSIN( LOCANG(ALPHA) )

      IF ( LOCAXA .EQ. LOCAXC ) THEN
         U  =       DCOS ( LOCANG(BETA) )
         V  =   D * DSIN ( LOCANG(BETA) )
      ELSE
         U  =  -D * DSIN( LOCANG(BETA) )
         V  =       DCOS( LOCANG(BETA) )
      END IF

!
!                            t
!     Next we compute dR/dt R.  Recall from the derivation above
!     that
!
!
!        [ W(A) ]   [ -1     0     -U   ][dALPHA/dt]
!        | W(B) | = |  0   -CA  -D*SA*V ||dBETA /dt|
!        [ W(L) ]   [  0  D*SA    -CA*V ][dGAMMA/dt]
!
!     In the previous entry point we used OMEGA for the vector
!     of rearranged components of W.
!
!        OMEGA(1) = W(A) = D*DRDTRT(L,B)
!        OMEGA(2) = W(B) = D*DRDTRT(A,L)
!        OMEGA(3) = W(L) = D*DRDTRT(B,A)
!
!        DRDTRT(L,B) = D*OMEGA(1)
!        DRDTRT(A,L) = D*OMEGA(2)
!        DRDTRT(B,A) = D*OMEGA(3)
!
!        [ DRDTRT(L,B) ]   [ -D     0     -D*U ][dALPHA/dt]
!        | DRDTRT(A,L) | = |  0 -D*CA    -SA*V ||dBETA /dt|
!        [ DRDTRT(B,A) ]   [  0    SA  -D*CA*V ][dGAMMA/dt]
!
!     We set up the matrix of this equation in SOLUTN below
!     and compute D*OMEGA which we denote by the variable DOMEGA.
!
      SOLUTN(1,1) =  -D
      SOLUTN(2,1) =   0.0D0
      SOLUTN(3,1) =   0.0D0

      SOLUTN(1,2) =   0.0D0
      SOLUTN(2,2) =  -D*CA
      SOLUTN(3,2) =     SA

      SOLUTN(1,3) =     -D*U
      SOLUTN(2,3) =    -SA*V
      SOLUTN(3,3) =  -D*CA*V

      CALL MXV ( SOLUTN, LOCANG(4), DOMEGA )

      DRDTRT(L,B) =  DOMEGA(1)
      DRDTRT(B,L) = -DOMEGA(1)

      DRDTRT(A,L) =  DOMEGA(2)
      DRDTRT(L,A) = -DOMEGA(2)

      DRDTRT(B,A) =  DOMEGA(3)
      DRDTRT(A,B) = -DOMEGA(3)

      DRDTRT(1,1) =  0.0D0
      DRDTRT(2,2) =  0.0D0
      DRDTRT(3,3) =  0.0D0

      CALL MXM ( DRDTRT, R, DRDT )

      DO J = 1,3
         DO I = 1,3
            XFORM(I,  J  ) = R   (I,J)
            XFORM(I+3,J+3) = R   (I,J)
            XFORM(I+3,J  ) = DRDT(I,J)
            XFORM(I,  J+3) = 0.0D0
         END DO
      END DO

      CALL CHKOUT ( 'EUL2XF' )
      RETURN

      END
