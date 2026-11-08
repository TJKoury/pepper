!$Procedure ZZCORSXF ( Correct state transformation matrix )

      subroutine zzcorsxf ( xmit, dlt, xform, corxfm )

! $ Abstract
!
!     Correct a state transformation matrix for the rate of change of
!     light time.
!
! $ Required_Reading
!
!     ROTATION
!     SPK
!     TIME
!
! $ Keywords
!
!     FRAMES
!     MATRIX
!     ROTATION
!     STATE
!
! $ Declarations

      implicit none

      logical               xmit
      double precision      dlt
      double precision      xform  ( 6, 6 )
      double precision      corxfm ( 6, 6 )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     XMIT       I   Radiation direction flag.
!     DLT        I   Light time derivative with respect to TDB.
!     XFORM      I   State transformation matrix.
!     CORXFM     O   Corrected state transformation matrix.
!
! $ Detailed_Input
!
!     XMIT           is a logical flag indicating the sense of
!                    radiation transmission associated with
!                    the light time correction: XMIT is .TRUE.
!                    for transmission corrections and .FALSE.
!                    for reception corrections. See the header
!                    of SPKEZR for a detailed discussion of
!                    light time corrections.
!
!     DLT            is the derivative of one way light time measured
!                    in TDB seconds with respect to TDB. DLT is
!                    signed and unitless.
!
!     XFORM          is a 6x6 state transformation matrix. XFORM
!                    may transform states from an inertial frame to a
!                    body-fixed frame or vice versa. XFORM has the form
!
!                        -               -
!                       |         :       |
!                       |  R(t)   :   0   |
!                       |........ :.......|
!                       |         :       |
!                       | d(R)/dt :  R(t) |
!                       |         :       |
!                        -               -
!
!                    where R(t) is a time-dependent rotation matrix.
!
! $ Detailed_Output
!
!     CORXFM         is the input matrix XFORM after correction for the
!                    rate of change of light time indicated by DLT. Let
!                    LTSIGN be 1 for transmission corrections and -1
!                    for reception corrections. Then CORXFM has the
!                    form
!
!                        -                        -
!                       |             :            |
!                       |     R(t)    :     0      |
!                       |.............:............|
!                       |             :            |
!                       | S * d(R)/dt :    R(t)    |
!                       |             :            |
!                        -                        -
!
!                    where
!
!                       S = 1 + LTSIGN*DLT
!
!                    CORXFM may be used to transform state vectors
!                    between an inertial reference frame and a
!                    body-fixed reference frame associated with a
!                    light-time corrected epoch. See the Particulars
!                    section for details.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This is a utility routine designed to simplify transformation of
!     state vectors between an inertial and a body-fixed reference
!     frame, where the evaluation epoch of the body-fixed frame is
!     adjusted by a light time value.
!
!     For example, suppose the aberration-corrected velocity of a
!     target relative to an observer is to be transformed from an
!     inertial reference frame into a target centered, target
!     body-fixed reference frame, where the orientation of this frame
!     is to be corrected for one-way light time between a surface point
!     on the target body and the observer.
!
!     In the discussion below, we use ET as a synonym for TDB, since
!     this terminology is used throughout the SPICE system.
!
!     The orientation of the reference frame can be expressed as
!
!        R ( ET + LTSIGN*LT(ET) )
!
!     where R is a rotation matrix, ET is the TDB epoch associated with
!     an observer, LT(ET) is the light time magnitude associated with
!     the epoch ET at the observer, and LTSIGN is the sign of the light
!     time; LTSIGN is negative for reception case corrections.
!
!     The expression
!
!        ET + LTSIGN*LT(ET)
!
!     represents the light time corrected epoch. Then, according to the
!     chain rule, the derivative with respect to ET of R is
!
!               |
!        d(R)/dt|                   * ( 1 + LTSIGN*d(LT)/d(ET) )
!               |ET + LTSIGN*LT(ET)
!
!     In the expression above, the factor on the left is the rotation
!     derivative that could be obtained by calling SXFORM to look up
!     the inertial-to-body-fixed state transformation matrix at the
!     epoch
!
!        ET + LTSIGN*LT(ET)
!
!     This is the rotation derivative that would apply if light
!     time were constant.
!
!     The factor on the right is the scale factor S shown in the
!     Detailed Output section above.
!
! $ Examples
!
!     1) Express the velocity of Mars as seen from Earth in
!        the IAU_MARS reference frame, where the frame orientation is
!        corrected for light time. Contrast the results obtained
!        using uncorrected and corrected state transformation matrices.
!        Show that the result obtained using a corrected matrix
!        matches that obtained from SPKEZR.
!
!        Note that, while the velocity we'll compute is not physically
!        realistic, it's perfectly usable for computations such as
!        finding the velocity of the apparent sub-Earth point on Mars.
!
!        Use the meta-kernel shown below to load the required SPICE
!        kernels.
!
!
!           KPL/MK
!
!           File: zzcorsxf_ex1.tm
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
!              de421.bsp                     Planetary ephemeris
!              pck00008.tpc                  Planet orientation and
!                                            radii
!              naif0008.tls                  Leapseconds
!
!
!           \begindata
!
!              KERNELS_TO_LOAD = ( 'de421.bsp',
!                                  'pck00008.tpc',
!                                  'naif0008.tls'  )
!
!           \begintext
!
!           End of meta-kernel
!
!
!
!        Example code begins here.
!
!
!           PROGRAM EX1
!           IMPLICIT NONE
!
!     C
!     C     Local variables
!     C
!           DOUBLE PRECISION      ET
!           DOUBLE PRECISION      LT
!           DOUBLE PRECISION      DLT
!           DOUBLE PRECISION      XFORM  ( 6, 6 )
!           DOUBLE PRECISION      CORXFM ( 6, 6 )
!           DOUBLE PRECISION      STATE0 ( 6 )
!           DOUBLE PRECISION      STATE1 ( 6 )
!           DOUBLE PRECISION      STATE2 ( 6 )
!           DOUBLE PRECISION      STATE3 ( 6 )
!           DOUBLE PRECISION      VELDIF ( 3 )
!
!           INTEGER               I
!
!     C
!     C     Load kernels.
!     C
!           CALL FURNSH ( 'corsxf_ex1.tm' )
!
!     C
!     C     Convert an observation epoch to TDB.
!     C
!           CALL STR2ET ( '2008 MAR 23', ET )
!
!     C
!     C     Look up the aberration-corrected state
!     C     of Mars as seen from the Earth at ET
!     C     in the J2000 frame. Use SPKACS since this
!     C     routine returns the light time derivative.
!     C
!           CALL SPKACS ( 499, ET,     'J2000', 'LT+S',
!          .              399, STATE0, LT,      DLT     )
!
!           WRITE (*,*) ' '
!           WRITE (*,*) 'Mars-Earth light time derivative = ', DLT
!
!     C
!     C     Convert the state into the IAU_MARS frame at
!     C     ET-LT. This gives us the state without accounting
!     C     for the rate of change of light time.
!     C
!           CALL SXFORM ( 'J2000', 'IAU_MARS', ET-LT, XFORM  )
!           CALL MXVG   ( XFORM,   STATE0,     6,  6, STATE1 )
!
!     C
!     C     Display the velocity portion of the state.
!     C
!           WRITE (*,*) ' '
!           WRITE (*,*) 'IAU_MARS-relative velocity obtained '
!           WRITE (*,*) 'using SPKACS and SXFORM (km/s):'
!
!           WRITE (*, '(E24.16)' ) ( STATE1(I), I = 4, 6 )
!
!     C
!     C     Obtain the correct state transformation matrix
!     C     from ZZCORSXF; transform the state using this matrix.
!     C
!           CALL ZZCORSXF ( .FALSE., DLT,     XFORM, CORXFM )
!           CALL MXVG     ( CORXFM,   STATE0, 6,  6, STATE2 )
!
!     C
!     C     Display the velocity portion of the state.
!     C
!           WRITE (*,*) ' '
!           WRITE (*,*) 'IAU_MARS-relative velocity obtained '
!          .//          'using ZZCORSXF (km/s):'
!
!           WRITE (*, '(E24.16)' ) ( STATE2(I), I = 4, 6 )
!
!     C
!     C     Display the velocity difference:
!     C
!           CALL VSUB ( STATE2(4), STATE1(4), VELDIF )
!
!           WRITE (*,*) ' '
!           WRITE (*,*) 'Velocity difference (km/s):'
!           WRITE (*, '(E24.16)' ) ( VELDIF(I), I = 1, 3 )
!
!     C
!     C     Look up the desired state using SPKEZR for comparison.
!     C
!           CALL SPKEZR ( 'MARS',  ET,     'IAU_MARS', 'LT+S',
!          .              'EARTH', STATE3, LT                 )
!
!     C
!     C     Display the velocity portion of the state.
!     C
!           WRITE (*,*) ' '
!           WRITE (*,*) 'IAU_MARS-relative velocity obtained '
!          .//          'using SPKEZR (km/s):'
!
!           WRITE (*, '(E24.16)' ) ( STATE3(I), I = 4, 6 )
!
!     C
!     C     Display the velocity difference:
!     C
!           CALL VSUB ( STATE3(4), STATE2(4), VELDIF )
!
!           WRITE (*,*) ' '
!           WRITE (*,*) 'SPKEZR vs ZZCORSXF velocity difference (km/s):'
!           WRITE (*, '(E24.16)' ) ( VELDIF(I), I = 1, 3 )
!
!           END
!
!
!        When this program was executed on a PC/Linux/g77 system, the
!        output was
!
!           Mars-Earth light time derivative =   5.70610116E-05
!
!           IAU_MARS-relative velocity obtained
!           using SPKACS and SXFORM (km/s):
!            0.1094230439483713E+05
!           -0.7388150695390612E+04
!           -0.8550198289693935E+01
!
!           IAU_MARS-relative velocity obtained using ZZCORSXF (km/s):
!            0.1094167989684505E+05
!           -0.7387727898874676E+04
!           -0.8550198284585768E+01
!
!           Velocity difference (km/s):
!           -0.6244979920775222E+00
!            0.4227965159361702E+00
!            0.5108166334366615E-08
!
!           IAU_MARS-relative velocity obtained using SPKEZR (km/s):
!            0.1094167989684505E+05
!           -0.7387727898874676E+04
!           -0.8550198284585768E+01
!
!           SPKEZR vs ZZCORSXF velocity difference (km/s):
!            0.0000000000000000E+00
!            0.0000000000000000E+00
!            0.0000000000000000E+00
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 06-MAY-2008 (NJB)
!
!-&

! $ Index_Entries
!
!     correct state transformation for light time rate
!
!-&

!
!     Local variables
!
      double precision      ltsign
      double precision      scale

      integer               col

!
!     Determine the sign of the light time correction.
!
      if ( xmit ) then

         ltsign =  1.d0
      else
         ltsign = -1.d0
      end if

!
!     Since the only block we're changing is
!     the lower left, first copy the input matrix
!     to the output matrix.
!
      call moved ( xform, 36, corxfm )

!
!     Adjust the rotation derivative block for
!     the rate of change of light time. All
!     that's required is to scale the block by
!
!        1 + LTSIGN*DLT
!
!
      scale = 1.d0 + ltsign*dlt


      do col = 1, 3
!
!        Scale the vector starting at index
!        (4,COL) in place.
!
         call vsclip ( scale, corxfm(4,col) )

      end do

      end

