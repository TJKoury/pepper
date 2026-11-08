!$Procedure XF2RAV ( Transform to rotation and angular velocity)

      subroutine xf2rav ( xform, rot, av )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Determine the rotation matrix and angular velocity of the
!     rotation from a state transformation matrix.
!
! $ Required_Reading
!
!     ROTATION
!
! $ Keywords
!
!     FRAMES
!
! $ Declarations

      implicit none

      real(wp)              xform  ( 6, 6 )
      real(wp)              rot    ( 3, 3 )
      real(wp)              av     ( 3    )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     XFORM      I   is a state transformation matrix.
!     ROT        O   is the rotation associated with XFORM.
!     AV         O   is the angular velocity associated with XFORM.
!
! $ Detailed_Input
!
!     XFORM    is a state transformation matrix from one frame
!              FRAME1 to some other frame FRAME2.
!
! $ Detailed_Output
!
!     ROT      is a rotation that gives the transformation from
!              some frame FRAME1 to another frame FRAME2.
!
!     AV       is the angular velocity of the transformation.
!              In other words, if P is the position of a fixed
!              point in FRAME2, then from the point of view of
!              FRAME1, P rotates (in a right handed sense) about
!              an axis parallel to AV. Moreover the rate of rotation
!              in radians per unit time is given by the length of
!              AV.
!
!              More formally, the velocity V of P in FRAME1 is
!              given by
!                                 T
!                  V  = AV x ( ROT  * P )
!
!              The components of AV are given relative to FRAME1.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  No checks are performed on XFORM to ensure that it is indeed
!         a state transformation matrix.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine is essentially a macro routine for converting
!     state transformation matrices into the equivalent representation
!     in terms of a rotation and angular velocity.
!
!     This routine is an inverse of the routine RAV2XF.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Suppose that you wanted to determine the angular velocity
!        of the Earth body-fixed reference frame with respect to
!        J2000 at a particular epoch ET. The following code example
!        illustrates a procedure for computing the angular velocity.
!
!        Use the meta-kernel shown below to load the required SPICE
!        kernels.
!
!
!           KPL/MK
!
!           File name: xf2rav_ex1.tm
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
!              earth_720101_070426.bpc       Earth historical
!                                            binary PCK
!              naif0012.tls                  Leapseconds
!
!           \begindata
!
!              KERNELS_TO_LOAD = ( 'earth_720101_070426.bpc',
!                                  'naif0012.tls'            )
!
!           \begintext
!
!           End of meta-kernel
!
!
!        Example code begins here.
!
!
!              PROGRAM XF2RAV_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local parameters.
!        C
!              CHARACTER*(*)         META
!              PARAMETER           ( META   = 'xf2rav_ex1.tm' )
!
!              CHARACTER*(*)         UTCSTR
!              PARAMETER           ( UTCSTR = '2005-OCT-10 16:00:00' )
!
!        C
!        C     Local variables.
!        C
!              DOUBLE PRECISION      AV     ( 3    )
!              DOUBLE PRECISION      ET
!              DOUBLE PRECISION      FTMTRX ( 6, 6 )
!              DOUBLE PRECISION      ROT    ( 3, 3 )
!
!              INTEGER               I
!              INTEGER               J
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
!        C     ITRF93.
!        C
!              CALL SXFORM ( 'J2000', 'ITRF93', ET, FTMTRX )
!
!        C
!        C     Now get the angular velocity by calling XF2RAV
!        C
!              CALL XF2RAV ( FTMTRX, ROT, AV )
!
!        C
!        C      Display the results.
!        C
!              WRITE(*,'(A)') 'Rotation matrix:'
!              DO I = 1, 3
!
!                 WRITE(*,'(3F16.11)') ( ROT(I,J), J=1,3 )
!
!              END DO
!
!              WRITE(*,*)
!              WRITE(*,'(A)') 'Angular velocity:'
!              WRITE(*,'(3F16.11)') AV
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        Rotation matrix:
!          -0.18603277688  -0.98254352801   0.00014659080
!           0.98254338275  -0.18603282936  -0.00053610915
!           0.00055402128   0.00004429795   0.99999984555
!
!        Angular velocity:
!           0.00000004025   0.00000000324   0.00007292114
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
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.1, 19-MAY-2020 (JDR)
!
!        Edited the header to comply with NAIF standard. Added complete
!        code example based on existing example.
!
!        Added ROTATION to the required readings.
!
!-    SPICELIB Version 1.1.0, 28-JUL-1997 (WLT)
!
!        The example in version 1.0.0 was incorrect. The example
!        in version 1.1.0 fixes the previous problem.
!
!-    SPICELIB Version 1.0.0, 19-SEP-1995 (WLT)
!
!-&


! $ Index_Entries
!
!     State transformation to rotation and angular velocity
!
!-&


      integer(ip)           i
      integer(ip)           j

      real(wp)              drdt  ( 3, 3 )
      real(wp)              omega ( 3, 3 )


!
!     A state transformation matrix XFORM has the following form
!
!
!         [      |     ]
!         |  R   |  0  |
!         |      |     |
!         | -----+-----|
!         |  dR  |     |
!         |  --  |  R  |
!         [  dt  |     ]
!
!
!     where R is a rotation and dR/dt is the time derivative of that
!     rotation.  From this we can immediately read the rotation and
!     its derivative.
!
      do i = 1, 3
         do j = 1,3
            rot (i,j) = xform(i,  j)
            drdt(i,j) = xform(i+3,j)
         end do
      end do

!
!     Recall that ROT is a transformation that converts positions
!     in some frame FRAME1 to positions in a second frame FRAME2.
!
!     The angular velocity matrix OMEGA (the cross product matrix
!     corresponding to AV) has the following property.
!
!     If P is the position of an object that is stationary with
!     respect to FRAME2 then the velocity V of that object in FRAME1
!     is given by:
!                          t
!         V  =  OMEGA * ROT  *  P
!
!     But V is also given by
!
!                    t
!               d ROT
!         V =   -----  * P
!                 dt
!
!     So that
!                                  t
!                    t        d ROT
!         OMEGA * ROT    =   -------
!                               dt
!
!     Hence
!                             t
!                       d ROT
!         OMEGA    =   -------  *  ROT
!                         dt
!
!
!
      call mtxm ( drdt, rot, omega )

!
!     Recall that OMEGA has the form
!
!         _                     _
!        |                       |
!        |   0    -AV(3)  AV(2)  |
!        |                       |
!        |  AV(3)    0   -AV(1)  |
!        |                       |
!        | -AV(2)   AV(1)   0    |
!        |_                     _|
!
      av(1) = omega(3,2)
      av(2) = omega(1,3)
      av(3) = omega(2,1)

      return
      end
