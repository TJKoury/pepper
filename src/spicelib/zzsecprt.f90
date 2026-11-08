!$Procedure ZZSECPRT ( Calculate dot terms for DPSPCE perturbation )

      subroutine zzsecprt ( isynfl, dg   , del   , xni  , &
                            omegao, atime, omgdot, xli  , &
                            xfact , xldot, xndot , xnddt )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!    Routine to calculate the dot terms for the secular perturbation
!    of a vehicle.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     SECULAR PERTURBATION
!
! $ Declarations

      implicit none

      integer(ip)           isynfl

      real(wp)              omegao
      real(wp)              atime
      real(wp)              omgdot
      real(wp)              xli
      real(wp)              dg     ( 10 )
      real(wp)              xfact
      real(wp)              xldot
      real(wp)              xni
      real(wp)              del    (  3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ISYNFL     I   Resonance flag
!     DG         I   Parameter array
!     DEL        I   Parameter array of delta values
!     XNI        I   An intermediate linear term passed from the
!                    calling routine
!     OMEGAO     I   Original argument of perigee
!     ATIME      I   An intermediate time term passed from the calling
!                    routine
!     OMGDOT     I   Time rate of change of argument of perigee
!     XLI        I   An intermediate angular term passed from the
!                    calling routine
!     XFACT      I   The value BFACT - XNQ
!     XLDOT      O   Time rate of change of XL
!     XNDOT      O   Time rate of change of XN
!     XNDDT      O   Time rate of change of XNDOT
!
! $ Detailed_Input
!
!     ISYNFL      is the flag used to indicate the need for resonance
!                 calculations.
!
!     DG          is the parameter array replacing the Dxxxx values.
!
!     DEL         is the parameter array replacing DEL1, DEL2 and DEL3.
!
!     XNI         is an intermediate linear term passed from the main
!                 term for the calculation of XLDOT = XNI  +  XFACT
!
!     OMEGAO      is the original value for the argument of perigee.
!
!     ATIME       is an intermediate time term passed from the main
!                 routine used to calculate the time dependent
!                 argument of perigee term XOMI
!
!     OMGDOT      is the time derivative of the argument of the perigee.
!
!     XLI         is an intermediate angular term
!
!     XFACT       is the value BFACT - XNQ calculated in ZZDPINIT
!
! $ Detailed_Output
!
!     XLDOT       time derivative of the XL term.
!
!     XNDOT       time derivative of the XN term.
!
!     XNDDT       second time derivative of XN, time derivative of the
!                 time derivative.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This subroutine was constructed from a section of code in ZZDPSEC
!     in the original Spacetrack 3 report.  The code block was called
!     using a set of conditional GO TO's.  The block has been written as
!     this subroutine to improve clarity and maintainability and to
!     conform to the NAIF style standard.
!
! $ Examples
!
!     None needed.
!
! $ Restrictions
!
!     1)  This routine should be called only by ZZDPSEC as part of the
!     DPSPCE subroutine package.  It has no other use.
!
! $ Literature_References
!
!     Spacetrack 3 report.
!
! $ Author_and_Institution
!
!     E.D. Wright      (JPL)
!
! $ Version
!
!-   SPICELIB Version 1.1.1, 03-OCT-2021 (NJB)
!
!       Reordered header sections. Corrected typo in comments.
!
!-   SPICELIB Version 1.1.0, 24-MAR-1999 (EDW)
!
!       Correction made to format of Version descriptions.
!       Name of variable OMGDT changed to OMGDOT to be consistent
!       with name usage in other deep space two line elements
!       routines.
!
!-   SPICELIB Version 1.0.0, MAY-19-1997 (EDW)
!
!
!-&

! $ Index_Entries
!
!     perturbed dot terms
!
!-&

!
!     Local variables.
!
      real(wp)              x2li
      real(wp)              x2omi
      real(wp)              xnddt
      real(wp)              xndot
      real(wp)              xomi

      real(wp), parameter :: g22 = 5.7686396_wp

      real(wp), parameter :: g32 = 0.95240898_wp

      real(wp), parameter :: g44 = 1.8014998_wp

      real(wp), parameter :: g52 = 1.0508330_wp

      real(wp), parameter :: g54 = 4.4108898_wp

      real(wp), parameter :: fasx2 = 0.13130908_wp

      real(wp), parameter :: fasx4 = 2.8843198_wp

      real(wp), parameter :: fasx6 = 0.37448087_wp



!
!     Calculate the dot terms with respect to the state of the
!     resonance flag.
!

      if ( isynfl == 0 ) then

!
!        Resonance flag set.
!

         xomi  = omegao + omgdot * atime
         x2omi = xomi   + xomi
         x2li  = xli    + xli

         xndot =  dg( 1  )*dsin( x2omi + xli  - g22 ) &
                + dg( 2  )*dsin( xli          - g22 ) &
                + dg( 3  )*dsin( xomi  + xli  - g32 ) &
                + dg( 4  )*dsin(-xomi  + xli  - g32 ) &
                + dg( 5  )*dsin( x2omi + x2li - g44 ) &
                + dg( 6  )*dsin( x2li         - g44 ) &
                + dg( 7  )*dsin( xomi  + xli  - g52 ) &
                + dg( 8  )*dsin(-xomi  + xli  - g52 ) &
                + dg( 9  )*dsin( xomi  + x2li - g54 ) &
                + dg( 10 )*dsin(-xomi  + x2li - g54 )

         xnddt =  dg( 1  )*dcos( x2omi + xli  - g22 ) &
                + dg( 2  )*dcos( xli          - g22 ) &
                + dg( 3  )*dcos( xomi  + xli  - g32 ) &
                + dg( 4  )*dcos(-xomi  + xli  - g32 ) &
                + dg( 7  )*dcos( xomi  + xli  - g52 ) &
                + dg( 8  )*dcos(-xomi  + xli  - g52 ) &
         +2.0_wp*(  dg( 5  )*dcos( x2omi + x2li - g44 ) &
                + dg( 6  )*dcos( x2li         - g44 ) &
                + dg( 9  )*dcos( xomi  + x2li - g54 ) &
                + dg( 10 )*dcos( xomi  + x2li - g54 ))

      else

!
!        Resonance flag not set
!

         xndot =  del(1)*dsin(        xli - fasx2   ) &
                + del(2)*dsin( 2.0_wp*( xli - fasx4 ) ) &
                + del(3)*dsin( 3.0_wp*( xli - fasx6 ) )

         xnddt =      del(1)*dcos(        xli - fasx2   ) &
               + 2.0_wp*del(2)*dcos( 2.0_wp*( xli - fasx4 ) ) &
               + 3.0_wp*del(3)*dcos( 3.0_wp*( xli - fasx6 ) )

      end if


      xldot = xni  +  xfact
      xnddt = xnddt * xldot

!
!     Hi!  What are you doing way down here?  Did you bring pizza?
!

      return
      end
