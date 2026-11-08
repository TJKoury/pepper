!$Procedure ZZTEME ( J2000 to TEME at epoch )

      subroutine zzteme( et, j2tm, tm2j )
      use pepper_kinds, only: wp

! $ Abstract
!
!     J2000 to TEME, and TEME to J2000, probably.
!
! $ Required_Reading
!
!     FRAMES
!
! $ Keywords
!
!     FRAMES
!
! $ Declarations

      implicit none

      real(wp)              et
      real(wp)              j2tm    ( 6, 6 )
      real(wp)              tm2j    ( 6, 6 )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ET         I   Epoch of the state transformation operator.
!     J2TM       O   The state transformation operator J2000 to TEME.
!     TM2J       O   The state transformation operator TEME to J2000.
!
! $ Detailed_Input
!
!     ET         time expressed in terms of TDB seconds past epoch J2000
!                for which to to calculate the J2000 frame to TEME frame
!                transformation operator.
!
! $ Detailed_Output
!
!     J2TM       the 6x6 operator that transforms cartesian states from
!                the J2000 to TEME reference frame.
!
!     TM2J       the 6x6 operator that transforms cartesian states from
!                the TEME to J2000 reference frame.
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
!     TETE    True equator true equinox
!     MEME    Mean equator mean equinox
!     TEME    True equator mean equinox
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
!     N.J. Bachman   (JPL)
!     S.C. Krening   (JPL)
!     E.D. Wright    (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.0, 10-OCT-2021 (EDW)
!
!        Routine now returns TEME to J2000 mapping operator.
!
!-    SPICELIB Version 1.0.0, 06-MAR-2012 (NJB) (SCK) (EDW)
!
!-&

! $ Index_Entries
!
!   transformation J2000 frame to pseudo TEME
!   transformation pseudo TEME to J2000
!
!-&

!
!     SPICELIB functions
!
      logical               return

!
!     Local variables
!
      real(wp)              m1     ( 6, 6 )
      real(wp)              m1inv  ( 6, 6 )
      real(wp)              m2     ( 6, 6 )
      real(wp)              m2inv  ( 6, 6 )
      real(wp)              xj2000 ( 6 )
      real(wp)              z      ( 6 )
      real(wp)              zj2000 ( 6 )

      if ( return() ) return


!
!     Extract the MEME +X vector and its derivative, both
!     expressed relative to the J2000 frame, into X.
!
      call zzeprc76( et, m1    )
      call invstm  ( m1, m1inv )

      call moved ( m1inv(1,1), 6, xj2000 )

!
!     Extract the TETE +Z vector and its derivative, both
!     expressed relative to the MEME frame, into Z.
!
      call zzenut80( et, m2    )
      call invstm  ( m2, m2inv )

      call moved ( m2inv(1,3), 6, z )

!
!     Transform Z to the J2000 frame.
!
      call mxvg ( m1inv, z, 6, 6, zj2000 )

!
!     Compute the TEME to J2000 state transformation, and the
!     inverse.
!
      call zztwovxf ( zj2000, 3, xj2000, 1, tm2j )
      call invstm   ( tm2j,  j2tm )

      return
      end
