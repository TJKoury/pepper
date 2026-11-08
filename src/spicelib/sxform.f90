!$Procedure SXFORM ( State Transformation Matrix )

      subroutine sxform ( from, to, et, xform )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Return the state transformation matrix from one frame to
!     another at a specified epoch.
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

      include               'zzctr.inc'

      character(*)          from
      character(*)          to
      real(wp)              et
      real(wp)              xform  ( 6, 6 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FROM       I   Name of the frame to transform from.
!     TO         I   Name of the frame to transform to.
!     ET         I   Epoch of the state transformation matrix.
!     XFORM      O   A state transformation matrix.
!
! $ Detailed_Input
!
!     FROM     is the name of a reference frame in which a state is
!              known.
!
!     TO       is the name of a reference frame in which it is
!              desired to represent the state.
!
!     ET       is the epoch in ephemeris seconds past the epoch of
!              J2000 (TDB) at which the state transformation matrix
!              should be evaluated.
!
! $ Detailed_Output
!
!     XFORM    is the state transformation matrix that transforms
!              states from the reference frame FROM to the frame TO
!              at epoch ET. If (x, y, z, dx, dy, dz) is a state
!              relative to the frame FROM then the vector ( x', y',
!              z', dx', dy', dz' ) is the same state relative to the
!              frame TO at epoch ET. Here the vector ( x', y', z',
!              dx', dy', dz' ) is defined by the equation:
!
!                 .-   -.     .-          -.   .-  -.
!                 | x'  |     |            |   | x  |
!                 | y'  |     |            |   | y  |
!                 | z'  |  =  |   XFORM    |   | z  |
!                 | dx' |     |            |   | dx |
!                 | dy' |     |            |   | dy |
!                 | dz' |     |            |   | dz |
!                 `-   -'     `-          -'   `-  -'
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If sufficient information has not been supplied via loaded
!         SPICE kernels to compute the transformation between the two
!         frames, an error is signaled by a routine in the call tree of
!         this routine.
!
!     2)  If either frame FROM or TO is not recognized, the error
!         SPICE(UNKNOWNFRAME) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine provides the user level interface to computing state
!     transformations from one reference frame to another.
!
!     Note that the reference frames may be inertial or non-inertial.
!     However, the user must take care that sufficient SPICE kernel
!     information is loaded to provide a complete state transformation
!     path from the FROM frame to the TO frame.
!
! $ Examples
!
!     Suppose that you have geodetic coordinates of a station on
!     the surface of the earth and that you need the inertial
!     (J2000) state of this station. The following code fragment
!     illustrates how to transform the position of the station to
!     a J2000 state.
!
!        CALL BODVRD ( 'EARTH', RADII, 3, N, ABC  )
!
!        EQUATR   =  ABC(1)
!        POLAR    =  ABC(3)
!        F        = (EQUATR - POLAR) / EQUATR
!
!        CALL GEOREC ( LONG, LAT, 0.0D0,  EQUATR, F,  ESTATE )
!
!        ESTATE(4) = 0.0D0
!        ESTATE(5) = 0.0D0
!        ESTATE(6) = 0.0D0
!
!        CALL SXFORM ( 'IAU_EARTH', 'J2000',   ET, XFORM  )
!        CALL MXVG   (  XFORM,       ESTATE, 6, 6, JSTATE )
!
!     The state JSTATE is the desired J2000 state of the station.
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
!     C.H. Acton         (JPL)
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.1, 05-JUL-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.0, 23-SEP-2013 (BVS)
!
!        Updated to save the input frame names and POOL state counters
!        and to do frame name-ID conversions only if the counters have
!        changed.
!
!-    SPICELIB Version 1.0.3, 27-FEB-2008 (BVS)
!
!        Added FRAMES to the $Required_Reading section.
!
!-    SPICELIB Version 1.0.2, 23-OCT-2005 (NJB)
!
!        Header example had invalid flattening factor computation;
!        this was corrected. Reference to BODVAR in header was
!        replaced with reference to BODVRD.
!
!-    SPICELIB Version 1.0.1, 29-JUL-2003 (NJB) (CHA)
!
!        Minor header changes were made to improve clarity.
!
!-    SPICELIB Version 1.0.0, 19-SEP-1995 (WLT)
!
!-&


! $ Index_Entries
!
!     Find a state transformation matrix
!
!-&


!
!     Spicelib Functions
!
      logical               return

!
!     Local parameters
!

!
!     Saved frame name length.
!
      integer(ip)           frnmln
      parameter           ( frnmln = 32 )

!
!     Local Variables.
!
      integer(ip)           fcode
      integer(ip)           tcode

!
!     Saved frame name/ID item declarations.
!
      integer(ip)           svctr1 ( ctrsiz )
      character(frnmln)     svfrom
      integer(ip)           svfcod

      integer(ip)           svctr2 ( ctrsiz )
      character(frnmln)     svto
      integer(ip)           svtcde

      logical               first

!
!     Saved frame name/ID items.
!
      save                  svctr1
      save                  svfrom
      save                  svfcod

      save                  svctr2
      save                  svto
      save                  svtcde

      save                  first

!
!     Initial values.
!
      data                  first   / .true. /


!
!     Standard SPICE error handling.
!
      if ( return() ) then
         return
      end if

      call chkin ( 'SXFORM' )

!
!     Initialization.
!
      if ( first ) then

!
!        Initialize counters
!
         call zzctruin( svctr1 )
         call zzctruin( svctr2 )

         first = .false.

      end if


      call zznamfrm ( svctr1, svfrom, svfcod, from, fcode )
      call zznamfrm ( svctr2, svto,   svtcde, to,   tcode )

!
!     Only non-zero id-codes are legitimate frame id-codes.  Zero
!     indicates that the frame wasn't recognized.
!
      if      ( fcode /= 0 .and. tcode /= 0 ) then

         call frmchg ( fcode, tcode, et, xform )

      else if ( fcode == 0 .and. tcode == 0 ) then

         call setmsg ( 'Neither of the frames # or # was ' &
         //            'recognized as a known reference frame. ' )
         call errch  ( '#', from )
         call errch  ( '#', to   )
         call sigerr ( 'SPICE(UNKNOWNFRAME)'  )

      else if ( fcode == 0 ) then

         call setmsg ( 'The frame # was not recognized as a ' &
         //            'known reference frame. ' )
         call errch  ( '#', from )
         call sigerr ( 'SPICE(UNKNOWNFRAME)'  )

      else if ( tcode == 0 ) then

         call setmsg ( 'The frame # was not recognized as a ' &
         //            'known reference frame. ' )
         call errch  ( '#', to   )
         call sigerr ( 'SPICE(UNKNOWNFRAME)'  )

      end if

      call chkout ( 'SXFORM' )
      return
      end
