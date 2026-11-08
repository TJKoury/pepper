!$Procedure PXFORM ( Position Transformation Matrix )

      subroutine pxform ( from, to, et, rotate )

! $ Abstract
!
!     Return the matrix that transforms position vectors from one
!     specified frame to another at a specified epoch.
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

      character*(*)         from
      character*(*)         to
      double precision      et
      double precision      rotate  ( 3, 3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FROM       I   Name of the frame to transform from.
!     TO         I   Name of the frame to transform to.
!     ET         I   Epoch of the rotation matrix.
!     ROTATE     O   A rotation matrix.
!
! $ Detailed_Input
!
!     FROM     is the name of a reference frame in which a position
!              vector is known.
!
!     TO       is the name of a reference frame in which it is desired
!              to represent a position vector.
!
!     ET       is the epoch in ephemeris seconds past the epoch of J2000
!              (TDB) at which the position transformation matrix ROTATE
!              should be evaluated.
!
! $ Detailed_Output
!
!     ROTATE   is the matrix that transforms position vectors from
!              the reference frame FROM to the frame TO at epoch ET.
!              If (x, y, z) is a position relative to the frame FROM
!              then the vector ( x', y', z') is the same position
!              relative to the frame TO at epoch ET. Here the
!              vector ( x', y', z' ) is defined by the equation:
!
!                 .-   -.     .-        -.   .-  -.
!                 | x'  |     |          |   | x  |
!                 | y'  |  =  |  ROTATE  |   | y  |
!                 | z'  |     |          |   | z  |
!                 `-   -'     `-        -'   `-  -'
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If sufficient information has not been supplied via loaded
!         SPICE kernels to compute the transformation between the
!         two frames, an error is signaled by a routine
!         in the call tree of this routine.
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
!     This routine provides the user level interface to computing
!     position transformations from one reference frame to another.
!
!     Note that the reference frames may be inertial or non-inertial.
!     However, the user must take care that sufficient SPICE kernel
!     information is loaded to provide a complete position
!     transformation path from the FROM frame to the TO frame.
!
!     A common type of reference frame transformation is one from one
!     time-dependent frame to another, where the orientations of the
!     frames are computed at different epochs. For example, a remote
!     sensing application may compute the transformation from a target
!     body-fixed frame, with its orientation evaluated at the epoch of
!     photon emission, to a spacecraft instrument frame, with its
!     orientation evaluated at the epoch of photon reception. The
!     SPICELIB routine PXFRM2 computes this type of frame
!     transformation.
!
! $ Examples
!
!     Suppose that you have geodetic coordinates of a station on the
!     surface of the earth and that you need the inertial (J2000)
!     position of this station. The following code fragment
!     illustrates how to transform the position of the station to a
!     J2000 position.
!
!        CALL BODVRD ( 'EARTH', RADII, 3, N, ABC  )
!
!        EQUATR   =  ABC(1)
!        POLAR    =  ABC(3)
!        F        = (EQUATR - POLAR) / EQUATR
!
!        CALL GEOREC ( LONG, LAT, 0.0D0,  EQUATR, F, EPOS )
!
!        CALL PXFORM ( 'IAU_EARTH', 'J2000', ET,  ROTATE  )
!        CALL MXV    (  ROTATE,      EPOS,   JPOS         )
!
!     The position JPOS is the desired J2000 position of the station.
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
!-    SPICELIB Version 1.1.1, 09-AUG-2021 (NJB) (JDR)
!
!        Edited the header to comply with NAIF standard. Updated the
!        $Particulars header section to mention PXFRM2.
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
!        Various header corrections were made.
!
!-    SPICELIB Version 1.0.0, 05-APR-1999 (WLT)
!
!-&


! $ Index_Entries
!
!     Find a position transformation matrix
!
!-&


!
!     Spicelib Functions
!
      logical               return

!
!     Local parameters.
!

!
!     Saved frame name length.
!
      integer               frnmln
      parameter           ( frnmln = 32 )

!
!     Local Variables.
!
      integer               fcode
      integer               tcode

!
!     Saved frame name/ID item declarations.
!
      integer               svctr1 ( ctrsiz )
      character*(frnmln)    svfrom
      integer               svfcod

      integer               svctr2 ( ctrsiz )
      character*(frnmln)    svto
      integer               svtcde

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

      call chkin ( 'PXFORM' )

!
!     Initialization.
!
      if ( first ) then

!
!        Initialize counters.
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
      if      ( fcode .ne. 0 .and. tcode .ne. 0 ) then

         call refchg ( fcode, tcode, et, rotate )

      else if ( fcode .eq. 0 .and. tcode .eq. 0 ) then

         call setmsg ( 'Neither of the frames # or # was ' &
         //            'recognized as a known reference frame. ' )
         call errch  ( '#', from )
         call errch  ( '#', to   )
         call sigerr ( 'SPICE(UNKNOWNFRAME)'  )

      else if ( fcode .eq. 0 ) then

         call setmsg ( 'The frame # was not recognized as a ' &
         //            'known reference frame. ' )
         call errch  ( '#', from )
         call sigerr ( 'SPICE(UNKNOWNFRAME)'  )

      else if ( tcode .eq. 0 ) then

         call setmsg ( 'The frame # was not recognized as a ' &
         //            'known reference frame. ' )
         call errch  ( '#', to   )
         call sigerr ( 'SPICE(UNKNOWNFRAME)'  )

      end if

      call chkout ( 'PXFORM' )
      return
      end
