!$Procedure SPKW15 ( SPK, write a type 15 segment )

      subroutine spkw15 ( handle, body,  center, frame,  first, last, &
                          segid,  epoch, tp,     pa,     p,     ecc, &
                          j2flg,  pv,    gm,     j2,     radius      )

! $ Abstract
!
!     Write an SPK segment of type 15 given a type 15 data record.
!
! $ Required_Reading
!
!     SPK
!
! $ Keywords
!
!     EPHEMERIS
!
! $ Declarations

      implicit none

      integer               handle
      integer               body
      integer               center
      character*(*)         frame
      double precision      first
      double precision      last
      character*(*)         segid
      double precision      epoch
      double precision      tp     ( 3 )
      double precision      pa     ( 3 )
      double precision      p
      double precision      ecc
      double precision      j2flg
      double precision      pv     ( 3 )
      double precision      gm
      double precision      j2
      double precision      radius

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of an SPK file open for writing.
!     BODY       I   Body code for ephemeris object.
!     CENTER     I   Body code for the center of motion of the body.
!     FRAME      I   The reference frame of the states.
!     FIRST      I   First valid time for which states can be computed.
!     LAST       I   Last valid time for which states can be computed.
!     SEGID      I   Segment identifier.
!     EPOCH      I   Epoch of the periapse.
!     TP         I   Trajectory pole vector.
!     PA         I   Periapsis vector.
!     P          I   Semi-latus rectum.
!     ECC        I   Eccentricity.
!     J2FLG      I   J2 processing flag.
!     PV         I   Central body pole vector.
!     GM         I   Central body GM.
!     J2         I   Central body J2.
!     RADIUS     I   Equatorial radius of central body.
!
! $ Detailed_Input
!
!     HANDLE   is the file handle of an SPK file that has been
!              opened for writing.
!
!     BODY     is the NAIF ID for the body whose states are
!              to be recorded in an SPK file.
!
!     CENTER   is the NAIF ID for the center of motion associated
!              with BODY.
!
!     FRAME    is the reference frame that states are referenced to,
!              for example 'J2000'.
!
!     FIRST    are the bounds on the ephemeris times, expressed as
!     LAST     seconds past J2000.
!
!     SEGID    is the segment identifier. An SPK segment identifier
!              may contain up to 40 characters.
!
!     EPOCH    is the epoch of the orbit elements at periapse
!              in ephemeris seconds past J2000.
!
!     TP       is a vector parallel to the angular momentum vector
!              of the orbit at epoch expressed relative to FRAME. A
!              unit vector parallel to TP will be stored in the
!              output segment.
!
!     PA       is a vector parallel to the position vector of the
!              trajectory at periapsis of EPOCH expressed relative
!              to FRAME. A unit vector parallel to PA will be
!              stored in the output segment.
!
!     P        is the semi-latus rectum--- p in the equation:
!
!                 r = p/(1 + ECC*COS(Nu))
!
!     ECC      is the eccentricity.
!
!     J2FLG    is the J2 processing flag describing what J2
!              corrections are to be applied when the orbit is
!              propagated.
!
!              All J2 corrections are applied if the value of J2FLG
!              is not 1, 2 or 3.
!
!              If the value of the flag is 3 no corrections are
!              done.
!
!              If the value of the flag is 1 no corrections are
!              computed for the precession of the line of apsides.
!              However, regression of the line of nodes is
!              performed.
!
!              If the value of the flag is 2 no corrections are
!              done for the regression of the line of nodes.
!              However, precession of the line of apsides is
!              performed.
!
!              Note that J2 effects are computed only if the orbit
!              is elliptic and does not intersect the central body.
!
!     PV       is a vector parallel to the north pole vector of the
!              central body expressed relative to FRAME. A unit
!              vector parallel to PV will be stored in the output
!              segment.
!
!     GM       is the central body GM.
!
!     J2       is the central body J2 (dimensionless).
!
!     RADIUS   is the equatorial radius of the central body.
!
!     Units are radians, km, seconds.
!
! $ Detailed_Output
!
!     None. A type 15 segment is written to the file attached
!     to HANDLE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the eccentricity is less than zero, the error
!         SPICE(BADECCENTRICITY) is signaled.
!
!     2)  If the semi-latus rectum is 0, the error
!         SPICE(BADLATUSRECTUM) is signaled.
!
!     3)  If the pole vector, trajectory pole vector or periapsis vector
!         have zero length, the error SPICE(BADVECTOR) is signaled.
!
!     4)  If the trajectory pole vector and the periapsis vector are
!         not orthogonal, the error SPICE(BADINITSTATE) is signaled.
!         The test for orthogonality is very crude. The routine simply
!         checks that the dot product of the unit vectors parallel
!         to the trajectory pole and periapse vectors is less than
!         0.00001. This check is intended to catch blunders, not to
!         enforce orthogonality to double precision capacity.
!
!     5)  If the mass of the central body is non-positive, the error
!         SPICE(NONPOSITIVEMASS) is signaled.
!
!     6)  If the radius of the central body is negative, the error
!         SPICE(BADRADIUS) is signaled.
!
!     7)  If the segment identifier has more than 40 non-blank
!         characters, the error SPICE(SEGIDTOOLONG) is signaled.
!
!     8)  If the segment identifier contains non-printing characters,
!         the error SPICE(NONPRINTABLECHARS) is signaled.
!
!     9)  If there are inconsistencies in the BODY, CENTER, FRAME or
!         FIRST and LAST times, an error is signaled by
!         a routine in the call tree of this routine.
!
! $ Files
!
!     A new type 15 SPK segment is written to the SPK file attached
!     to HANDLE.
!
! $ Particulars
!
!     This routine writes an SPK type 15 data segment to the open SPK
!     file according to the format described in the type 15 section of
!     the SPK Required Reading. The SPK file must have been opened with
!     write access.
!
!     This routine is provided to provide direct support for the MASL
!     precessing orbit formulation.
!
! $ Examples
!
!     Suppose that at time EPOCH you have the J2000 periapsis
!      state of some object relative to some central body and would
!      like to create a type 15 SPK segment to model the motion of
!      the object using simple regression and precession of the
!      line of nodes and apsides. The following code fragment
!      illustrates how you can prepare such a segment. We shall
!      assume that you have in hand the J2000 direction of the
!      central body's pole vector, its GM, J2 and equatorial
!      radius. In addition we assume that you have opened an SPK
!      file for write access and that it is attached to HANDLE.
!
!     (If your state is at an epoch other than periapse the
!      fragment below will NOT produce a "correct" type 15 segment
!      for modeling the motion of your object.)
!
!      C
!      C     First we get the osculating elements.
!      C
!            CALL OSCELT ( STATE, EPOCH, GM, ELTS )
!
!      C
!      C     From these collect the eccentricity and semi-latus rectum.
!      C
!            ECC = ELTS ( 2 )
!            P   = ELTS ( 1 ) * ( 1.0D0 + ECC )
!      C
!      C     Next get the trajectory pole vector and the
!      C     periapsis vector.
!      C
!            CALL UCRSS ( STATE(1), STATE(4), TP )
!            CALL VHAT  ( STATE(1),           PA )
!
!      C
!      C     Enable both J2 corrections.
!      C
!
!           J2FLG = 0.0D0
!
!      C
!      C     Now add the segment.
!      C
!
!            CALL SPKW15 ( HANDLE, BODY,  CENTER, FRAME,  FIRST, LAST,
!            .              SEGID,  EPOCH, TP,     PA,    P,     ECC,
!            .              J2FLG,  PV,    GM,     J2,    RADIUS      )
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
!
! $ Version
!
!-    SPICELIB Version 2.1.0, 03-JUN-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.0.0, 29-MAY-2012 (NJB)
!
!        Input vectors that nominally have unit length
!        are mapped to local copies that actually do
!        have unit length. The applicable inputs are TP, PA,
!        and PV. The Detailed Input header section was updated
!        to reflect the change.
!
!        Some typos in error messages were corrected.
!
!-    SPICELIB Version 1.0.0, 28-NOV-1994 (WLT)
!
!-&


! $ Index_Entries
!
!     Write a type 15 SPK segment
!
!-&


!
!     SPICELIB Functions
!
      double precision      dpr
      double precision      vdot
      double precision      vsep

      integer               lastnb

      logical               failed
      logical               return
      logical               vzero

!
!     Local Variables
!
!
!     Segment descriptor size
!
      integer               ns
      parameter           ( ns      =   5 )

!
!     Segment identifier size
!
      integer               sidlen
      parameter           ( sidlen  =  40 )

!
!     SPK data type
!
      integer               type
      parameter           ( type    = 15 )

!
!     Range of printing characters
!
      integer               fprint
      parameter           ( fprint  =  32 )

      integer               lprint
      parameter           ( lprint  = 126 )

!
!     Number of items in a segment
!
      integer               datsiz
      parameter           ( datsiz  = 16 )


      double precision      angle
      double precision      dot
      double precision      descr ( ns )
      double precision      mytp  ( 3 )
      double precision      mypa  ( 3 )

      double precision      record ( datsiz )

      integer               i
      integer               value


!
!     Standard SPICE error handling.
!
      if ( return() ) then
         return
      end if

      call chkin ( 'SPKW15')

!
!     Fetch the various entities from the inputs and put them into
!     the data record, first the epoch.
!
      record(1) = epoch


!
!     Convert TP and PA to unit vectors.
!
      call vhat ( pa, mypa )
      call vhat ( tp, mytp )

!
!     The trajectory pole vector.
!
      call vequ ( mytp, record(2) )

!
!     The periapsis vector.
!
      call vequ ( mypa, record(5) )

!
!     Semi-latus rectum ( P in the P/(1 + ECC*COS(Nu)  ),
!     and eccentricity.
!
      record(8) = p
      record(9) = ecc

!
!     J2 processing flag.
!
      record(10) = j2flg

!
!     Central body pole vector.
!
      call vhat ( pv, record(11) )

!
!     The central mass, J2 and radius of the central body.
!
      record(14) = gm
      record(15) = j2
      record(16) = radius

!
!     Check all the inputs here for obvious failures.  It's much
!     better to check them now and quit than it is to get a bogus
!     segment into an SPK file and diagnose it later.
!

      if ( p .le. 0 ) then

         call setmsg ( 'The semi-latus rectum supplied to the ' &
         //            'SPK type 15 evaluator was non-positive.  This ' &
         //            'value must be positive. The value supplied was ' &
         //            '#.'                    )
         call errdp  ( '#', p                  )
         call sigerr ( 'SPICE(BADLATUSRECTUM)' )
         call chkout ( 'SPKW15'                )
         return

      else if ( ecc .lt. 0.0d0 ) then

         call setmsg ( 'The eccentricity supplied for a type 15 ' &
         //            'segment is negative.  It must be non-negative. ' &
         //            'The value supplied ' &
         //            'to the type 15 evaluator was #. ' )
         call errdp  ( '#',   ecc                )
         call sigerr ( 'SPICE(BADECCENTRICITY)'  )
         call chkout ( 'SPKW15'                  )
         return

      else if ( gm  .le. 0.0d0 ) then

         call setmsg ( 'The mass supplied for the central body ' &
         //            'of a type 15 segment was non-positive. ' &
         //            'Masses must be positive.  The value ' &
         //            'supplied was #. '        )
         call errdp  ( '#', gm                   )
         call sigerr ( 'SPICE(NONPOSITIVEMASS)'  )
         call chkout ( 'SPKW15'                  )
         return

      else if ( vzero(tp) ) then

         call setmsg ( 'The trajectory pole vector supplied to ' &
         //            'SPKW15 had length zero. The most likely ' &
         //            'cause of this problem is an uninitialized ' &
         //            'vector.'            )
         call sigerr ( 'SPICE(BADVECTOR)'   )
         call chkout ( 'SPKW15'             )
         return

      else if ( vzero(pa) ) then

         call setmsg ( 'The periapse vector supplied to SPKW15 ' &
         //            'had length zero. The most likely cause ' &
         //            'of this problem is an uninitialized vector.' )
         call sigerr ( 'SPICE(BADVECTOR)'   )
         call chkout ( 'SPKW15'             )
         return

      else if ( vzero(pv) ) then

         call setmsg ( 'The central pole vector supplied to ' &
         //            'SPKW15 had length zero. The most likely ' &
         //            'cause of this problem is an uninitialized ' &
         //            'vector. '           )
         call sigerr ( 'SPICE(BADVECTOR)'   )
         call chkout ( 'SPKW15'             )
         return


      else if ( radius .lt. 0.0d0 ) then

         call setmsg ( 'The central body radius was negative. ' &
         //            'It must be zero or positive.  The value ' &
         //            'supplied was #. '  )
         call errdp  ( '#', radius         )
         call sigerr ( 'SPICE(BADRADIUS)'  )
         call chkout ( 'SPKW15'            )
         return

      end if

!
!     One final check.  Make sure the pole and periapse vectors are
!     orthogonal. (We will use a very crude check but this should
!     rule out any obvious errors.)
!
      dot = vdot ( mypa, mytp )

      if ( abs(dot) .gt. 1.0d-5 ) then

         angle = vsep ( pa, tp ) * dpr()

         call setmsg ( 'The periapsis and trajectory pole ' &
         //            'vectors are not orthogonal. The angle ' &
         //            'between them is # degrees. '         )
         call errdp  ( '#',     angle        )
         call sigerr ( 'SPICE(BADINITSTATE)' )
         call chkout ( 'SPKW15'              )
         return

      end if
!
!     Make sure the segment identifier is not too long.
!
      if ( lastnb(segid) .gt. sidlen ) then

         call setmsg ( 'Segment identifier contains more than ' &
         //            '40 characters.'                          )
         call sigerr ( 'SPICE(SEGIDTOOLONG)'                     )
         call chkout ( 'SPKW15'                                  )
         return

      end if
!
!     Make sure it has only printing characters.
!
      do i = 1, lastnb(segid)

         value = ichar( segid(i:i) )

         if ( ( value .lt. fprint ) .or. ( value .gt. lprint ) ) then

            call setmsg ( 'The segment identifier contains ' &
            //            'the nonprintable character having ascii ' &
            //            'code #.'                               )
            call errint ( '#',   value                            )
            call sigerr ( 'SPICE(NONPRINTABLECHARS)'              )
            call chkout ( 'SPKW15'                                )
            return

         end if

      end do
!
!     All of the obvious checks have been performed on the input
!     record.  Create the segment descriptor. (FIRST and LAST are
!     checked by SPKPDS as well as consistency between BODY and CENTER).
!
      call spkpds ( body,  center, frame, type, first, last, &
                    descr        )

      if ( failed() ) then
         call chkout ( 'SPKW15' )
         return
      end if

!
!     Begin a new segment.
!
      call dafbna ( handle, descr, segid )

      if ( failed() ) then
         call chkout ( 'SPKW15' )
         return
      end if

      call dafada ( record, datsiz )

      if ( .not. failed() ) then
         call dafena
      end if

      call chkout ( 'SPKW15' )
      return
      end
