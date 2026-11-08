!$Procedure SPKPV ( S/P Kernel, position and velocity )

      subroutine spkpv ( handle, descr, et, ref, state, center )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Return the state (position and velocity) of a target body
!     relative to some center of motion in a specified frame.
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

      include               'zzctr.inc'

      integer(ip)           handle
      real(wp)              descr    ( 5 )
      real(wp)              et
      character(*)          ref
      real(wp)              state    ( 6 )
      integer(ip)           center

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     DESCR      I   Segment descriptor.
!     ET         I   Target epoch.
!     REF        I   Target reference frame.
!     STATE      O   Position, velocity.
!     CENTER     O   Center of state.
!
! $ Detailed_Input
!
!     HANDLE,
!     DESCR    are the file handle assigned to a SPK file, and the
!              descriptor for a segment within the file. Together
!              they determine the ephemeris data from which the
!              state of the body is to be computed.
!
!     ET       is the epoch (ephemeris time) at which the state
!              is to be computed.
!
!     REF      is the name of the reference frame to
!              which the vectors returned by the routine should
!              be rotated. This may be any frame supported by
!              the SPICELIB subroutine FRMCHG.
!
! $ Detailed_Output
!
!     STATE    contains the position and velocity, at epoch ET,
!              for whatever body is covered by the specified segment.
!              STATE has six elements: the first three contain the
!              body's position; the last three contain the body's
!              velocity. These vectors are rotated into the
!              specified reference frame, the origin of
!              which is located at the center of motion for the
!              body (see CENTER, below).  Units are always km and
!              km/sec.
!
!     CENTER   is the integer ID code of the center of motion for
!              the state.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the requested reference frame is not supported by the
!         current version of CHGIRF, the error SPICE(SPKREFNOTSUPP)
!         is signaled.
!
! $ Files
!
!     See argument HANDLE.
!
! $ Particulars
!
!     Once SPKPV was the most basic of the SPK readers, the reader upon
!     which SPKSSB, SPKAPP, and SPKEZ were built. However, its function
!     has now largely been replaced by SPKPVN. SPKPV should not normally
!     be called except by old software written before the release of
!     SPKPVN. This routine should be considered obsolete.
!
! $ Examples
!
!     In the following code fragment, an entire SPK file is searched
!     for segments containing a particular epoch. For each one found,
!     the body, center, segment identifier, and range at the epoch
!     are printed out.
!
!        CALL DAFOPR ( 'TEST.SPK', HANDLE )
!        CALL DAFBFS (             HANDLE )
!
!        CALL DAFFNA ( FOUND  )
!
!        DO WHILE ( FOUND )
!           CALL DAFGS ( DESCR )
!           CALL DAFUS ( DESCR, 2, 6, DC, IC )
!
!           IF ( DC(1) .LE. ET  .AND.  ET .LE. DC(2) ) THEN
!              CALL SPKPV ( HANDLE, DESCR, ET, 'J2000', STATE, CENTER )
!              CALL DAFGN ( IDENT )
!
!              WRITE (*,*)
!              WRITE (*,*) 'Body   = ', IC(1)
!              WRITE (*,*) 'Center = ', CENTER,
!              WRITE (*,*) 'ID     = ', IDENT
!              WRITE (*,*) 'Range  = ', VNORM ( STATE )
!           END IF
!
!           CALL DAFFNA ( FOUND )
!        END DO
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
!     K.R. Gehringer     (JPL)
!     J.M. Lynch         (JPL)
!     H.A. Neilan        (JPL)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     R.E. Thurman       (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 6.1.1, 14-APR-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary $Revision section. Moved SPK required reading from
!        $Literature_References to $Required_Reading section.
!
!-    SPICELIB Version 6.1.0, 06-DEC-2013 (BVS) (NJB)
!
!        Updated to save the input frame name and POOL state counter
!        and to do frame name-ID conversion only if the counter has
!        changed. In-line comment regarding frame change was edited.
!
!-    SPICELIB Version 6.0.0, 19-SEP-1995 (WLT)
!
!        The routine was updated to handle non-inertial frames.
!
!-    SPICELIB Version 5.0.0, 13-MAR-1995 (KRG)
!
!        The routine was updated to handle type 14.
!
!        A new exception, 3, was also added.
!
!-    SPICELIB Version 4.0.0, 04-NOV-1994 (WLT)
!
!        The routine was updated to handle type 15.
!
!-    SPICELIB Version 3.0.0, 04-AUG-1993 (NJB)
!
!        The routine was updated to handle types 08 and 09.
!
!-    SPICELIB Version 2.0.0, 01-APR-1992 (JML)
!
!        The routine was updated to handle type 05.
!
!-    SPICELIB Version 1.0.2, 18-JUL-1991 (NJB)
!
!        The description of the output STATE was expanded slightly.
!
!-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
!
!        Literature references added to the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (RET)
!
!-&


! $ Index_Entries
!
!     position and velocity from ephemeris
!     SPK file position and velocity
!
!-&


!
!     SPICELIB functions
!
      logical               return

!
!     Local parameters.
!

!
!     Saved frame name length.
!
      integer(ip), parameter :: frnmln = 32

!
!     Some local space is needed in which to return records, and
!     into which to unpack the segment descriptor.
!
      integer(ip)           irf
      integer(ip)           irfreq
      integer(ip)           ic         (    6 )
      real(wp)              dc         (    2 )
      real(wp)              xform      ( 6, 6 )
      real(wp)              tstate     ( 6    )

!
!     Saved frame name/ID item declarations.
!
      integer(ip)           svctr1 ( ctrsiz )
      character(frnmln)     svref
      integer(ip)           svirfr

      logical               first

!
!     Saved frame name/ID items.
!
      save                  svctr1
      save                  svref
      save                  svirfr

      save                  first

!
!     Initial values.
!
      data                  first   / .true. /


!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     Initialization.
!
      if ( first ) then

!
!        Initialize counter.
!
         call zzctruin( svctr1 )

         first = .false.

      end if


      call dafus ( descr, 2, 6, dc, ic )

      center = ic(2)
      irf    = ic(3)

!
!     Rotate the raw state from its native frame to the one requested
!     by the user only if the two frames differ.
!
      call zznamfrm ( svctr1, svref, svirfr, ref, irfreq )

      if ( irfreq == 0 ) then

         call setmsg ( 'No support for frame #.' )
         call errch  ( '#', ref                  )
         call sigerr ( 'SPICE(SPKREFNOTSUPP)'    )

      else if ( irfreq /= irf ) then

         call spkpvn ( handle, descr,  et, irf, tstate, center )
         call frmchg ( irf,    irfreq, et,              xform  )
         call mxvg   ( xform,  tstate,  6, 6,           state  )

      else

         call spkpvn ( handle, descr, et, irf, state, center )

      end if

      return
      end
