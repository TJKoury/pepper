!$Procedure SPKE10 ( Evaluate SPK record, type 10 )

      subroutine spke10 ( et, record, state )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Evaluate a single SPK data record from a segment of type 10
!     (NORAD two-line element sets.). This evaluator uses algorithms
!     as described in Vallado 2006 [4].
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

      include 'zzsgp4.inc'

      real(wp)              et
      real(wp)              record   ( * )
      real(wp)              state    ( 6 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ET         I   Target epoch.
!     RECORD     I   Data record.
!     STATE      O   State (position and velocity).
!
! $ Detailed_Input
!
!     ET       is a target epoch, specified as ephemeris seconds past
!              J2000, at which a state vector is to be computed.
!
!     RECORD   is a data record which, when evaluated at epoch ET,
!              will give the state (position and velocity) of some
!              body, relative to some center, in some inertial
!              reference frame.
!
!              The structure of RECORD is:
!
!                  RECORD(1)
!                     .            Geophysical Constants such as
!                     .            GM, J2, J3, J4, etc.
!                     .
!                  RECORD(NGEO)
!
!                  RECORD(NGEO + 1)
!                     .
!                     .            elements and epoch for the body
!                     .            at epoch 1.
!                     .
!                  RECORD(NGEO + NELEMS )
!
!                  RECORD(NGEO + NELEMS + 1)
!                     .
!                     .            elements and epoch for the body
!                     .            at epoch 2.
!                     .
!                  RECORD(NGEO + 2*NELEMS )
!
!              Epoch 1 and epoch 2 are the times in the segment that
!              bracket ET. If ET is less than the first time in the
!              segment then both epochs 1 and 2 are equal to the
!              first time. And if ET is greater than the last time
!              then, epochs 1 and 2 are set equal to this last time.
!
! $ Detailed_Output
!
!     STATE    is the state produced by evaluating RECORD at ET.
!              Units are km and km/sec relative to the J2000
!              reference frame.
!
! $ Parameters
!
!     NGEO     is the number of geophysical constants for SGP4 SPK
!              records.
!
!     AFSPC    set the SGP4 propagator to use the original
!              Space Track #3 GST algorithm as described in Hoots [1];
!              value defined in zzsgp4.inc.
!
!     IMPRVD   set the SGP4 propagator to use the improved GST
!              algorithm as defined in Vallado [4]; value defined in
!              zzsgp4.inc.
!
! $ Exceptions
!
!     1)  If a problem occurs when evaluating the two-line elements, an
!         error is signaled by a routine in the call tree of this
!         routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine interpolates a state from the two reference sets
!     of two-line element sets contained in RECORD.
!
!     It is assumed that this routine is used in conjunction with
!     the routine SPKR10 as shown here:
!
!        CALL SPKR10   ( HANDLE, DESCR, ET, RECORD         )
!        CALL SPKE10   (                ET, RECORD, STATE  )
!
!     Where it is known in advance that the HANDLE, DESCR pair points
!     to a type 10 data segment.
!
! $ Examples
!
!     The SPKEnn routines are almost always used in conjunction with
!     the corresponding SPKRnn routines, which read the records from
!     SPK files.
!
!     The data returned by the SPKRnn routine is in its rawest form,
!     taken directly from the segment. As such, it will be meaningless
!     to a user unless he/she understands the structure of the data type
!     completely. Given that understanding, however, the SPKRnn
!     routines might be used to examine raw segment data before
!     evaluating it with the SPKEnn routines.
!
!
!     C
!     C     Get a segment applicable to a specified body and epoch.
!     C
!           CALL SPKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND )
!
!     C
!     C     Look at parts of the descriptor.
!     C
!           CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
!           CENTER = ICD( 2 )
!           REF    = ICD( 3 )
!           TYPE   = ICD( 4 )
!
!           IF ( TYPE .EQ. 10 ) THEN
!
!              CALL SPKR10 ( HANDLE, DESCR, ET, RECORD )
!                  .
!                  .  Look at the RECORD data.
!                  .
!              CALL SPKE10 ( ET, RECORD, STATE )
!                  .
!                  .  Check out the evaluated state.
!                  .
!           END IF
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     [1]  F. Hoots and R. Roehrich, "Spacetrack Report #3: Models for
!          Propagation of the NORAD Element Sets," U.S. Air Force
!          Aerospace Defense Command, Colorado Springs, CO, 1980.
!
!     [2]  F. Hoots, "Spacetrack Report #6: Models for Propagation of
!          Space Command Element Sets,"  U.S. Air Force Aerospace
!          Defense Command, Colorado Springs, CO, 1986.
!
!     [3]  F. Hoots, P. Schumacher and R. Glover, "History of Analytical
!          Orbit Modeling in the U. S. Space Surveillance System,"
!          Journal of Guidance, Control, and Dynamics. 27(2):174-185,
!          2004.
!
!     [4]  D. Vallado, P. Crawford, R. Hujsak and T. Kelso, "Revisiting
!          Spacetrack Report #3," paper AIAA 2006-6753 presented at the
!          AIAA/AAS Astrodynamics Specialist Conference, Keystone, CO.,
!          August 21-24, 2006.
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
!-    SPICELIB Version 3.1.0, 10-OCT-2021 (JDR) (EDW)
!
!        Use of modified ZZTEME to eliminate a matrix inversion.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 3.0.0, 18-FEB-2015 (EDW)
!
!        Evaluator now uses Vallado derived propagator as described
!        in Vallado 2006 [4].
!
!-    SPICELIB Version 2.0.0, 01-JAN-2011 (EDW)
!
!        Correction of state transformation calculation. Algorithm
!        now computes state transformation as from TEME to J2000.
!        The previous version of this routine calculated TETE to
!        J2000.
!
!-    SPICELIB Version 1.1.0, 01-SEP-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in MTXV and VADD calls.
!
!-    SPICELIB Version 1.0.0, 18-JUL-1997 (WLT)
!
!-&


! $ Index_Entries
!
!     evaluate type_10 SPK segment
!
!-&


!
!     SPICELIB functions
!

      real(wp)              pi
      logical               failed
      logical               return

!
!     The nutation in obliquity and longitude as well as their rates
!     follow the elements.  So we've got four angles/angle rates
!     following the elements
!
      integer(ip)           nangs
      parameter           ( nangs = 4 )

!
!     The locations of the epochs and the starts of the element
!     sets are given below.
!
      integer(ip)           time1
      parameter           ( time1  = ngeo + kepoch )

      integer(ip)           time2
      parameter           ( time2  = nelems + nangs + time1  )

      integer(ip)           model1
      parameter           ( model1 = ngeo + 1      )

      integer(ip)           angs1
      parameter           ( angs1 = model1 + nelems )

      integer(ip)           model2
      parameter           ( model2 = angs1 + nangs )


!
!     Local variables
!
      real(wp)              j2tm     ( 6, 6 )
      real(wp)              tm2j     ( 6, 6 )
      real(wp)              s1       ( 6 )
      real(wp)              s2       ( 6 )
      real(wp)              tmpsta   ( 6 )
      real(wp)              vcomp    ( 3 )
      real(wp)              arg
      real(wp)              dargdt
      real(wp)              denom
      real(wp)              dwdt
      real(wp)              mypi
      real(wp)              numer
      real(wp)              t1
      real(wp)              t2
      real(wp)              w

      logical               first
      save

      data                  first /.true./

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'SPKE10' )
      end if

      if ( first ) then
         first  = .false.
         mypi   = pi()
      end if

!
!     Fetch the two epochs stored in the record.
!
      t1 = record(time1)
      t2 = record(time2)

!
!     Evaluate the two states. Call them s_1(t) and s_2(t).
!     Let the position and velocity components be: p_1, v_1, p_2, v_2.
!
!     The final position is a weighted average.
!
!     Let
!
!        W(t) =  0.5 + 0.5*COS( PI*(t-t1)/(t2-t1) )
!
!     then
!
!        p  = W(t)*p_1(t) + (1 - W(t))*p_2(t)
!        v  = W(t)*v_1(t) + (1 - W(t))*v_2(t) + W'(t)*(p_1(t) - p_2(t))
!
!     If t1 = t2, the state is just s(t1).
!
!     Note: there are a number of weighting schemes we could have
!     used.  This one has the nice property that
!
!     The graph of W is symmetric about the point
!
!        ( (t1+t2)/2,  W( (t1+t2)/2 ) )
!
!     The range of W is from 1 to 0. The derivative of W is
!     symmetric and zero at both t1 and t2.
!

      if ( t1 /= t2 ) then

!
!        Initialize then propagate.
!
!        XXSGP4E returns on entry if XXSGP4I signals an error.
!

!
!        Evaluate first TLE.
!
         call xxsgp4i ( record(1), record(model1), afspc )

         if ( failed() ) then
            call chkout ( 'SPKE10' )
            return
         end if

!
!        Time from epoch of set 1 in minutes.
!
         call xxsgp4e ( (et - t1)/60.0_wp, s1 )

         if ( failed() ) then
            call chkout ( 'SPKE10' )
            return
         end if

!
!        Evaluate second TLE.
!
         call xxsgp4i ( record(1), record(model2), afspc )

         if ( failed() ) then
            call chkout ( 'SPKE10' )
            return
         end if

!
!        Time from epoch of set 2 in minutes.
!
         call xxsgp4e ( (et - t2)/60.0_wp, s2 )

         if ( failed() ) then
            call chkout ( 'SPKE10' )
            return
         end if

!
!        Compute the weighting function that we'll need later
!        when we combine states 1 and 2.
!
         numer  = et - t1
         denom  = t2 - t1
         arg    = numer*mypi/denom
         dargdt =       mypi/denom

         w      =  0.5_wp + 0.5_wp * dcos( arg )
         dwdt   =        - 0.5_wp * dsin( arg ) * dargdt

!
!        Now compute the weighted average of the two states.
!
         call vlcomg ( 6,  w,    s1,       1.0_wp-w, s2,     state     )
         call vlcom  (     dwdt, s1,       -dwdt,  s2,     vcomp     )
         call vadd   (           state(4),         vcomp,  tmpsta(4) )
         call vequ   (           tmpsta(4),                state(4)  )

      else

!
!        Evaluate the TLE.
!
         call xxsgp4i ( record(1), record(model1), afspc )

         if ( failed() ) then
            call chkout ( 'SPKE10' )
            return
         end if

!
!        Time from epoch of set 1 in minutes.
!
         call xxsgp4e ( (et - t1)/60.0_wp, state )

         if ( failed() ) then
            call chkout ( 'SPKE10' )
            return
         end if

      end if

!
!     Finally, convert the TEME state to J2000.  First
!     calculate the mapping from J2000 to TEME (J2TM), and from
!     TEME to J2000 (TM2J) at time ET...
!
!                         -1
!     Note that J2TM = TM2J
!
      call zzteme( et, j2tm, tm2j )

!
!     ...now convert the TEME state to a J2000 state.
!
      call mxvg   ( tm2j, state, 6, 6, tmpsta )
      call moved  ( tmpsta,  6,      state     )

      call chkout ( 'SPKE10' )
      return

      end
