!$Procedure SPKE17 ( Evaluate a type 17 SPK data record)

      subroutine spke17 ( et, recin, state )

! $ Abstract
!
!     Evaluate a single SPK data record from a segment of type 17
!     (Equinoctial Elements).
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

      double precision      et
      double precision      recin  ( * )
      double precision      state  ( 6 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ET         I   Target epoch.
!     RECIN      I   Data record.
!     STATE      O   State (position and velocity).
!
! $ Detailed_Input
!
!     ET       is a target epoch, specified as ephemeris seconds past
!              J2000, at which a state vector is to be computed.
!
!     RECIN    is a data record which, when evaluated at epoch ET,
!              will give the state (position and velocity) of some
!              body, relative to some center, in some inertial
!              reference frame.
!
!              The structure of RECIN is:
!
!                 RECIN (1)  epoch of the elements in ephemeris seconds
!                            past J2000.
!
!                 RECIN (2)-RECIN (10) Equinoctial Elements:
!
!                    RECIN (2)  is the semi-major axis (A) of the orbit.
!
!                    RECIN (3)  is the value of H at the specified
!                               epoch. ( E*SIN(ARGP+NODE) ).
!
!                    RECIN (4)  is the value of K at the specified epoch
!                               ( E*COS(ARGP+NODE) ).
!
!                    RECIN (5)  is the mean longitude (MEAN0+ARGP+NODE)
!                               at the epoch of the elements.
!
!                    RECIN (6)  is the value of P
!                               (TAN(INC/2)*SIN(NODE)) at the specified
!                               epoch.
!
!                    RECIN (7)  is the value of Q
!                               (TAN(INC/2)*COS(NODE)) at the specified
!                               epoch.
!
!                    RECIN (8)  is the rate of the longitude of periapse
!                               (dARGP/dt + dNODE/dt ) at the epoch of
!                               the elements. This rate is assumed to
!                               hold for all time.
!
!                    RECIN (9)  is the derivative of the mean longitude
!                               ( dM/dt + dARGP/dt + dNODE/dt ).  This
!                               rate is assumed to be constant.
!
!                    RECIN (10)  is the rate of the longitude of the
!                                ascending node ( dNODE/dt).
!
!                 RECIN (11) Right Ascension of the pole of the
!                            orbital reference system relative to the
!                            reference frame of the associated SPK
!                            segment.
!
!                 RECIN (12) Declination of the pole of the
!                            orbital reference system relative to
!                            the reference frame of the associated
!                            SPK segment.
!
! $ Detailed_Output
!
!     STATE    is the state produced by evaluating RECIN at ET.
!              Units are km and km/sec.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the eccentricity is greater than 0.9, the error
!         SPICE(BADECCENTRICITY) is signaled.
!
!     2)  If the semi-major axis is non-positive, the error
!         SPICE(BADSEMIAXIS) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine performs a cursory examination of the elements
!     of a type 17 SPK data record and then passes the equinoctial
!     elements contained in that record on to the SPICE routine
!     EQNCPV for evaluation.
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
!           IF ( TYPE .EQ. 17 ) THEN
!
!              CALL SPKR17 ( HANDLE, DESCR, ET, RECORD )
!                  .
!                  .  Look at the RECORD data.
!                  .
!              CALL SPKE17 ( ET, RECORD, STATE )
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
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 27-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 08-JAN-1997 (WLT)
!
!-&


! $ Index_Entries
!
!     evaluate type_17 SPK segment
!
!-&


!
!     SPICELIB Functions
!
      logical               return

!
!     Local Variables
!
      double precision      a
      double precision      rapole
      double precision      decpol
      double precision      h
      double precision      k
      double precision      ecc
      double precision      epoch

!
!     Standard SPICE error handling.
!
      if ( return() ) then
         return
      end if

      call chkin ( 'SPKE17')

!
!     Fetch the various entities from the input record, first the epoch.
!
      epoch  = recin(1)
      a      = recin(2)
      h      = recin(3)
      k      = recin(4)
      ecc    = dsqrt( h*h + k*k )

      rapole = recin(11)
      decpol = recin(12)


!
!     Check all the inputs here for obvious failures.  Yes, perhaps
!     this is overkill.  However, there is a lot more computation
!     going on in this routine so that the small amount of overhead
!     here should not be significant.
!
      if ( a .le. 0 ) then

         call setmsg ( 'The semi-major axis supplied to the ' &
         //            'SPK type 17 evaluator was non-positive.  This ' &
         //            'value must be positive. The value supplied was ' &
         //            '#.'                    )
         call errdp  ( '#', a                  )
         call sigerr ( 'SPICE(BADSEMIAXIS)'    )
         call chkout ( 'SPKE17'                )
         return

      else if ( ecc .gt. 0.9d0 ) then

         call setmsg ( 'The eccentricity supplied for a type 17 ' &
         //            'segment is greater than 0.9.  It must be ' &
         //            'less than 0.9.' &
         //            'The value supplied ' &
         //            'to the type 17 evaluator was #. ' )
         call errdp  ( '#',   ecc                )
         call sigerr ( 'SPICE(BADECCENTRICITY)'  )
         call chkout ( 'SPKE17'                  )
         return

      end if

!
!     That's all for here, just plug the elements into the routine
!     knows how to evaluate the equinoctial elements.
!
      call eqncpv ( et, epoch, recin(2), rapole, decpol, state )

!
!     That's all folks.  Check out and return.
!
      call chkout('SPKE17')
      return
      end
