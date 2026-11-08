!$Procedure SPKPVN ( S/P Kernel, position and velocity in native frame )

      subroutine spkpvn ( handle, descr, et, ref, state, center )

! $ Abstract
!
!     Return, for a specified SPK segment and time, the state (position
!     and velocity) of the segment's target body relative to its center
!     of motion.
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

      include 'spkrec.inc'

      integer               handle
      double precision      descr    ( 5 )
      double precision      et
      integer               ref
      double precision      state    ( 6 )
      integer               center

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     DESCR      I   Segment descriptor.
!     ET         I   Target epoch.
!     REF        O   Target reference frame.
!     STATE      O   Position, velocity.
!     CENTER     O   Center of state.
!     MAXREC     P   Maximum length of records returned by SPKRnn.
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
! $ Detailed_Output
!
!     REF      is the id-code of the reference frame to
!              which the vectors returned by the routine belong.
!
!     STATE    is a 6-dimensional vector that contains the position and
!              velocity, at epoch ET, for whatever body is covered by
!              the specified segment. STATE has six elements: the first
!              three contain the body's position; the last three contain
!              the body's velocity. These vectors are rotated into the
!              specified  reference frame, the origin of which is
!              located at the center of motion for the body (see CENTER,
!              below). Units are always km and km/sec.
!
!     CENTER   is the integer ID code of the center of motion for
!              the state.
!
! $ Parameters
!
!     MAXREC   is the maximum length of a record returned by any of
!              data type-specific routines SPKRnn, which are called
!              by SPKPVN (see $Particulars).
!
! $ Exceptions
!
!     1)  If the segment type is not supported by the current
!         version of SPKPVN, the error SPICE(SPKTYPENOTSUPP)
!         is signaled.
!
! $ Files
!
!     See argument HANDLE.
!
! $ Particulars
!
!     SPKPVN is the most basic of the SPK readers, the reader upon
!     which SPKPV and SPKGEO, etc. are built. It should not normally
!     be called directly except in cases where some optimization is
!     required. (That is, where the calling program has prior knowledge
!     of the center-barycenter shifts to be performed, or a non-standard
!     method of determining the files and segments to be used when
!     computing states.)
!
!     This is the only reader which makes distinctions between the
!     various segment types in the SPK format. The complete list
!     of types currently supported is shown below.
!
!        Type   Description
!        ----   -----------------------
!           1   Difference Lines
!           2   Chebyshev (P)
!           3   Chebyshev (P,V)
!           5   Two body propagation between discrete states
!           8   Lagrange interpolation, equally spaced discrete states
!           9   Lagrange interpolation, unequally spaced discrete states
!          12   Hermite interpolation, equally spaced discrete states
!          13   Hermite interpolation, unequally spaced discrete states
!          14   Chebyshev Unequally spaced
!          15   Precessing Ellipse
!          17   Equinoctial Elements
!          18   ESOC/DDID Hermite/Lagrange Interpolation
!          19   ESOC/DDID Piecewise Interpolation
!          20   Chebyshev (V)
!          21   Extended Modified Difference Array
!
!     SPKPVN is the only reader that needs to be changed in order to
!     add a new segment type to the SPK format. If a new data type is
!     added, the following steps should be taken:
!
!     1) Write two new routines, SPKRnn and SPKEnn, to read and
!        evaluate, respectively, a record from a data type nn segment.
!
!     2) Insert a new case into the body of SPKPVN to accommodate the
!        new type.
!
!     3) If necessary, adjust the parameter MAXREC, above, so that it
!        is large enough to encompass the maximum size of a record
!        returned by SPKRnn and passed to SPKEnn.
!
!        The maximum record lengths for each data type currently
!        supported are as follows:
!
!                  Data type       Maximum record length
!                  ---------       ---------------------
!                      1                    71
!                      2                    87
!                      3                   171
!                      5                    15
!                      8                   171
!                      9                   197
!                     12                    87
!                     13                    99
!                     14                 Variable
!                     15                    16
!                     17                    12
!                     18                   198
!                     19                   198
!                     20                   159
!                     21                   112
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
!
!           CALL DAFGS ( DESCR )
!           CALL DAFUS ( DESCR, 2, 6, DC, IC )
!
!           IF ( DC(1) .LE. ET  .AND.  ET .LE. DC(2) ) THEN
!              CALL SPKPVN ( HANDLE, DESCR, ET, REF, STATE, CENTER )
!              CALL DAFGN  ( IDENT )
!              CALL FRMNAM ( REF, FRAME )
!              WRITE (*,*)
!              WRITE (*,*) 'Body   = ', IC(1)
!              WRITE (*,*) 'Center = ', CENTER,
!              WRITE (*,*) 'ID     = ', IDENT
!              WRITE (*,*) 'Frame  = ', FRAME
!              WRITE (*,*) 'Range  = ', VNORM ( STATE )
!           END IF
!
!           CALL DAFFNA ( FOUND )
!
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
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 4.0.1, 27-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary $Revisions section. Improved $Abstract section.
!
!-    SPICELIB Version 4.0.0, 23-DEC-2013 (NJB)
!
!        Added support for types 19, 20 and 21. Added header
!        comments giving description for types 18, 19,
!        and 21. Removed header reference to type 4.
!
!-    SPICELIB Version 3.0.0, 16-AUG-2002 (NJB)
!
!        Added support for type 18. This routine now uses the
!        include file spkrec.inc to declare the record size.
!
!        Corrected header comments giving record sizes for types
!        8, 9, 12, 13.
!
!-    SPICELIB Version 2.0.0, 06-NOV-1999 (NJB)
!
!        Added support for types 12 and 13.
!
!-    SPICELIB Version 1.1.0, 07-JAN-1997 (WLT)
!
!        Added support for type 17.
!
!-    SPICELIB Version 1.0.0, 19-SEP-1995 (WLT)
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
      logical               failed

!
!     Some local space is needed in which to return records, and
!     into which to unpack the segment descriptor.
!


      double precision      record   ( maxrec )

      double precision      dc       (      2 )
      integer               ic       (      6 )
      integer               recsiz
      integer               type

!
!     Local Parameters
!
      integer               nstate
      parameter           ( nstate = 6 )

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'SPKPVN' )
      end if

!
!     Unpacking the segment descriptor will tell us the center,
!     reference frame, and data type for this segment.
!
      call dafus ( descr, 2, 6, dc, ic )

      center = ic(2)
      ref    = ic(3)
      type   = ic(4)

!
!     Each data type has a pair of routines to read and evaluate
!     records for that data type. These routines are the only ones
!     that actually look inside the segments.
!
!     By the time we have more than 100 data types, we should be
!     allowed to use longer variable names.
!
      if ( type .eq. 01 ) then
         call spkr01 ( handle, descr, et, record         )
         call spke01 (                et, record, state  )

      else if ( type .eq. 02 ) then
         call spkr02 ( handle, descr, et, record         )
         call spke02 (                et, record, state  )

      else if ( type .eq. 03 ) then
         call spkr03 ( handle, descr, et, record         )
         call spke03 (                et, record, state  )


!
!     Type 04 is not officially part of the library.
!
!     ELSE IF ( TYPE .EQ. 04 ) THEN
!         CALL SPKR04 ( HANDLE, DESCR, ET, RECORD         )
!         CALL SPKE04 (                ET, RECORD, STATE  )

      else if ( type .eq. 05 ) then
         call spkr05 ( handle, descr, et, record         )
         call spke05 (                et, record, state  )

      else if ( type .eq. 08 ) then
         call spkr08 ( handle, descr, et, record         )
         call spke08 (                et, record, state  )

      else if ( type .eq. 09 ) then
         call spkr09 ( handle, descr, et, record         )
         call spke09 (                et, record, state  )

      else if ( type .eq. 10 ) then
         call spkr10 ( handle, descr, et, record         )
         call spke10 (                et, record, state  )

      else if ( type .eq. 12 ) then
         call spkr12 ( handle, descr, et, record         )
         call spke12 (                et, record, state  )

      else if ( type .eq. 13 ) then
         call spkr13 ( handle, descr, et, record         )
         call spke13 (                et, record, state  )

      else if ( type .eq. 14 ) then
!
!        Fetch the number of Chebyshev coefficients, compute the record
!        size needed, and signal an error if there is not enough storage
!        in RECORD. The number of coefficients is the first constant
!        value in the generic segment.
!
         call sgfcon ( handle, descr, 1, 1, record(1) )

         if ( failed() ) then
            call chkout ( 'SPKPVN' )
            return
         end if

         recsiz = nstate * int(record(1)) + 3

         if ( recsiz .gt. maxrec ) then

            call setmsg ( 'Storage for # double precision numbers' &
            //            ' is needed for an SPK data record and' &
            //            ' only # locations were available. Update' &
            //            ' the parameter MAXREC in the subroutine' &
            //            ' SPKPVN and notify the NAIF group of this' &
            //            ' problem.'                                )
            call errint ( '#', recsiz                                )
            call errint ( '#', maxrec                                )
            call sigerr ( 'SPICE(SPKRECTOOLARGE)'                    )
            call chkout ( 'SPKPVN'                                    )
            return

         end if

         call spkr14 ( handle, descr, et, record         )
         call spke14 (                et, record, state  )

      else if ( type .eq. 15 ) then
         call spkr15 ( handle, descr, et, record         )
         call spke15 (                et, record, state  )

      else if ( type .eq. 17 ) then
         call spkr17 ( handle, descr, et, record         )
         call spke17 (                et, record, state  )

      else if ( type .eq. 18 ) then
         call spkr18 ( handle, descr, et, record         )
         call spke18 (                et, record, state  )

      else if ( type .eq. 19) then
         call spkr19 ( handle, descr, et, record         )
         call spke19 (                et, record, state  )

      else if ( type .eq. 20) then
         call spkr20 ( handle, descr, et, record         )
         call spke20 (                et, record, state  )

      else if ( type .eq. 21) then
         call spkr21 ( handle, descr, et, record         )
         call spke21 (                et, record, state  )

      else
         call setmsg ( 'SPK type # is not supported in your ' &
         //            'version of the SPICE library.  You will ' &
         //            'need to upgrade your version of the ' &
         //            'library to make use of ephemerides that ' &
         //            'contain this SPK data type. ' )
         call errint ( '#', type               )
         call sigerr ( 'SPICE(SPKTYPENOTSUPP)' )
         call chkout ( 'SPKPVN'                 )
         return
      end if


      call chkout ( 'SPKPVN' )
      return
      end
