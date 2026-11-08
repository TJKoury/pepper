!$Procedure DELTET ( Delta ET, ET - UTC )

      SUBROUTINE DELTET ( EPOCH, EPTYPE, DELTA )

! $ Abstract
!
!     Return the value of Delta ET (ET-UTC) for an input epoch.
!
! $ Required_Reading
!
!     TIME
!     KERNEL
!
! $ Keywords
!
!     TIME
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION EPOCH
      CHARACTER*(*)    EPTYPE
      DOUBLE PRECISION DELTA

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     EPOCH      I   Input epoch (seconds past J2000).
!     EPTYPE     I   Type of input epoch ('UTC' or 'ET').
!     DELTA      O   Delta ET (ET-UTC) at input epoch.
!
! $ Detailed_Input
!
!     EPOCH    is the epoch at which "delta ET" is to be computed.
!              EPOCH may be either UTC or ephemeris seconds past
!              J2000, as specified by EPTYPE.
!
!     EPTYPE   is the type of input epoch. It may be either
!              of the following:
!
!                 'UTC'    UTC seconds past J2000 UTC.
!
!                 'ET'     Ephemeris seconds past J2000 TDB,
!                          also known as barycentric dynamical
!                          time (TDB).
!
! $ Detailed_Output
!
!     DELTA    is the value of
!
!                 "delta ET" = ET - UTC
!
!              at the input epoch. This is added to UTC to give
!              ET, or subtracted from ET to give UTC. The routine
!              is reversible: that is, given the following calls,
!
!                 CALL DELTET ( UTC,      'UTC', DEL1 )
!                 CALL DELTET ( UTC+DEL1, 'ET',  DEL2 )
!
!              the expression
!
!                 ( DEL1 .EQ. DEL2 )
!
!              is always .TRUE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input epoch is not recognized, the error
!         SPICE(INVALIDEPOCH) is signaled.
!
!     2)  If the variables necessary for the computation of DELTA
!         have not been loaded into the kernel pool, the error
!         SPICE(KERNELVARNOTFOUND) is signaled.
!
!     3)  If the number of leapseconds in the pool is greater than
!         the local leapseconds buffer size, the error
!         SPICE(BUFFEROVERFLOW) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The constants necessary for computing the offset are taken
!     from the kernel pool, where they are assumed to have been
!     loaded from a kernel file.
!
!     The tables are consulted to determine the number of leap seconds
!     preceding the input epoch. Also, an approximation to the periodic
!     yearly variation (which has an amplitude of just under two
!     milliseconds) in the difference between ET and TAI (Atomic Time)
!     is computed. The final value of Delta ET is given by
!
!        Delta ET = ( ET - TAI ) + leap seconds
!
! $ Examples
!
!     The following example shows how DELTET may be used to convert
!     from UTC seconds past J2000 to ephemeris seconds past J2000.
!
!        CALL DELTET ( UTCSEC, 'UTC', DELTA )
!        ET = UTCSEC + DELTA
!
!     The following example shows how DELTET may be used to convert
!     from ephemeris seconds past J2000 to UTC seconds past J2000.
!
!        CALL DELTET ( ET, 'ET', DELTA )
!        UTCSEC = ET - DELTA
!
!     See the Time required reading time.req for further examples.
!
! $ Restrictions
!
!     1)  The routines STR2ET and ET2UTC are preferred for conversions
!         between UTC and ET. This routine is provided mainly as a
!         utility for STR2ET and ET2UTC.
!
!     2)  A leapseconds kernel containing leapseconds and relativistic
!         terms MUST be loaded prior to calling this subroutine.
!         Examples demonstrating how to load a kernel pool are included
!         in the Required Reading file time.req and in the $Examples
!         section of this header. For more general information about
!         kernel pools, please consult the Required Reading file
!         kernel.req.
!
! $ Literature_References
!
!     [1]  "The Astronomical Almanac for the Year 1990," United States
!          Naval Observatory, U.S. Government Printing Office,
!          Washington, D.C., 1989.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     W.M. Owen          (JPL)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.3.0, 24-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary entries in $Revisions section.
!
!        Replaced reference to UTC2ET with STR2ET in $Restrictions
!        section.
!
!-    SPICELIB Version 1.2.2, 18-APR-2014 (BVS)
!
!        Minor header edits.
!
!-    SPICELIB Version 1.2.1, 18-MAY-2010 (BVS)
!
!        Removed "C$" marker from text in the header.
!
!-    SPICELIB Version 1.2.0, 24-AUG-1998 (WLT)
!
!        The previous upgrade introduced an error in the fetch
!        of the variable DELTET/M from the kernel pool. This
!        error was corrected.
!
!-    SPICELIB Version 1.1.0, 20-APR-1998 (NJB)
!
!        Calls to RTPOOL were replaced with calls to GDPOOL, which
!        does more robust error checking. Check for buffer overflow
!        was added. Local declarations were re-organized.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) (IMU)
!
!-&


! $ Index_Entries
!
!     difference between ephemeris time and utc
!
!-&


! $ Revisions
!
!-    Beta Version 1.1.0, 06-OCT-1988 (IMU)
!
!        Tim Colvin of Rand noticed that times returned by UTC2ET
!        and TPARSE differed by one second. Upon closer
!        inspection, crack NAIF staff members deduced that in fact
!        Mr. Colvin had not loaded the kernel pool, and were
!        surprised to learn that no error had occurred.
!
!        Multiple FOUND flags and a bevy of new error messages
!        were implemented to cope with this unfortunate oversight.
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               NMAX
      PARAMETER           ( NMAX = 200 )

!
!     Local variables
!
      CHARACTER*20          MISSED   ( 5 )
      CHARACTER*4           TYPE
      CHARACTER*1           DTYPE

      DOUBLE PRECISION      AET
      DOUBLE PRECISION      DLEAP  ( 2,NMAX )
      DOUBLE PRECISION      DTA
      DOUBLE PRECISION      EA
      DOUBLE PRECISION      EB
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ETTAI
      DOUBLE PRECISION      K
      DOUBLE PRECISION      LEAPS
      DOUBLE PRECISION      M    ( 0:1 )
      DOUBLE PRECISION      MA

      INTEGER               I
      INTEGER               N
      INTEGER               NLEAP

      LOGICAL               FOUND    ( 5 )

!
!     Saved variables
!
      SAVE                  MISSED

!
!     Initial values
!
      DATA                  MISSED   / 'DELTET/DELTA_T_A, #', &
                                       'DELTET/K, #', &
                                       'DELTET/EB, #', &
                                       'DELTET/M, #', &
                                       'DELTET/DELTA_AT, #'    /



!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DELTET' )
      END IF

!
!     Convert the epoch type to uppercase, to simplify comparisons.
!
      CALL UCASE ( EPTYPE, TYPE )

!
!     Extract the necessary constants from the kernel pool.
!     Leap seconds and their epochs are interleaved in DELTA_AT.
!
!     DLEAP(1,i) is the number of leap seconds at DLEAP(2,i) UTC
!     seconds past J2000.
!
      CALL GDPOOL ( 'DELTET/DELTA_T_A', 1, 1, N, DTA, FOUND(1) )
      CALL GDPOOL ( 'DELTET/K',         1, 1, N, K,   FOUND(2) )
      CALL GDPOOL ( 'DELTET/EB',        1, 1, N, EB,  FOUND(3) )
      CALL GDPOOL ( 'DELTET/M',         1, 2, N, M,   FOUND(4) )

!
!     Check that the number of leapseconds is not too great for our
!     buffer size (not likely).
!
      CALL DTPOOL ( 'DELTET/DELTA_AT', FOUND(5), NLEAP, DTYPE )

      IF ( NLEAP .GT. (2*NMAX) ) THEN

         CALL SETMSG ( 'Number of leapseconds, #, is greater than ' // &
                       'the number that can be buffered, #.'         )
         CALL ERRINT ( '#',  NLEAP/2                                 )
         CALL ERRINT ( '#',  NMAX                                    )
         CALL SIGERR ( 'SPICE(BUFFERTOOSMALL)'                       )
         CALL CHKOUT ( 'DELTET'                                      )
         RETURN

      END IF

      CALL GDPOOL ( 'DELTET/DELTA_AT',  1,            2*NMAX, &
                     NLEAP,             DLEAP(1,1),   FOUND(5) )

      NLEAP = NLEAP / 2

      IF ( .NOT. (       FOUND(1) &
                   .AND. FOUND(2) &
                   .AND. FOUND(3) &
                   .AND. FOUND(4) &
                   .AND. FOUND(5) ) ) THEN

         CALL SETMSG ( 'The following, needed to compute '      // &
                       'Delta ET (ET - UTC), could not be '     // &
                       'found in the kernel pool: #'               )

         DO I = 1, 5
            IF ( .NOT. FOUND(I) ) THEN
               CALL ERRCH ( '#', MISSED(I) )
            END IF
         END DO

         CALL ERRCH  ( ', #', '.' )
         CALL SIGERR ( 'SPICE(KERNELVARNOTFOUND)' )

         CALL CHKOUT ( 'DELTET' )
         RETURN

      END IF



!
!     There are two separate quantities to be determined. First,
!     the appropriate number of leap seconds. Second, the size of
!     the periodic term ET-TAI.
!

!
!     For epochs before the first leap second, return Delta ET at
!     the epoch of the leap second minus one second.
!
      LEAPS = DLEAP(1,1) - 1

!
!     When counting leap seconds for UTC epochs, we can compare
!     directly against the values in DLEAP.
!
      IF ( TYPE .EQ. 'UTC' ) THEN

         DO I = 1, NLEAP
            IF ( EPOCH .GE. DLEAP(2,I) ) THEN
               LEAPS = DLEAP(1,I)
            END IF
         END DO

!
!     For ET epochs, things are a little tougher. In order to compare
!     the input epoch against the epochs of the leap seconds, we need
!     to compute ET-TAI at each of the leap epochs. To make sure that
!     the computation is reversible, it is always done at the nearest
!     ET second (the "approximate ET", or AET).
!
!     There must be a hundred ways to do this more efficiently.
!     For now, we'll settle for one that works.
!
      ELSE IF ( TYPE .EQ. 'ET' ) THEN

         DO I = 1, NLEAP
            IF ( EPOCH .GT. DLEAP(2,I) ) THEN

               AET   = DNINT ( DLEAP(2,I) + DTA + DLEAP(1,I) )

               MA    = M(0) + M(1) * AET
               EA    = MA   + EB   * SIN ( MA )
               ETTAI =        K    * SIN ( EA )

               ET    = DLEAP(2,I) + DTA + DLEAP(1,I) + ETTAI

               IF ( EPOCH .GE. ET ) THEN
                  LEAPS = DLEAP(1,I)
               END IF

            END IF
         END DO

!
!     Uh, those are the only choices.
!
      ELSE

         CALL SETMSG ( 'Epoch type was #' )
         CALL ERRCH  ( '#', TYPE )
         CALL SIGERR ( 'SPICE(INVALIDEPOCH)' )

         CALL CHKOUT ( 'DELTET' )
         RETURN

      END IF


!
!     Add the constant offset, leap seconds, and the relativistic term
!     (as before, computed at the nearest ET second).
!
      IF ( TYPE .EQ. 'ET' ) THEN
         AET = DNINT ( EPOCH )

      ELSE IF ( TYPE .EQ. 'UTC' ) THEN
         AET = DNINT ( EPOCH + DTA + LEAPS )
      END IF

      MA    = M(0) + M(1) * AET
      EA    = MA   + EB   * SIN ( MA )
      ETTAI =        K    * SIN ( EA )

      DELTA = DTA + LEAPS + ETTAI

      CALL CHKOUT ( 'DELTET' )
      RETURN
      END
