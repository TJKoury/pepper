!$Procedure SPKE10 ( Evaluate SPK record, type 10 )

      SUBROUTINE SPKE10 ( ET, RECORD, STATE )

! $ Abstract
!
!     Evaluate a single SPK data record from a segment of type 10
!     (NORAD two-line element sets.). This evaluator uses algorithms
!     as described in Vallado 2006 [4].
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
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

      IMPLICIT NONE

      INCLUDE 'zzsgp4.inc'

      DOUBLE PRECISION      ET
      DOUBLE PRECISION      RECORD   ( * )
      DOUBLE PRECISION      STATE    ( 6 )

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

      DOUBLE PRECISION      PI
      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     The nutation in obliquity and longitude as well as their rates
!     follow the elements.  So we've got four angles/angle rates
!     following the elements
!
      INTEGER               NANGS
      PARAMETER           ( NANGS = 4 )

!
!     The locations of the epochs and the starts of the element
!     sets are given below.
!
      INTEGER               TIME1
      PARAMETER           ( TIME1  = NGEO + KEPOCH )

      INTEGER               TIME2
      PARAMETER           ( TIME2  = NELEMS + NANGS + TIME1  )

      INTEGER               MODEL1
      PARAMETER           ( MODEL1 = NGEO + 1      )

      INTEGER               ANGS1
      PARAMETER           ( ANGS1 = MODEL1 + NELEMS )

      INTEGER               MODEL2
      PARAMETER           ( MODEL2 = ANGS1 + NANGS )


!
!     Local variables
!
      DOUBLE PRECISION      J2TM     ( 6, 6 )
      DOUBLE PRECISION      TM2J     ( 6, 6 )
      DOUBLE PRECISION      S1       ( 6 )
      DOUBLE PRECISION      S2       ( 6 )
      DOUBLE PRECISION      TMPSTA   ( 6 )
      DOUBLE PRECISION      VCOMP    ( 3 )
      DOUBLE PRECISION      ARG
      DOUBLE PRECISION      DARGDT
      DOUBLE PRECISION      DENOM
      DOUBLE PRECISION      DWDT
      DOUBLE PRECISION      MYPI
      DOUBLE PRECISION      NUMER
      DOUBLE PRECISION      T1
      DOUBLE PRECISION      T2
      DOUBLE PRECISION      W

      LOGICAL               FIRST
      SAVE

      DATA                  FIRST /.TRUE./

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPKE10' )
      END IF

      IF ( FIRST ) THEN
         FIRST  = .FALSE.
         MYPI   = PI()
      END IF

!
!     Fetch the two epochs stored in the record.
!
      T1 = RECORD(TIME1)
      T2 = RECORD(TIME2)

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

      IF ( T1 .NE. T2 ) THEN

!
!        Initialize then propagate.
!
!        XXSGP4E returns on entry if XXSGP4I signals an error.
!

!
!        Evaluate first TLE.
!
         CALL XXSGP4I ( RECORD(1), RECORD(MODEL1), AFSPC )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SPKE10' )
            RETURN
         END IF

!
!        Time from epoch of set 1 in minutes.
!
         CALL XXSGP4E ( (ET - T1)/60.D0, S1 )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SPKE10' )
            RETURN
         END IF

!
!        Evaluate second TLE.
!
         CALL XXSGP4I ( RECORD(1), RECORD(MODEL2), AFSPC )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SPKE10' )
            RETURN
         END IF

!
!        Time from epoch of set 2 in minutes.
!
         CALL XXSGP4E ( (ET - T2)/60.D0, S2 )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SPKE10' )
            RETURN
         END IF

!
!        Compute the weighting function that we'll need later
!        when we combine states 1 and 2.
!
         NUMER  = ET - T1
         DENOM  = T2 - T1
         ARG    = NUMER*MYPI/DENOM
         DARGDT =       MYPI/DENOM

         W      =  0.5D0 + 0.5D0 * DCOS( ARG )
         DWDT   =        - 0.5D0 * DSIN( ARG ) * DARGDT

!
!        Now compute the weighted average of the two states.
!
         CALL VLCOMG ( 6,  W,    S1,       1.D0-W, S2,     STATE     )
         CALL VLCOM  (     DWDT, S1,       -DWDT,  S2,     VCOMP     )
         CALL VADD   (           STATE(4),         VCOMP,  TMPSTA(4) )
         CALL VEQU   (           TMPSTA(4),                STATE(4)  )

      ELSE

!
!        Evaluate the TLE.
!
         CALL XXSGP4I ( RECORD(1), RECORD(MODEL1), AFSPC )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SPKE10' )
            RETURN
         END IF

!
!        Time from epoch of set 1 in minutes.
!
         CALL XXSGP4E ( (ET - T1)/60.D0, STATE )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SPKE10' )
            RETURN
         END IF

      END IF

!
!     Finally, convert the TEME state to J2000.  First
!     calculate the mapping from J2000 to TEME (J2TM), and from
!     TEME to J2000 (TM2J) at time ET...
!
!                         -1
!     Note that J2TM = TM2J
!
      CALL ZZTEME( ET, J2TM, TM2J )

!
!     ...now convert the TEME state to a J2000 state.
!
      CALL MXVG   ( TM2J, STATE, 6, 6, TMPSTA )
      CALL MOVED  ( TMPSTA,  6,      STATE     )

      CALL CHKOUT ( 'SPKE10' )
      RETURN

      END
