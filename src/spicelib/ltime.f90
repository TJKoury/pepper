!$Procedure LTIME ( Light Time )

      SUBROUTINE LTIME ( ETOBS, OBS, DIR, TARG, ETTARG, ELAPSD )

! $ Abstract
!
!     Compute the transmission (or reception) time of a signal at a
!     specified target, given the reception (or transmission) time at a
!     specified observer. Also return the elapsed time between
!     transmission and reception.
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
!     None.
!
! $ Keywords
!
!     SPK
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      ETOBS
      INTEGER               OBS
      CHARACTER*(2)         DIR
      INTEGER               TARG
      DOUBLE PRECISION      ETTARG
      DOUBLE PRECISION      ELAPSD

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ETOBS      I   Epoch of a signal at some observer.
!     OBS        I   NAIF ID of some observer.
!     DIR        I   Direction the signal travels ( '->' or '<-' ).
!     TARG       I   NAIF ID of the target object.
!     ETTARG     O   Epoch of the signal at the target.
!     ELAPSD     O   Time between transmit and receipt of the signal.
!
! $ Detailed_Input
!
!     ETOBS    is an epoch expressed as ephemeris seconds past J2000
!              TDB. This is the time at which an electromagnetic signal
!              is "at" the observer.
!
!     OBS      is the NAIF ID of some observer.
!
!     DIR      is the direction the signal travels. The
!              acceptable values are '->' and '<-'. When
!              you read the calling sequence from left to
!              right, the "arrow" given by DIR indicates
!              which way the electromagnetic signal is traveling.
!
!              If the argument list reads as below,
!
!                 ..., OBS, '->', TARG, ...
!
!              the signal is traveling from the observer to the
!              target.
!
!              If the argument reads as
!
!                 ..., OBS, '<-', TARG
!
!              the signal is traveling from the target to
!              the observer.
!
!     TARG     is the NAIF ID of the target.
!
! $ Detailed_Output
!
!     ETTARG   is the epoch expressed as ephemeris seconds past J2000
!              TDB at which the electromagnetic signal is "at" the
!              target body.
!
!              Note ETTARG is computed using only Newtonian
!              assumptions about the propagation of light.
!
!     ELAPSD   is the number of ephemeris seconds (TDB) between
!              transmission and receipt of the signal.
!
!              ELAPSD is computed as:
!
!                 ELAPSD = DABS( ETOBS - ETTARG )
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If DIR is not one of '->' or '<-', the error
!         SPICE(BADDIRECTION) is signaled. In this case
!         ETTARG and ELAPSD will not be altered from their
!         input values.
!
!     2)  If insufficient ephemeris information is available to
!         compute the outputs ETTARG and ELAPSD, or if observer
!         or target is not recognized, an error is signaled
!         by a routine in the call tree of this routine.
!
!         In this case, the value of ETTARG will be set to ETOBS
!         and ELAPSD will be set to zero.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Suppose a radio signal travels between two solar system
!     objects. Given an ephemeris for the two objects, which way
!     the signal is traveling, and the time when the signal is
!     "at" at one of the objects (the observer OBS), this routine
!     determines when the signal is "at" the other object (the
!     target TARG). It also returns the elapsed time between
!     transmission and receipt of the signal.
!
! $ Examples
!
!     Example 1.
!     ----------
!     Suppose a signal is transmitted at time ET from the Goldstone
!     tracking site (id-code 399001) to a spacecraft whose id-code
!     is -77.
!
!
!           signal traveling to spacecraft
!       *  -._.-._.-._.-._.-._.-._.-._.-._.->  *
!
!       Goldstone (OBS=399001)            Spacecraft (TARG = -77)
!       at epoch ETOBS(given)             at epoch ETTARG(unknown)
!
!     Assuming that all of the required SPICE kernels have been
!     loaded, the code fragment below shows how to compute the
!     time (ARRIVE) at which the signal arrives at the spacecraft
!     and how long (HOWLNG) it took the signal to reach the spacecraft.
!     (Note that we display the arrival time as the number of seconds
!     past J2000.)
!
!        OBS   = 399001
!        TARG  = -77
!        ETOBS = ET
!
!        CALL LTIME ( ETOBS, OBS, '->', TARG, ARRIVE, HOWLNG )
!        CALL ETCAL
!
!        WRITE (*,*) 'The signal arrived at time: ', ARRIVE
!        WRITE (*,*) 'It took ', HOWLNG, ' seconds to get there.'
!
!
!     Example 2.
!     ----------
!     Suppose a signal is received at the Goldstone tracking sight
!     at epoch ET from the spacecraft of the previous example.
!
!               signal sent from spacecraft
!         *  <-._.-._.-._.-._.-._.-._.-._.-._.- *
!
!       Goldstone (OBS=399001)               Spacecraft (TARG = -77)
!       at epoch ETOBS(given)                at epoch ETTARG(unknown)
!
!     Again assuming that all the required kernels have been loaded
!     the code fragment below computes the epoch at which the
!     signal was transmitted from the spacecraft.
!
!        OBS   = 399001
!        TARG  = -77
!        ETOBS = ET
!
!        CALL LTIME ( ETOBS, OBS, '<-', TARG, SENT, HOWLNG )
!        CALL ETCAL
!
!        WRITE (*,*) 'The signal was transmitted at: ', SENT
!        WRITE (*,*) 'It took ', HOWLNG, ' seconds to get here.'
!
!     EXAMPLE 3
!     ---------
!     Suppose there is a transponder on board the spacecraft of
!     the previous examples that transmits a signal back to the
!     sender exactly 1 microsecond after a signal arrives at
!     the spacecraft. If we send a signal from Goldstone
!     to the spacecraft and wait to receive it at Canberra.
!     What will be the epoch at which the return signal arrives
!     in Canberra? ( The id-code for Canberra is 399002 ).
!
!     Again, assuming we've loaded all the necessary kernels,
!     the fragment below will give us the answer.
!
!        GSTONE = 399001
!        SC     = -77
!        CANBER = 399002
!        ETGOLD = ET
!
!        CALL LTIME ( ETGOLD, GSTONE, '->', SC, SCGET, LT1 )
!
!     Account for the microsecond delay between receipt and transmit
!
!        SCSEND = SCGET + 0.000001
!
!        CALL LTIME ( SCSEND, SC, '->', CANBER, ETCANB, LT2 )
!
!        RNDTRP = ETCANB - ETGOLD
!
!        WRITE (*,*) 'The  signal arrives in Canberra at: ', ETCANB
!        WRITE (*,*) 'Round trip time for the signal was: ', RNDTRP
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
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.3, 26-OCT-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.2, 22-SEP-2004 (EDW)
!
!        Placed Copyright after $Abstract.
!
!-    SPICELIB Version 1.1.1, 18-NOV-1996 (WLT)
!
!        Errors in the $Examples section were corrected.
!
!-    SPICELIB Version 1.1.0, 10-JUL-1996 (WLT)
!
!        Added Copyright Notice to the header.
!
!-    SPICELIB Version 1.0.0, 10-NOV-1995 (WLT)
!
!-&


! $ Index_Entries
!
!     Compute uplink and downlink light time
!
!-&


!
!     SPICELIB Functions
!
      DOUBLE PRECISION      CLIGHT
      DOUBLE PRECISION      VDIST
      INTEGER               RTRIM
      LOGICAL               FAILED
      LOGICAL               RETURN
!
!     Local Variables
!
      DOUBLE PRECISION      C
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      MYET
      DOUBLE PRECISION      SOBS  ( 6 )
      DOUBLE PRECISION      STARG ( 6 )

      INTEGER               BCENTR
      INTEGER               R

      IF ( RETURN() )THEN
         RETURN
      END IF

      CALL CHKIN ( 'LTIME' )
!
!     First perform the obvious error check.
!
      IF ( DIR .NE. '->' .AND. DIR .NE. '<-' ) THEN

         CALL SETMSG ( 'The direction specifier for the signal ' &
         //            'was ''#''  it must be either ''->'' or ' &
         //            '''<-''. '            )
         R =  RTRIM  (      DIR              )
         CALL ERRCH  ( '#', DIR(1:R)         )
         CALL SIGERR ( 'SPICE(BADDIRECTION)' )
         CALL CHKOUT ( 'LTIME'               )
         RETURN

      END IF

!
!     We need two constants, the speed of light and the id-code
!     for the solar system barycenter.
!
      C      = CLIGHT ()
      BCENTR = 0
      MYET   = ETOBS

!
!     First get the barycenter relative states of the observer
!     and target.
!
      CALL SPKGEO ( OBS,  MYET, 'J2000', BCENTR, SOBS,  LT )
      CALL SPKGEO ( TARG, MYET, 'J2000', BCENTR, STARG, LT )

      ELAPSD = VDIST( SOBS, STARG ) / C

!
!     The rest is straight forward.  We either add the elapsed
!     time to get the next state or subtract the elapsed time.
!     This depends on whether we are receiving or transmitting
!     at the observer.
!
!     Note that 3 iterations as performed here gives us
!     Newtonian accuracy to the nanosecond level for all
!     known objects in the solar system.  The ephemeris
!     is certain to be much worse than this.
!
      IF ( DIR .EQ. '->' ) THEN

         ETTARG = MYET + ELAPSD

         CALL SPKGEO ( TARG, ETTARG, 'J2000', BCENTR, STARG, LT )

         ELAPSD = VDIST( SOBS, STARG ) / C
         ETTARG = MYET + ELAPSD

         CALL SPKGEO ( TARG, ETTARG, 'J2000', BCENTR, STARG, LT )

         ELAPSD = VDIST( SOBS, STARG ) / C
         ETTARG = MYET + ELAPSD

         CALL SPKGEO ( TARG, ETTARG, 'J2000', BCENTR, STARG, LT )

         ELAPSD = VDIST( SOBS, STARG ) / C
         ETTARG = MYET + ELAPSD

      ELSE

         ETTARG = MYET - ELAPSD

         CALL SPKGEO ( TARG, ETTARG, 'J2000', BCENTR, STARG, LT )

         ELAPSD = VDIST( SOBS, STARG ) / C
         ETTARG = MYET - ELAPSD

         CALL SPKGEO ( TARG, ETTARG, 'J2000', BCENTR, STARG, LT )

         ELAPSD = VDIST( SOBS, STARG ) / C
         ETTARG = MYET - ELAPSD

         CALL SPKGEO ( TARG, ETTARG, 'J2000', BCENTR, STARG, LT )

         ELAPSD = VDIST( SOBS, STARG ) / C
         ETTARG = MYET - ELAPSD

      END IF

      IF ( FAILED() ) THEN
         ETTARG = MYET
         ELAPSD = 0.0D0
      END IF

      CALL CHKOUT ( 'LTIME' )
      RETURN

      END
