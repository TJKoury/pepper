C$Procedure      T_GETSA ( Test utility, get stellar aberration )
  
      SUBROUTINE T_GETSA ( TARG, ET, REF, ABCORR, OBS, PCORR, DPCORR )
      IMPLICIT NONE

C$ Abstract
C
C     Return the stellar aberration offset vector and the time
C     derivative of this offset for specified SPKEZR inputs.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     SPK
C
C$ Keywords
C
C     TEST
C     UTILITY
C
C$ Declarations

      INTEGER               TARG
      DOUBLE PRECISION      ET
      CHARACTER*(*)         REF
      CHARACTER*(*)         ABCORR
      INTEGER               OBS
      DOUBLE PRECISION      PCORR  ( 3 )
      DOUBLE PRECISION      DPCORR ( 3 )

      INCLUDE 'zzabcorr.inc'
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     TARG       I   Target body ID code.
C     ET         I   Epoch.
C     REF        I   Reference frame name.
C     ABCORR     I   Aberration correction.
C     OBS        I   Observer ID code.
C     PCORR      O   Stellar aberration offset.
C     DPCORR     O   Velocity of stellar aberration offset.
C
C$ Detailed_Input
C
C     TARG           is the NAIF ID code of the target body. 
C
C     ET             is the time at which to evaluate the state vector
C                    with which the specified stellar aberration
C                    correction is associated. ET is expressed as
C                    seconds past J2000 TDB.
C
C     REF            is the reference frame in which to compute a state
C                    and corresponding stellar aberration correction.
C
C                    REF need not be inertial.
C
C     ABCORR         is an aberration correction specification
C                    string. Any value acceptable to SPKEZ
C                    may be used.
C
C     OBS            is the NAIF ID of the observer body.
C
C$ Detailed_Output
C
C     PCORR       is the stellar aberration correction for the position
C                 component of the state defined by the input
C                 arguments. Adding PCORR to this position vector
C                 produces the input observer-target position,
C                 corrected for stellar aberration.
C
C                 Units are km.
C
C     DPCORR      is the stellar aberration correction for the velocity
C                 component of the state defined by the input
C                 arguments. Adding DPCORR to this velocity vector
C                 produces the input observer-target velocity,
C                 corrected for stellar aberration.
C
C                 Units are km/s.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) Any exceptions incurred by ZZPRSCOR may be incurred
C        by this routine.
C
C     2) Any exceptions incurred by SPKLTC may be incurred
C        by this routine.
C
C     3) Any exceptions incurred by ZZSTELAB may be incurred
C        by this routine.
C
C$ Files
C
C     See the Files section of the header of SPKEZ.
C
C$ Particulars
C
C     This routine mimics the aberration correction logic of SPKACS: the
C     acceleration of the observer with respect to the solar system
C     barycenter is computed, an optionally light time corrected state
C     is computed using SPKLTC, then this state and the acceleration of
C     the observer with respect to the solar system barycenter are used
C     as inputs to the stellar aberration correction computation.
C
C$ Examples
C
C     See the test utility package T_TRAJFN for usage
C     examples.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    TESTUTIL Version 1.0.0, 29-DEC-2007 (NJB)
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               FAILED

C
C     Local parameters
C
      INTEGER               SSB
      PARAMETER           ( SSB    = 0 )
      
C
C     Local variables
C
      DOUBLE PRECISION      ACC    ( 3 )
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      DLT
      DOUBLE PRECISION      DLTCTR
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      LTCENT
      DOUBLE PRECISION      LTSIGN
      DOUBLE PRECISION      LTSSB
      DOUBLE PRECISION      SASTAT ( 6 )
      DOUBLE PRECISION      SSBOBS ( 6 )
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      STCTR  ( 6 )
      DOUBLE PRECISION      STOBS  ( 6, 2 )
      DOUBLE PRECISION      T
      DOUBLE PRECISION      XFORM  ( 6, 6 )
      DOUBLE PRECISION      XSTATE ( 6 )

      INTEGER               CENTER
      INTEGER               I
      INTEGER               REQFRM
      INTEGER               TYPE
      INTEGER               TYPEID

      LOGICAL               ATTBLK ( ABATSZ )
      LOGICAL               FOUND
      LOGICAL               XMIT

      
      CALL CHKIN ( 'T_GETSA' )

C
C     Compute the corrections in the J2000 frame. We'll
C     transform them later to the requested frame.
C     
      CALL ZZPRSCOR ( ABCORR, ATTBLK )
      XMIT = ATTBLK(XMTIDX)

      DELTA = 1.D0

C
C     Determine the acceleration of the observer with
C     respect to the solar system barycenter at ET.
C     Use a quadratic estimate.
C
      DO I = 1, 2

         T = ET + (2*I - 3)*DELTA

         CALL SPKGEO ( OBS, T, 'J2000', SSB, STOBS(1,I), LTSSB )

      END DO

      CALL QDERIV ( 3, STOBS(4,1), STOBS(4,2), DELTA, ACC )

C
C     Get the light-time corrected state of the target with respect
C     to the observer. Also get the geometric state of the observer
C     with respect to the solar system barycenter at ET.
C
      CALL SPKSSB ( OBS, ET,    'J2000', SSBOBS )

      CALL SPKLTC ( TARG,    ET,   'J2000', ABCORR, 
     .              SSBOBS,  STATE, LT,     DLT    )

C
C     Get the stellar aberration correction and its time derivative.
C     Note that the input observer state relative to the solar system
C     barycenter was obtained from SPKLTC.
C
      CALL ZZSTELAB ( XMIT, ACC, SSBOBS(4), STATE, PCORR, DPCORR )

C
C     Get the transformation from J2000 to the request frame. This
C     transformation should be evaluated at the light-time corrected
C     epoch associated with the center of the request frame.
C
      CALL NAMFRM ( REF, REQFRM )
 
      IF ( REQFRM .EQ. 0 ) THEN

         CALL SETMSG ( 'The requested output frame ''#'' is '
     .   //            'not recognized by the reference frame '
     .   //            'subsystem.  Please check that the '
     .   //            'appropriate kernels have been loaded '
     .   //            'and that you have correctly entered '
     .   //            'the name of the output frame. '       )
         CALL ERRCH  ( '#', REF                               )
         CALL SIGERR ( 'SPICE(UNKNOWNFRAME)'                  )
         CALL CHKOUT ( 'T_GETSA'                              )
         RETURN
 
      END IF

C
C     If we reach this point, FOUND will be .TRUE. after
C     the following call to FRINFO:
C
      CALL FRINFO ( REQFRM, CENTER, TYPE, TYPEID, FOUND )

C
C     Get light time to the frame's center.
C
      IF ( CENTER .EQ. OBS ) THEN

         LTCENT = 0.0D0
         DLTCTR = 0.0D0

      ELSE IF ( CENTER .EQ. TARG ) THEN

         LTCENT = LT
         DLTCTR = DLT

      ELSE

         CALL SPKLTC ( CENTER, ET,    'J2000', ABCORR,
     .                 SSBOBS, STCTR, LTCENT,  DLTCTR )
      END IF
C
C        If something went wrong (like we couldn't get the state of
C        the center relative to the observer) now it is time to quit.
C
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'T_GETSA' )
         RETURN
      END IF

C
C     If the aberration corrections are for transmission, make the sign
C     of the light time positive, since we wish to compute the
C     orientation of the non-inertial frame at an epoch later than ET
C     by the one-way light time.
C
      IF ( XMIT ) THEN

         LTSIGN =  1
      ELSE
         LTSIGN = -1
      END IF

C
C     Look up the transformation from J2000 to REF at the light time
C     corrected epoch.
C
      CALL SXFORM ( 'J2000', REF, ET + LTSIGN*LTCENT, XFORM )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'T_GETSA' )
         RETURN
      END IF

C
C     The frame rotation we wish to apply is of the form
C
C        R(t)
C
C     where
C
C        t = ET + LTSIGN*LTCENT(ET)
C
C     since the light time from the frame center to the
C     observer LTCENT is a function of ET. So
C
C                              |
C        d(R)/d(ET) = d(R)/d(t)|                  * (1 + d(t)/d(ET))
C                              |t=ET+LTSIGN*LTCENT
C
C
C                              |
C                   = d(R)/d(t)|                  * (1 + LTSIGN*DLTCTR)
C                              |t=ET+LTSIGN*LTCENT
C
C     Scale the derivative block of the transformation to account
C     for the rate of change of light time.
C
      DO I = 1, 3
         CALL VSCLIP ( 1.D0 + LTSIGN*DLTCTR, XFORM(4,I) )
      END DO

C
C     Now apply the transformation to the stellar aberration
C     correction position and velocity. We'll pack these 
C     components together as a state to simplify the multiplication.
C 
      CALL VEQU ( PCORR,  SASTAT    )
      CALL VEQU ( DPCORR, SASTAT(4) )

      CALL MXVG ( XFORM, SASTAT, 6, 6, XSTATE )

C
C     Now unpack the stellar aberration correction components.
C
      CALL VEQU ( XSTATE,    PCORR  )
      CALL VEQU ( XSTATE(4), DPCORR )

      CALL CHKOUT ( 'T_GETSA' )
      RETURN
      END




