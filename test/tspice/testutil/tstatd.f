C$Procedure      TSTATD ( Test Attitude )
 
      SUBROUTINE TSTATD ( ET, MATRIX, ANGVEL )
 
C$ Abstract
C
C     This routine produces attitude and angular velocity values
C     that should duplicate the values for the test spacecraft
C     with id-code -10001.
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
C      None.
C
C$ Keywords
C
C       TESTING
C
C$ Declarations
 
      IMPLICIT NONE
 
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      MATRIX ( 3, 3 )
      DOUBLE PRECISION      ANGVEL ( 3    )
 
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      ET         I   An ephemeris epoch in seconds past J2000
C      MATRIX     O   A rotation from J2000 to the frame of -10001
C      ANGVEL     O   The angular velocity of the rotation.
C
C$ Detailed_Input
C
C     ET          an epoch given in terms of TDB seconds past the
C                 epoch of J2000.
C
C$ Detailed_Output
C
C     MATRIX      is the expected orientation of the test body -10001
C                 relative to the J2000 frame.
C
C     ANGVEL      is the expected angular velocity of the test body
C                 -10001 relative to the J2000 frame.
C
C$ Parameters
C
C      None.
C
C$ Files
C
C      None.
C
C$ Exceptions
C
C     Error free.
C
C$ Particulars
C
C     This routine creates a model for the attitude and angular
C     velocity of the fictitious body -10001  used in testing SPICE
C     SPK, CK, SCLK, and IK systems.
C
C     The attitude is perfectly aligned with the Galactic axes at the
C     epoch of J2000.
C
C     The body rotates at a constant rate of 1 radian every 10 million
C     seconds.  Every 100 million seconds the axis of rotation changes.
C
C     The axes of rotation are:
C
C     From           To                Axis of rotation    Time Interval
C     ------------  ------------       ----------------    -------------
C     -Infinity     -900,000,000       ( 1, 2, 4 )            1
C     -900,000,000  -800,000,000       ( 2, 1, 4 )            2
C     -800,000,000  -700,000,000       ( 4, 1, 2 )            3
C
C     -700,000,000  -600,000,000       ( 4, 2, 1 )            4
C     -600,000,000  -500,000,000       ( 2, 1, 4 )            5
C     -500,000,000  -400,000,000       ( 1, 4, 2 )            6
C
C     -400,000,000  -300,000,000       ( 1, 2, 3 )            7
C     -300,000,000  -200,000,000       ( 2, 3, 1 )            8
C     -200,000,000  -100,000,000       ( 3, 1, 2 )            9
C
C     -100,000,000   000,000,000       ( 3, 2, 1 )            10
C      000,000,000   100,000,000       ( 2, 1, 3 )            11
C      100,000,000   200,000,000       ( 1, 3, 2 )            12
C
C      200,000,000   300,000,000       ( 2, 3, 6 )            13
C      300,000,000   400,000,000       ( 3, 6, 2 )            14
C      400,000,000   500,000,000       ( 6, 2, 3 )            15
C
C      500,000,000   600,000,000       ( 6, 3, 2 )            16
C      600,000,000   700,000,000       ( 3, 2, 6 )            17
C      700,000,000   800,000,000       ( 2, 6, 3 )            18
C
C      800,000,000   900,000,000       ( 1, 1, 1 )            19
C      900,000,000   +Infinity         ( 0, 0, 1 )            20
C
C
C$ Examples
C
C     This routine can be used in conjunction with the routine
C     TSTCK3 to perform tests on components of the CK system.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C      W.L. Taber      (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-    Test Utilities Version 1.0.0, 24-JAN-1995 (WLT)
C
C-&
 
C$ Index_Entries
C
C     Attitude of the test body -10001 
C
C-&
 
C
C     Spicelib Functions
C
      INTEGER               LSTLED
 
C
C     Local Variables
C
      INTEGER               I
 
      DOUBLE PRECISION      AXIS   (    3, 20 )
      DOUBLE PRECISION      DTHETA
      DOUBLE PRECISION      REF    ( 3, 3, 20 )
      DOUBLE PRECISION      REFTIM (       20 )
      DOUBLE PRECISION      ROT    ( 3, 3     )
      DOUBLE PRECISION      TEMP   ( 3, 3     )
      DOUBLE PRECISION      TEMPAX (    3     )
 
      LOGICAL               FIRST
      SAVE
 
      DATA                  FIRST / .TRUE. /
 
 
 
      IF ( FIRST ) THEN
 
         FIRST = .FALSE.
C
C        Setup.  Here we create the rotation matrices at the
C        boundaries of the intervals described in the header.
C
 
         AXIS(1, 1) = 1.0D0
         AXIS(2, 1) = 2.0D0
         AXIS(3, 1) = 4.0D0
 
 
         AXIS(1, 2) = 2.0D0
         AXIS(2, 2) = 4.0D0
         AXIS(3, 2) = 1.0D0
 
 
         AXIS(1, 3) = 4.0D0
         AXIS(2, 3) = 1.0D0
         AXIS(3, 3) = 2.0D0
 
 
         AXIS(1, 4) = 4.0D0
         AXIS(2, 4) = 2.0D0
         AXIS(3, 4) = 1.0D0
 
 
         AXIS(1, 5) = 2.0D0
         AXIS(2, 5) = 1.0D0
         AXIS(3, 5) = 4.0D0
 
 
         AXIS(1, 6) = 1.0D0
         AXIS(2, 6) = 4.0D0
         AXIS(3, 6) = 2.0D0
 
 
         AXIS(1, 7) = 1.0D0
         AXIS(2, 7) = 2.0D0
         AXIS(3, 7) = 3.0D0
 
 
         AXIS(1, 8) = 2.0D0
         AXIS(2, 8) = 3.0D0
         AXIS(3, 8) = 1.0D0
 
 
         AXIS(1, 9) = 3.0D0
         AXIS(2, 9) = 1.0D0
         AXIS(3, 9) = 2.0D0
 
 
         AXIS(1,10) = 3.0D0
         AXIS(2,10) = 2.0D0
         AXIS(3,10) = 1.0D0
 
 
         AXIS(1,11) = 2.0D0
         AXIS(2,11) = 1.0D0
         AXIS(3,11) = 3.0D0
 
 
         AXIS(1,12) = 1.0D0
         AXIS(2,12) = 3.0D0
         AXIS(3,12) = 2.0D0
 
 
         AXIS(1,13) = 2.0D0
         AXIS(2,13) = 3.0D0
         AXIS(3,13) = 6.0D0
 
 
         AXIS(1,14) = 3.0D0
         AXIS(2,14) = 6.0D0
         AXIS(3,14) = 2.0D0
 
 
         AXIS(1,15) = 6.0D0
         AXIS(2,15) = 2.0D0
         AXIS(3,15) = 3.0D0
 
 
         AXIS(1,16) = 6.0D0
         AXIS(2,16) = 3.0D0
         AXIS(3,16) = 2.0D0
 
         AXIS(1,17) = 3.0D0
         AXIS(2,17) = 2.0D0
         AXIS(3,17) = 6.0D0
 
 
         AXIS(1,18) = 2.0D0
         AXIS(2,18) = 6.0D0
         AXIS(3,18) = 3.0D0
 
 
         AXIS(1,19) = 1.0D0
         AXIS(2,19) = 1.0D0
         AXIS(3,19) = 1.0D0
 
 
         AXIS(1,20) = 0.0D0
         AXIS(2,20) = 0.0D0
         AXIS(3,20) = 1.0D0
 
C
C        The 11'th reference matrix should be the identity matrix.
C
 
 
         REF(1,1,11) = 1.0D0
         REF(2,1,11) = 0.0D0
         REF(3,1,11) = 0.0D0
 
         REF(1,2,11) = 0.0D0
         REF(2,2,11) = 1.0D0
         REF(3,2,11) = 0.0D0
 
         REF(1,3,11) = 0.0D0
         REF(2,3,11) = 0.0D0
         REF(3,3,11) = 1.0D0
 
         REFTIM( 11) = 0.0D0
 
         DO I = 12, 20
C
C           Recall that the rate of rotation is 1 radian every
C           10 million seconds.  The axis of rotation changes
C           every 100 million seconds.  Hence a DTHETA of 10 radians.
C
            DTHETA    = 10.0D0
            REFTIM(I) = REFTIM(I-1) + 100000000.0D0
 
            CALL AXISAR ( AXIS(1, I-1), DTHETA,       TEMP       )
            CALL MXM    ( TEMP,         REF(1,1,I-1), REF(1,1,I) )
 
         END DO
 
         DO I = 10, 1, -1
 
            REFTIM(I) =  REFTIM(I+1) - 100000000.0D0
            DTHETA    = -10.0D0
 
            CALL AXISAR ( AXIS(1,I), DTHETA,       TEMP        )
            CALL MXM    ( TEMP,      REF(1,1,I+1), REF(1,1,I)  )
 
         END DO
 
      END IF
 
C
C     Compute the offset from the appropriate reference time and
C     simply rotate about the appropriate axis.
C
      I      = MAX( 1, LSTLED ( ET, 20, REFTIM ) )
      DTHETA = ( ET - REFTIM(I) ) / 10000000.0D0
 
      CALL AXISAR ( AXIS(1,I), DTHETA,     TEMP )
      CALL MXM    ( TEMP,      REF(1,1,I), ROT  )
      CALL XPOSE  ( ROT,       MATRIX           )
 
      CALL VHAT ( AXIS(1,I),   TEMPAX           )
      CALL VSCL ( 1.0D-7,      TEMPAX,   ANGVEL )
 
 
      RETURN
      END
 
 
 
