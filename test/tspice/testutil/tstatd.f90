!$Procedure      TSTATD ( Test Attitude )

      SUBROUTINE TSTATD ( ET, MATRIX, ANGVEL )

! $ Abstract
!
!     This routine produces attitude and angular velocity values
!     that should duplicate the values for the test spacecraft
!     with id-code -10001.
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
!      None.
!
! $ Keywords
!
!       TESTING
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      ET
      DOUBLE PRECISION      MATRIX ( 3, 3 )
      DOUBLE PRECISION      ANGVEL ( 3    )


! $ Brief_I/O
!
!      VARIABLE  I/O  DESCRIPTION
!      --------  ---  --------------------------------------------------
!      ET         I   An ephemeris epoch in seconds past J2000
!      MATRIX     O   A rotation from J2000 to the frame of -10001
!      ANGVEL     O   The angular velocity of the rotation.
!
! $ Detailed_Input
!
!     ET          an epoch given in terms of TDB seconds past the
!                 epoch of J2000.
!
! $ Detailed_Output
!
!     MATRIX      is the expected orientation of the test body -10001
!                 relative to the J2000 frame.
!
!     ANGVEL      is the expected angular velocity of the test body
!                 -10001 relative to the J2000 frame.
!
! $ Parameters
!
!      None.
!
! $ Files
!
!      None.
!
! $ Exceptions
!
!     Error free.
!
! $ Particulars
!
!     This routine creates a model for the attitude and angular
!     velocity of the fictitious body -10001  used in testing SPICE
!     SPK, CK, SCLK, and IK systems.
!
!     The attitude is perfectly aligned with the Galactic axes at the
!     epoch of J2000.
!
!     The body rotates at a constant rate of 1 radian every 10 million
!     seconds.  Every 100 million seconds the axis of rotation changes.
!
!     The axes of rotation are:
!
!     From           To                Axis of rotation    Time Interval
!     ------------  ------------       ----------------    -------------
!     -Infinity     -900,000,000       ( 1, 2, 4 )            1
!     -900,000,000  -800,000,000       ( 2, 1, 4 )            2
!     -800,000,000  -700,000,000       ( 4, 1, 2 )            3
!
!     -700,000,000  -600,000,000       ( 4, 2, 1 )            4
!     -600,000,000  -500,000,000       ( 2, 1, 4 )            5
!     -500,000,000  -400,000,000       ( 1, 4, 2 )            6
!
!     -400,000,000  -300,000,000       ( 1, 2, 3 )            7
!     -300,000,000  -200,000,000       ( 2, 3, 1 )            8
!     -200,000,000  -100,000,000       ( 3, 1, 2 )            9
!
!     -100,000,000   000,000,000       ( 3, 2, 1 )            10
!      000,000,000   100,000,000       ( 2, 1, 3 )            11
!      100,000,000   200,000,000       ( 1, 3, 2 )            12
!
!      200,000,000   300,000,000       ( 2, 3, 6 )            13
!      300,000,000   400,000,000       ( 3, 6, 2 )            14
!      400,000,000   500,000,000       ( 6, 2, 3 )            15
!
!      500,000,000   600,000,000       ( 6, 3, 2 )            16
!      600,000,000   700,000,000       ( 3, 2, 6 )            17
!      700,000,000   800,000,000       ( 2, 6, 3 )            18
!
!      800,000,000   900,000,000       ( 1, 1, 1 )            19
!      900,000,000   +Infinity         ( 0, 0, 1 )            20
!
!
! $ Examples
!
!     This routine can be used in conjunction with the routine
!     TSTCK3 to perform tests on components of the CK system.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!      W.L. Taber      (JPL)
!
! $ Literature_References
!
!      None.
!
! $ Version
!
!-    Test Utilities Version 1.0.0, 24-JAN-1995 (WLT)
!
!-&

! $ Index_Entries
!
!     Attitude of the test body -10001
!
!-&

!
!     Spicelib Functions
!
      INTEGER               LSTLED

!
!     Local Variables
!
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
!
!        Setup.  Here we create the rotation matrices at the
!        boundaries of the intervals described in the header.
!

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

!
!        The 11'th reference matrix should be the identity matrix.
!


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
!
!           Recall that the rate of rotation is 1 radian every
!           10 million seconds.  The axis of rotation changes
!           every 100 million seconds.  Hence a DTHETA of 10 radians.
!
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

!
!     Compute the offset from the appropriate reference time and
!     simply rotate about the appropriate axis.
!
      I      = MAX( 1, LSTLED ( ET, 20, REFTIM ) )
      DTHETA = ( ET - REFTIM(I) ) / 10000000.0D0

      CALL AXISAR ( AXIS(1,I), DTHETA,     TEMP )
      CALL MXM    ( TEMP,      REF(1,1,I), ROT  )
      CALL XPOSE  ( ROT,       MATRIX           )

      CALL VHAT ( AXIS(1,I),   TEMPAX           )
      CALL VSCL ( 1.0D-7,      TEMPAX,   ANGVEL )


      RETURN
      END



