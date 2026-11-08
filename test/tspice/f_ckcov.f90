!$Procedure F_CKCOV ( CKCOV tests )

      SUBROUTINE F_CKCOV ( OK )

! $ Abstract
!
!     This routine tests the SPICELIB routines
!
!        CKCOV
!        CKOBJ
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
!     None.
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'ck05.inc'
      INCLUDE 'ck06.inc'
      INCLUDE 'ckparam.inc'

      LOGICAL               OK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   logical indicating test status.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OK         is a logical that indicates the test status to the
!                caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     This routine does not generate any errors. Routines in its
!     call tree may generate errors that are either intentional and
!     trapped or unintentional and need reporting.  The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!     None.
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
!    E. D. Wright   (JPL)
!    N. J. Bachman  (JPL)
!
! $ Version
!
!-    TSPICE Version 3.0.0, 05-FEB-2014 (NJB) (EDW)
!
!        Updated to support CK type 6.
!
!        All variables to SAVE for f2c'd version compiled with Borland.
!        Implemented a proper SPICE header.
!
!-    TSPICE Version 2.0.0, 30-NOV-2007 (NJB)
!
!        Bug fixes: changed interpolation interval start
!        and stop times for segments 3 and 5 to be consistent
!        with pattern described in accompanying comments.
!        Also modified SCLK kernel data generated at run-time
!        so that least significant clock field is consistent
!        with the "tick per second" parameter TIKPER.
!
!        These changes were made to make the test code more
!        easily understood and more maintainable. They
!        don't materially affect the test code's ability
!        to exercise the target code.
!
!-    TSPICE Version 1.0.0, 07-JAN-2005 (NJB)
!
!-&

! $ Index_Entries
!
!   Test CKCOV, CKOBJ
!
!-&

!
!     SPICELIB functions
!
      INTEGER               CARDD
      INTEGER               CARDI

      LOGICAL               EXISTS
      LOGICAL               ODD

!
!     Local parameters
!
      CHARACTER*(*)         CK
      PARAMETER           ( CK     = 'ckcov.bc'  )

      CHARACTER*(*)         EK
      PARAMETER           ( EK     = 'ckcov.bes' )

      CHARACTER*(*)         SPK
      PARAMETER           ( SPK   = 'ckcov.bsp'  )

      CHARACTER*(*)         XFRCK
      PARAMETER           ( XFRCK = 'ckcov.xc'   )


      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               LONGLN
      PARAMETER           ( LONGLN = 240 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               MAXDEF
      PARAMETER           ( MAXDEF = 15 )

      INTEGER               NINS
      PARAMETER           ( NINS   = 6 )

      INTEGER               MAXCOV
      PARAMETER           ( MAXCOV = 10000 )

      INTEGER               MAXREC
      PARAMETER           ( MAXREC = 10000 )

      INTEGER               MXMINI
      PARAMETER           ( MXMINI = MAXREC )

      INTEGER               SIDLEN
      PARAMETER           ( SIDLEN = 40 )

!
!     MAXPKT is the size of the largest CK type 5 subtype's packet.
!
      INTEGER               MAXPKT
      PARAMETER           ( MAXPKT = C05PS2 )

!
!     Local variables
!
      CHARACTER*(LNSIZE)    CVSTAT
      CHARACTER*(LNSIZE)    DEFTXT ( MAXDEF )
      CHARACTER*(SIDLEN)    SEGID
      CHARACTER*(LONGLN)    TITLE

      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      AVVS   ( 3, MAXREC )
      DOUBLE PRECISION      CMAT   ( 3, 3 )
      DOUBLE PRECISION      COVER  ( LBCELL : MAXCOV )
      DOUBLE PRECISION      CLEND  ( MAXREC )
      DOUBLE PRECISION      CLSTRT ( MAXREC )
      DOUBLE PRECISION      DC     ( 2 )
      DOUBLE PRECISION      DLTANG
      DOUBLE PRECISION      ENDS   ( MAXREC )
      DOUBLE PRECISION      EPOCHS ( MAXREC )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      INSETS ( 2 )
      DOUBLE PRECISION      IVLBDS ( MXMINI+1 )
      DOUBLE PRECISION      LAST
      DOUBLE PRECISION      PACKET ( MAXPKT )
      DOUBLE PRECISION      PKTS   ( MAXREC * MAXPKT )
      DOUBLE PRECISION      QUATS  ( 0:3, MAXREC )
      DOUBLE PRECISION      RATE
      DOUBLE PRECISION      RATES  ( MXMINI )
      DOUBLE PRECISION      STARTS ( MAXREC )
      DOUBLE PRECISION      T3END
      DOUBLE PRECISION      TMPWIN ( LBCELL : MAXCOV )
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      XAVINT ( LBCELL : MAXCOV, NINS )
      DOUBLE PRECISION      XAVSEG ( LBCELL : MAXCOV, NINS )
      DOUBLE PRECISION      XCVINT ( LBCELL : MAXCOV, NINS )
      DOUBLE PRECISION      XCVSEG ( LBCELL : MAXCOV, NINS )
      DOUBLE PRECISION      Z      ( 3 )

      INTEGER               CLKID  ( NINS )
      INTEGER               DEFSIZ
      INTEGER               DEGREE
      INTEGER               DEGRES ( MXMINI )
      INTEGER               INST   ( NINS )
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               IDS    ( LBCELL : NINS+1 )
      INTEGER               IVLN   ( NINS )
      INTEGER               J
      INTEGER               K
      INTEGER               L
      INTEGER               M
      INTEGER               NIVREC
      INTEGER               NMINI
      INTEGER               NPKTS  ( MXMINI )
      INTEGER               NSEG   ( NINS )
      INTEGER               NR     ( NINS )
      INTEGER               NREC
      INTEGER               NSTART
      INTEGER               PKTSIZ
      INTEGER               SUBTPS ( MXMINI )
      INTEGER               SUBTYP
      INTEGER               TIKPER ( NINS )
      INTEGER               XIDS   ( LBCELL : NINS+1 )
      INTEGER               XUNIT

      LOGICAL               SELLST
      LOGICAL               USEAV

!
!     Saved variables
!
      SAVE

!
!     Initial values
!
      DATA                  INST   / -1000, -2000, -3000, &
                                     -4000, -5000, -6000 /

      DATA                  NSEG   /  3,    3,     4, &
                                      4,    4,     4     /

      DATA                  NR     /  4,    99,    199, &
                                      2399, 2399,  2399  /

      DATA                  IVLN   /  4,    3,     7, &
                                      20,   20,    20    /

      DATA                  TIKPER /  2,    4,     8, &
                                      16,   32,    32    /

      DATA                  Z      /  0.D0, 0.D0,  1.D0  /

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_CKCOV' )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Setup:  create and load SCLK definitions ' // &
                   'for each instrument.'                      )

      DO I = 1, NINS

         CLKID(I)   = INST(I)/1000

         DEFTXT(1)  = 'SCLK_KERNEL_ID         = ( @03-JAN-2005/02:03 )'
         DEFTXT(2)  = 'SCLK_DATA_TYPE_#       = ( 1 )'
         DEFTXT(3)  = 'SCLK01_TIME_SYSTEM_#   = ( 2 )'
         DEFTXT(4)  = 'SCLK01_N_FIELDS_#      = ( 2 )'
         DEFTXT(5)  = 'SCLK01_MODULI_#        = ( 4294967296 **  )'
         DEFTXT(6)  = 'SCLK01_OFFSETS_#       = ( 0          0   )'
         DEFTXT(7)  = 'SCLK01_OUTPUT_DELIM_#  = ( 1 )'
         DEFTXT(8)  = 'SCLK_PARTITION_START_# = ( 0 )'
         DEFTXT(9)  = 'SCLK_PARTITION_END_#   = ( 1.0995116277750E+12 )'
         DEFTXT(10) = 'SCLK01_COEFFICIENTS_#  = ( 0  0  1 )'

         DEFSIZ     = 10

         DO J = 1, DEFSIZ
            CALL REPMI ( DEFTXT(J), '#', -CLKID(I), DEFTXT(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )
         END DO

!
!        Insert the modulus for the second SCLK field for the
!        current instrument.
!
         CALL REPMI ( DEFTXT(5), '**', TIKPER(I), DEFTXT(5) )

         CALL LMPOOL ( DEFTXT, DEFSIZ )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


!
!     We'll need a leapseconds kernel too.
!
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Setup:  create CK file.' )


!
!     Create a CK file with data for five objects.
!
      IF ( EXISTS(CK) ) THEN
         CALL DELFIL(CK)
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL CKOPN ( CK, CK, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Initialize the expected coverage windows.
!
      DO I = 1, NINS

         CALL SSIZED ( MAXCOV, XAVSEG(LBCELL,I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL SSIZED ( MAXCOV, XAVINT(LBCELL,I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL SSIZED ( MAXCOV, XCVSEG(LBCELL,I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL SSIZED ( MAXCOV, XCVINT(LBCELL,I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL SSIZED ( MAXCOV, TMPWIN )

!
!     Initializations to make compilers happy.
!
      FIRST = 0.D0
      LAST  = 0.D0
      T3END = 0.D0

!
!     For each instrument, we'll create a sequence of segments. Because
!     we have CKCOV code (in some cases, the code resides in supporting
!     utilities) unique to each data type, we'll create segments of all
!     data types:  all of the segments for the Ith instrument will of
!     data type I.  Characteristics of the segments such as presence of
!     angular velocity, spacing of epochs and interpolation intervals,
!     spacing of segments, and time ordering of segments relative to
!     each other will vary.
!
      DO I = 1, NINS

         DO J = 1, NSEG(I)
!
!           Create segments for instrument I.  All segments for
!           instrument I will use data type I.
!
!           Set the number of records in the Jth segment for instrument
!           I.
!
            NREC   = NR ( J )

!
!           Set the number of pointing records per interpolation
!           interval for instrument I.
!
            NIVREC = IVLN ( J )

!
!           The flag USEAV indicates how the angular velocity flag
!           will be set.  Odd-indexed segments get angular velocity.
!
            USEAV  = ODD ( J )
!
!           Proceed to create the Jth segment for instrument I.
!           The following code is data-type dependent.
!
            IF ( I .EQ. 1 ) THEN
!
!              This is the CK type 1 case.
!
!              The segments we create will be separated by a 3 tick gap.
!              Records will be 3*J ticks apart.
!
!              Set segment start and epochs.
!
               IF ( J .EQ. 1 ) THEN
                  FIRST = 0.D0
               ELSE
!
!                 LAST is left over from the previous J-loop iteration.
!
                  FIRST = LAST + 3.D0
               END IF

!
!              Set EPOCHS, QUATS, and AVVS.
!
!              Pointing data are not relevant for these tests,
!              but having distinct entries could be helpful for
!              debugging.  The Kth entry will be a frame rotation
!              by K milliradians about the Z-axis.
!
               DO K = 1, NREC
!
!                 As stated above, records will be 3*J ticks apart.
!
                  EPOCHS(K) =  FIRST + DBLE ( 3*J*(K-1) )

!                 The angle required by AXISAR is the negative of
!                 the frame rotation angle.
!
                  DLTANG = 1.D-3
                  ANGLE  =  - ( K * DLTANG )

                  CALL AXISAR ( Z,    ANGLE, CMAT       )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL M2Q    ( CMAT,        QUATS(0,K) )
                  CALL CHCKXC ( .FALSE., ' ', OK )

!
!                 Set angular velocity to be consistent with
!                 the rotation data.  Remember angular velocity
!                 units are radians/sec, so we must multiply
!                 radians/tick by ticks/second for instrument I.
!
                  CALL VSCL ( TIKPER(I)*DLTANG/(3*J), Z, AVVS(1,K) )

               END DO

!
!              Set segment end time.
!
               LAST = EPOCHS(NREC)

!
!              Add the segment's coverage interval to our segment-level
!              expected coverage window for the Ith instrument.
!
               CALL WNINSD ( FIRST, LAST, XCVSEG(LBCELL,I) )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              If we're providing angular velocity for this segment,
!              then this segment contributes to the coverage window
!              for the angular-velocity only segments at the interval
!              level.
!
               IF ( USEAV ) THEN
                  CALL WNINSD ( FIRST, LAST, XAVSEG(LBCELL,I) )
                  CALL CHCKXC ( .FALSE., ' ', OK )
               END IF

!
!              The singleton intervals defined by the pointing epochs
!              act as interpolation intervals for type 1 segments.
!              Add the interpolation intervals to our interval-level
!              expected coverage window for the Ith instrument.
!
               DO K = 1, NREC
                  CALL WNINSD ( EPOCHS(K), EPOCHS(K), XCVINT(LBCELL,I) )
                  CALL CHCKXC ( .FALSE., ' ', OK )
               END DO

!
!              If we're providing angular velocity for this segment,
!              then this segment contributes to the coverage window
!              for the angular-velocity only segments at the interval
!              level.
!
               IF ( USEAV ) THEN

                  DO K = 1, NREC

                     CALL WNINSD ( EPOCHS(K), &
                                   EPOCHS(K), &
                                   XAVINT(LBCELL,I) )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                  END DO

               END IF

!
!              Create segment ID.
!
               SEGID = 'Segment # for instrument #.'

               CALL REPMI  ( SEGID, '#', J, SEGID )
               CALL REPMI  ( SEGID, '#', I, SEGID )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Write the current segment to our CK.
!
               CALL CKW01 ( HANDLE,   FIRST,  LAST,   INST(I), &
                            'J2000',  USEAV,  SEGID,  NREC, &
                            EPOCHS,   QUATS,  AVVS            )
               CALL CHCKXC ( .FALSE., ' ', OK )



            ELSE IF ( I .EQ. 2 ) THEN
!
!              This is the CK type 2 case.
!
!              For type 2, angular velocity is present by definition.
!
               USEAV = .TRUE.

!
!              We're going to copy the data for the type 1 case, but
!              here, the segments we create will abut each other.
!              Records will be 2*J ticks apart.
!
!              Set segment start and epochs.
!
               IF ( J .EQ. 1 ) THEN
                  FIRST = 0.D0
               ELSE
!
!                 LAST is left over from the previous J-loop iteration.
!
                  FIRST = LAST
               END IF

!
!              Set EPOCHS, QUATS, and AVVS.
!
!              Pointing data are not relevant for these tests,
!              but having distinct entries could be helpful for
!              debugging.  The Kth entry will be a frame rotation
!              by K milliradians about the Z-axis.
!
               DO K = 1, NREC
!
!                 As stated above, records will be 2*J ticks apart.
!
                  EPOCHS(K) =  FIRST + DBLE ( (K-1)*J*2 )

!
!                 Each interpolation interval will be 1 tick long.
!
                  ENDS(K)   =  EPOCHS(K) + 1.D0


!                 The angle required by AXISAR is the negative of
!                 the frame rotation angle.
!
                  DLTANG = 1.D-3
                  ANGLE  =  - ( K * DLTANG )

                  CALL AXISAR ( Z,    ANGLE, CMAT       )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL M2Q    ( CMAT,        QUATS(0,K) )
                  CALL CHCKXC ( .FALSE., ' ', OK )

!
!                 Set angular velocity to be consistent with
!                 the rotation data.  Remember angular velocity
!                 units are radians/sec, so we must multiply
!                 radians/tick by ticks/second for instrument I.
!
                  CALL VSCL ( TIKPER(I)*DLTANG/(2*J), Z, AVVS(1,K) )

!
!                 Set the clock rate in seconds per tick for the
!                 Kth interpolation interval.
!
                  RATES(K) =  1.D0 / TIKPER(I)

               END DO

!
!              Set segment end time.  Note that this is the end of
!              the last interpolation interval.
!
               LAST = ENDS(NREC)

!
!              Add the segment's coverage interval to our segment-level
!              expected coverage window for the Ith instrument.
!
               CALL WNINSD ( FIRST, LAST, XCVSEG(LBCELL,I) )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Since we're providing angular velocity for this segment,
!              then this segment contributes to the coverage window
!              for the angular-velocity only segments at the interval
!              level.
!
               CALL WNINSD ( FIRST, LAST, XAVSEG(LBCELL,I) )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Add the interpolation intervals to our interval-level
!              expected coverage window for the Ith instrument.
!
               DO K = 1, NREC
                  CALL WNINSD ( EPOCHS(K), ENDS(K), XCVINT(LBCELL,I) )
                  CALL WNINSD ( EPOCHS(K), ENDS(K), XAVINT(LBCELL,I) )
               END DO

!
!              Create segment ID.
!
               SEGID = 'Segment # for instrument #.'

               CALL REPMI  ( SEGID, '#', J, SEGID )
               CALL REPMI  ( SEGID, '#', I, SEGID )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Write the current segment to our CK.
!
               CALL CKW02 ( HANDLE,   FIRST,  LAST,  INST(I), &
                            'J2000',  SEGID,  NREC,  EPOCHS, &
                            ENDS,     QUATS,  AVVS,  RATES   )

               CALL CHCKXC ( .FALSE., ' ', OK )


            ELSE IF ( I .EQ. 3 ) THEN
!
!              This is the CK type 3 case.
!
!              The segments we create will be separated by a 3 tick gap.
!              Records will be J ticks apart.
!
!              Set segment start and epochs.
!
               IF ( J .EQ. 1 ) THEN
                  FIRST = 0.D0
               ELSE
!
!                 LAST is left over from the previous J-loop iteration.
!
                  FIRST = LAST + 3.D0
               END IF

!
!              Set EPOCHS, QUATS, and AVVS.
!
!              Pointing data are not relevant for these tests,
!              but having distinct entries could be helpful for
!              debugging.  The Kth entry will be a frame rotation
!              by K milliradians about the Z-axis.
!
               DO K = 1, NREC
!
!                 As stated above, records will be J ticks apart.
!
                  EPOCHS(K) =  FIRST + DBLE ( (K-1)*J )

!                 The angle required by AXISAR is the negative of
!                 the frame rotation angle.
!
                  DLTANG = 1.D-3
                  ANGLE  =  - ( K * DLTANG )

                  CALL AXISAR ( Z,    ANGLE, CMAT       )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL M2Q    ( CMAT,        QUATS(0,K) )
                  CALL CHCKXC ( .FALSE., ' ', OK )

!
!                 Set angular velocity to be consistent with
!                 the rotation data.  Remember angular velocity
!                 units are radians/sec, so we must multiply
!                 radians/tick by ticks/second for instrument I.
!
                  CALL VSCL ( TIKPER(I)*DLTANG/J, Z, AVVS(1,K) )

               END DO

!
!              Set segment end time.
!
               LAST = EPOCHS(NREC)

!
!              Add the segment's coverage interval to our segment-level
!              expected coverage window for the Ith instrument.
!
               CALL WNINSD ( FIRST, LAST, XCVSEG(LBCELL,I) )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              If we're providing angular velocity for this segment,
!              then this segment contributes to the coverage window
!              for the angular-velocity only segments at the interval
!              level.
!
               IF ( USEAV ) THEN
                  CALL WNINSD ( FIRST, LAST, XAVSEG(LBCELL,I) )
                  CALL CHCKXC ( .FALSE., ' ', OK )
               END IF

!
!              Set the interval start times.  The first epoch
!              is always the start of an interpolation interval
!              in these tests.  Each interval has length NIVREC
!              records.
!
               L = 0

               DO K = 1,  NREC,  NIVREC
!
!                 Increment the interpolation interval; set the
!                 start time.
!
                  L         = L + 1

                  STARTS(L) = EPOCHS(K)

!
!                 Keep track of the interval end times.
!
                  IF ( L .GT. 1 ) THEN
!
!                    Record the end time of the previous interval.
!
                     ENDS(L-1) = EPOCHS(K-1)

                  END IF

               END DO

!
!              Set the interpolation interval count.
!
               NSTART = L

!
!              The end time of the last interval is (in this test)
!              always the last epoch.
!
               ENDS(NSTART) = EPOCHS(NREC)

!
!              Add the interpolation intervals to our interval-level
!              expected coverage window for the Ith instrument.
!
               DO K = 1, NSTART
                  CALL WNINSD ( STARTS(K), ENDS(K), XCVINT(LBCELL,I) )
               END DO

!
!              If we're providing angular velocity for this segment,
!              then this segment contributes to the coverage window
!              for the angular-velocity only segments at the interval
!              level.
!
               IF ( USEAV ) THEN

                  DO K = 1, NSTART

                     CALL WNINSD ( STARTS(K), &
                                   ENDS(K), &
                                   XAVINT(LBCELL,I) )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                  END DO

               END IF

!
!              Create segment ID.
!
               SEGID = 'Segment # for instrument #.'

               CALL REPMI  ( SEGID, '#', J, SEGID )
               CALL REPMI  ( SEGID, '#', I, SEGID )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Write the current segment to our CK.
!
               CALL CKW03 ( HANDLE,   FIRST,  LAST,   INST(I), &
                            'J2000',  USEAV,  SEGID,  NREC, &
                            EPOCHS,   QUATS,  AVVS,   NSTART,  STARTS )
               CALL CHCKXC ( .FALSE., ' ', OK )


!
!              If this is the last type 3 segment, save the end
!              time of the segment.
!
               IF ( J .EQ. NSEG(3) ) THEN
                  T3END = LAST
               END IF
!
!
!

            ELSE IF ( I .EQ. 4 ) THEN
!
!              This is the CK type 4 case.
!
!              The segments we create will be separated by a 3 tick gap.
!              Records will be J ticks apart.
!
!              Set segment start and epochs.
!
               IF ( J .EQ. 1 ) THEN
                  FIRST = 0.D0
               ELSE
!
!                 LAST is left over from the previous J-loop iteration.
!
                  FIRST = LAST + 3.D0
               END IF

!
!              Create segment ID.
!
               SEGID = 'Segment # for instrument #.'

               CALL REPMI  ( SEGID, '#', J, SEGID )
               CALL REPMI  ( SEGID, '#', I, SEGID )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Begin the segment.
!
               CALL CKW04B ( HANDLE,  FIRST, INST(I), &
                             'J2000', USEAV, SEGID   )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Define the start epochs for the packets.
!
               DO K = 1, NREC
!
!                 Packet starts will be 1000*J ticks apart.
!
                  EPOCHS(K) =  FIRST + DBLE ( (K-1)*J*1000 )

               END DO

!
!              The segment end time matches the end time of the last
!              packet.
!
               LAST = EPOCHS(NREC)

!
!              Define the data packets for the current segment;
!              add each one to the segment.
!
               DO K = 1, NREC
!
!                 Fill in the current packet.  The packet structure
!                 is as follows:
!
!                   ----------------------------------------------------
!                   | The midpoint of the approximation interval       |
!                   ----------------------------------------------------
!                   | The radius of the approximation interval         |
!                   ----------------------------------------------------
!                   | Number of coefficients for q0                    |
!                   ----------------------------------------------------
!                   | Number of coefficients for q1                    |
!                   ----------------------------------------------------
!                   | Number of coefficients for q2                    |
!                   ----------------------------------------------------
!                   | Number of coefficients for q3                    |
!                   ----------------------------------------------------
!                   | Number of coefficients for AV1                   |
!                   ----------------------------------------------------
!                   | Number of coefficients for AV2                   |
!                   ----------------------------------------------------
!                   | Number of coefficients for AV3                   |
!                   ----------------------------------------------------
!                   | q0 Cheby coefficients                            |
!                   ----------------------------------------------------
!                   | q1 Cheby coefficients                            |
!                   ----------------------------------------------------
!                   | q2 Cheby coefficients                            |
!                   ----------------------------------------------------
!                   | q3 Cheby coefficients                            |
!                   ----------------------------------------------------
!                   | AV1 Cheby coefficients (optional)                |
!                   ----------------------------------------------------
!                   | AV2 Cheby coefficients (optional)                |
!                   ----------------------------------------------------
!                   | AV3 Cheby coefficients (optional)                |
!                   ----------------------------------------------------
!
!                 The interval radius will be 499 ticks.  This will
!                 put the intervals two ticks apart.
!
!                 The interval midpoint will be at the start time
!                 plus 499 ticks.
!
                  PKTS(1) = 499.D0 + EPOCHS(K)
                  PKTS(2) = 499.D0
!
!                 Our quaternions will be constant.
!
                  PKTS(3) = 1
                  PKTS(4) = 1
                  PKTS(5) = 1
                  PKTS(6) = 1
!
!                 Angular velocity will be constant at 0.
!
                  PKTS(7) = 1
                  PKTS(8) = 1
                  PKTS(9) = 1
!
!                 Cheby coefficients for the quaternion elements:
!
                  PKTS(10) = 1.D0
                  PKTS(11) = 2.D0
                  PKTS(12) = 3.D0
                  PKTS(13) = 4.D0
!
!                 Cheby coefficients for the angular velocity elements:
!
                  PKTS(14) = 0.D0
                  PKTS(15) = 0.D0
                  PKTS(16) = 0.D0

!
!                 The packet size depends on whether we're using
!                 angular velocity in this segment.
!
                  IF ( USEAV ) THEN
                     PKTSIZ = 16
                  ELSE
                     PKTSIZ = 13
                  END IF

!
!                 Add the current packet.
!
                  CALL CKW04A ( HANDLE, 1, PKTSIZ, PKTS, EPOCHS(K) )
                  CALL CHCKXC ( .FALSE., ' ', OK )

!
!                 Add the interpolation interval to our interval-level
!                 expected coverage window for the Ith instrument.
!
                  CALL WNINSD ( EPOCHS(K), &
                                EPOCHS(K) + 2*PKTS(2), &
                                XCVINT(LBCELL,I)       )
                  CALL CHCKXC ( .FALSE., ' ', OK )

!
!                 If we're providing angular velocity for this segment,
!                 then this segment contributes to the coverage window
!                 for the angular-velocity only segments at the interval
!                 level.
!
                  IF ( USEAV ) THEN

                     CALL WNINSD ( EPOCHS(K), &
                                   EPOCHS(K) + 2*PKTS(2), &
                                   XAVINT(LBCELL,I)       )

                     CALL CHCKXC ( .FALSE., ' ', OK )

                  END IF

               END DO

!
!              End the segment.
!
               CALL CKW04E ( HANDLE, LAST )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Add the segment's coverage interval to our segment-level
!              expected coverage window for the Ith instrument.
!
               CALL WNINSD ( FIRST, LAST, XCVSEG(LBCELL,I) )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              If we're providing angular velocity for this segment,
!              then this segment contributes to the coverage window
!              for the angular-velocity only segments at the interval
!              level.
!
               IF ( USEAV ) THEN
                  CALL WNINSD ( FIRST, LAST, XAVSEG(LBCELL,I) )
                  CALL CHCKXC ( .FALSE., ' ', OK )
               END IF


            ELSE IF ( I .EQ. 5 ) THEN
!
!              This is the CK type 5 case.
!
!              Set type 5 subtype.  We expect NSEG(5) == 4.
!
               IF ( NSEG(5) .NE. 4 ) THEN

                  CALL SETMSG ( 'Test cases for CK type 5 segments ' // &
                                'use a different type 5 subtype for '// &
                                'each segment.  The Ith segment is ' // &
                                'mapped to subtype I-1.  Subtype '   // &
                                'numbers range from 0 to 3. NSEG(5) '// &
                                'was expected to be 4 but was #.'    )
                  CALL ERRINT ( '#', NSEG(5)                         )
                  CALL SIGERR ( 'SPICE(BUG)'                         )
                  CALL CHCKXC ( .FALSE., ' ', OK )

               END IF

               SUBTYP = J - 1

!
!              Set packet size.
!
               IF ( SUBTYP .EQ. 0 ) THEN
                  PKTSIZ = C05PS0

               ELSE IF ( SUBTYP .EQ. 1 ) THEN
                  PKTSIZ = C05PS1

               ELSE IF ( SUBTYP .EQ. 2 ) THEN
                  PKTSIZ = C05PS2

               ELSE IF ( SUBTYP .EQ. 3 ) THEN
                  PKTSIZ = C05PS3
               END IF


!              We'll mimic the construction of the type 3 segments,
!              but we'll put the segments in reverse time order
!              relative to each other.
!
!              T3END is supposed to have been initialized before
!              we get here.
!
!
!              We'll use M as a complementary index with respect to
!              J and NSEG(I):
!
               M      =  NSEG(I) + 1 - J

!
!              We must set NREC and USEAV specially for this
!              "backward" segment order.
!
               NREC   =  NR  ( M )
               USEAV  =  ODD ( M )

!
!              Set segment bounds insets:  except for the short
!              segment, the segment bounds will be *inside* the
!              coverage of the interpolation intervals.
!
               IF ( NREC .LT. 99 ) THEN
                  INSETS(1) = 0.D0
                  INSETS(2) = 0.D0
               ELSE
                  INSETS(1) = 3*M
                  INSETS(2) = 5*M
               END IF

!
!              So M will start at NSEG(I) and count down to 1.
!
!              The segments we create will be separated by a 3 tick gap.
!              Records will be M ticks apart.
!
!              Set segment end and epochs.
!
               IF ( M .EQ. NSEG(I) ) THEN
                  LAST = T3END
               ELSE
!
!                 FIRST is left over from the previous M-loop iteration.
!
                  LAST = FIRST - 3.D0
               END IF

!
!              Set EPOCHS, QUATS, and AVVS.
!
!              Pointing data are not relevant for these tests,
!              but having distinct entries could be helpful for
!              debugging.  The Kth entry will be a frame rotation
!              by K milliradians about the Z-axis.
!
               DO K = NREC, 1, -1
!
!                 As stated above, records will be M ticks apart.
!
                  EPOCHS(K) =  LAST - DBLE ( M*(NREC-K) )


!                 The angle required by AXISAR is the negative of
!                 the frame rotation angle.
!
                  DLTANG = 1.D-3
                  ANGLE  =  - ( K * DLTANG )

                  CALL AXISAR ( Z,    ANGLE, CMAT       )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL M2Q    ( CMAT,        QUATS(0,K) )
                  CALL CHCKXC ( .FALSE., ' ', OK )

!
!                 Set angular velocity to be consistent with
!                 the rotation data.  Remember angular velocity
!                 units are radians/sec, so we must multiply
!                 radians/tick by ticks/second for instrument I.
!
                  CALL VSCL ( TIKPER(I)*DLTANG/M, Z, AVVS(1,K) )

!
!                 Set packet contents.
!
                  CALL CLEARD ( MAXPKT, PACKET )

                  IF ( SUBTYP .EQ. 0 ) THEN
!
!                    Packets contain quaternions and quaternion
!                    derivatives.  We'll set the derivatives to zero.
!
                     CALL MOVED ( QUATS(0,K), 4, PACKET )

                  ELSE IF ( SUBTYP .EQ. 1 ) THEN
!
!                    Packets contain quaternions only.
!
                     CALL MOVED ( QUATS(0,K), 4, PACKET )

                  ELSE IF ( SUBTYP .EQ. 2 ) THEN
!
!                    Packets contain quaternions, quaternion
!                    derivatives, angular velocity, and angular
!                    velocity derivatives.  We'll set the derivatives
!                    to zero (even though this makes the angular
!                    velocity and quaternion derivatives
!                    incompatible---subtype 2 is meant to handle
!                    this).
!
                     CALL MOVED ( QUATS(0,K), 4, PACKET    )
                     CALL MOVED ( AVVS (1,K), 3, PACKET(9) )


                  ELSE IF ( SUBTYP .EQ. 3 ) THEN
!
!                    Packets contain quaternions and angular velocity.
!
                     CALL MOVED ( QUATS(0,K), 4, PACKET    )
                     CALL MOVED ( AVVS (1,K), 3, PACKET(5) )

                  END IF

!
!                 Insert packet into packet array.
!
                  L  =  1  +  (K-1)*PKTSIZ

                  CALL MOVED ( PACKET, PKTSIZ, PKTS(L) )

               END DO

!
!              Set segment start time.
!
               FIRST = EPOCHS(1)

!
!              Add the segment's coverage interval to our segment-level
!              expected coverage window for the Ith instrument.
!
               DC(1)  =  FIRST + INSETS(1)
               DC(2)  =  LAST  - INSETS(2)

               CALL WNINSD ( DC(1), DC(2), XCVSEG(LBCELL,I) )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              If we're providing angular velocity for this segment,
!              then this segment contributes to the coverage window
!              for the angular-velocity only segments at the interval
!              level.
!
               IF ( USEAV ) THEN
                  CALL WNINSD ( DC(1), DC(2), XAVSEG(LBCELL,I) )
                  CALL CHCKXC ( .FALSE., ' ', OK )
               END IF

!
!              Set the interval start times.  The first epoch
!              is always the start of an interpolation interval
!              in these tests.  Each interval has length NIVREC
!              records.
!
               L = 0

               DO K = 1,  NREC,  NIVREC
!
!                 Increment the interpolation interval; set the
!                 start time.
!
                  L         = L + 1

                  STARTS(L) = EPOCHS(K)

!
!                 Keep track of the interval end times.
!
                  IF ( L .GT. 1 ) THEN
!
!                    Record the end time of the previous interval.
!
                     ENDS(L-1) = EPOCHS(K-1)

                  END IF

               END DO

!
!              Set the interpolation interval count.
!
               NSTART = L

!
!              The end time of the last interval is (in this test)
!              always the last epoch.
!
               ENDS(NSTART) = EPOCHS(NREC)

!
!              Add the interpolation intervals to our interval-level
!              expected coverage window for the Ith instrument.
!
               DO K = 1, NSTART
!
!                 Adjust the interpolation intervals to account for
!                 the segment boundaries.
!
                  CLSTRT(K)  =  MAX ( STARTS(K), DC(1) )
                  CLEND(K)   =  MIN ( ENDS(K),   DC(2) )

                  IF ( CLSTRT(K) .LE. CLEND(K) ) THEN
                     CALL WNINSD( CLSTRT(K), CLEND(K), XCVINT(LBCELL,I))
                  END IF

               END DO

!
!              If we're providing angular velocity for this segment,
!              then this segment contributes to the coverage window
!              for the angular-velocity only segments at the interval
!              level.
!
               IF ( USEAV ) THEN

                  DO K = 1, NSTART
!
!                    The interpolation intervals that have been
!                    adjusted to account for the segment boundaries
!                    are contained in CLSTRT and CLEND.
!
                     IF ( CLSTRT(K) .LE. CLEND(K) ) THEN

                        CALL WNINSD ( CLSTRT(K), &
                                      CLEND(K), &
                                      XAVINT(LBCELL,I) )

                        CALL CHCKXC ( .FALSE., ' ', OK )

                     END IF

                  END DO

               END IF

!
!              Create segment ID.
!
               SEGID = 'Segment # for instrument #.'

               CALL REPMI  ( SEGID, '#', J, SEGID )
               CALL REPMI  ( SEGID, '#', I, SEGID )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Write the current segment to our CK.  All interpolating
!              polynomials will be cubic.
!
               DEGREE = 3
               RATE   = 1.D0 / TIKPER(I)

               CALL CKW05 ( HANDLE,   SUBTYP,   DEGREE, DC(1),  DC(2), &
                            INST(I),  'J2000',  USEAV,  SEGID,  NREC, &
                            EPOCHS,   PKTS,     RATE,   NSTART, STARTS )

               CALL CHCKXC ( .FALSE., ' ', OK )



            ELSE IF ( I .EQ. 6 ) THEN
!
!              This is the CK type 6 case.
!
!              We'll use the "select last" option.
!
               SELLST = .TRUE.
!
!              Set type 6 subtype.  We expect NSEG(6) == 4.
!
               IF ( NSEG(6) .NE. 4 ) THEN

                  CALL SETMSG ( 'Test cases for CK type 6 segments ' // &
                                'use a different type 6 subtype for '// &
                                'each segment.  The Ith segment is ' // &
                                'mapped to subtype I-1.  Subtype '   // &
                                'numbers range from 0 to 3. NSEG(6) '// &
                                'was expected to be 4 but was #.'    )
                  CALL ERRINT ( '#', NSEG(6)                         )
                  CALL SIGERR ( 'SPICE(BUG)'                         )
                  CALL CHCKXC ( .FALSE., ' ', OK )

               END IF

               SUBTYP = J - 1

!
!              Set packet size.
!
               IF ( SUBTYP .EQ. 0 ) THEN
                  PKTSIZ = C06PS0

               ELSE IF ( SUBTYP .EQ. 1 ) THEN
                  PKTSIZ = C06PS1

               ELSE IF ( SUBTYP .EQ. 2 ) THEN
                  PKTSIZ = C06PS2

               ELSE IF ( SUBTYP .EQ. 3 ) THEN
                  PKTSIZ = C06PS3
               END IF

!
!              We'll mimic the construction of the type 3 segments,
!              but we'll put the segments in reverse time order
!              relative to each other.
!
!              T3END is supposed to have been initialized before
!              we get here.
!
!              We'll use M as a complementary index with respect to
!              J and NSEG(I):
!
               M      =  NSEG(I) + 1 - J

!
!              We must set NREC and USEAV specially for this
!              "backward" segment order.
!
               NREC   =  NR  ( M )

               USEAV  =  ODD ( M )

!
!              Set segment bounds insets:  except for the short
!              segment, the segment bounds will be *inside* the
!              coverage of the interpolation intervals.
!
               IF ( NREC .LT. 99 ) THEN
                  INSETS(1) = 0.D0
                  INSETS(2) = 0.D0
               ELSE
                  INSETS(1) = 3*M
                  INSETS(2) = 5*M
               END IF

!
!              So M will start at NSEG(I) and count down to 1.
!
!              The segments we create will be separated by a 3 tick gap.
!              Records will be M ticks apart.
!
!              Set segment end and epochs.
!
               IF ( M .EQ. NSEG(I) ) THEN
                  LAST = T3END
               ELSE
!
!                 FIRST is left over from the previous M-loop iteration.
!
                  LAST = FIRST - 3.D0
               END IF

!
!              Set EPOCHS, QUATS, and AVVS.
!
!              Pointing data are not relevant for these tests,
!              but having distinct entries could be helpful for
!              debugging.  The Kth entry will be a frame rotation
!              by K milliradians about the Z-axis.
!
               DO K = NREC, 1, -1
!
!                 As stated above, records will be M ticks apart.
!
                  EPOCHS(K) =  LAST - DBLE ( M*(NREC-K) )


!                 The angle required by AXISAR is the negative of
!                 the frame rotation angle.
!
                  DLTANG = 1.D-3
                  ANGLE  =  - ( K * DLTANG )

                  CALL AXISAR ( Z,    ANGLE, CMAT       )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL M2Q    ( CMAT,        QUATS(0,K) )
                  CALL CHCKXC ( .FALSE., ' ', OK )

!
!                 Set angular velocity to be consistent with
!                 the rotation data.  Remember angular velocity
!                 units are radians/sec, so we must multiply
!                 radians/tick by ticks/second for instrument I.
!
                  CALL VSCL ( TIKPER(I)*DLTANG/M, Z, AVVS(1,K) )

!
!                 Set packet contents.
!
                  CALL CLEARD ( MAXPKT, PACKET )

                  IF ( SUBTYP .EQ. 0 ) THEN
!
!                    Packets contain quaternions and quaternion
!                    derivatives.  We'll set the derivatives to zero.
!
                     CALL MOVED ( QUATS(0,K), 4, PACKET )

                  ELSE IF ( SUBTYP .EQ. 1 ) THEN
!
!                    Packets contain quaternions only.
!
                     CALL MOVED ( QUATS(0,K), 4, PACKET )

                  ELSE IF ( SUBTYP .EQ. 2 ) THEN
!
!                    Packets contain quaternions, quaternion
!                    derivatives, angular velocity, and angular
!                    velocity derivatives.  We'll set the derivatives
!                    to zero (even though this makes the angular
!                    velocity and quaternion derivatives
!                    incompatible---subtype 2 is meant to handle
!                    this).
!
                     CALL MOVED ( QUATS(0,K), 4, PACKET    )
                     CALL MOVED ( AVVS (1,K), 3, PACKET(9) )


                  ELSE IF ( SUBTYP .EQ. 3 ) THEN
!
!                    Packets contain quaternions and angular velocity.
!
                     CALL MOVED ( QUATS(0,K), 4, PACKET    )
                     CALL MOVED ( AVVS (1,K), 3, PACKET(5) )

                  END IF

!
!                 Insert packet into packet array.
!
                  L  =  1  +  (K-1)*PKTSIZ

                  CALL MOVED ( PACKET, PKTSIZ, PKTS(L) )

               END DO

!
!              Set segment start time.
!
               FIRST = EPOCHS(1)

!
!              Add the segment's coverage interval to our segment-level
!              expected coverage window for the Ith instrument.
!
               DC(1)  =  FIRST + INSETS(1)
               DC(2)  =  LAST  - INSETS(2)

               CALL WNINSD ( DC(1), DC(2), XCVSEG(LBCELL,I) )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              If we're providing angular velocity for this segment,
!              then this segment contributes to the coverage window
!              for the angular-velocity only segments at the interval
!              level.
!
               IF ( USEAV ) THEN
                  CALL WNINSD ( DC(1), DC(2), XAVSEG(LBCELL,I) )
                  CALL CHCKXC ( .FALSE., ' ', OK )
               END IF

!
!              Set the interval start times.  The first epoch
!              is always the start of an interpolation interval
!              in these tests.  Each interval but the last
!              one in the segment has length NIVREC records.
!              NMINI counts the number of interpolation intervals
!              (equal to the number of mini-segments) in the
!              segment.
!
!              Each interval has a gap at the end. The gap duration
!              is the (constant) separation between epochs, which
!              has been set to M ticks.
!
               L = 0

!
!              Terminate this loop before the last record, so we don't
!              start an interpolation interval at that record.
!
               DO K = 1,  NREC-1,  NIVREC
!
!                 Increment the interpolation interval; set the
!                 start time.
!
                  L         = L + 1

                  IVLBDS(L) = EPOCHS(K)
                  STARTS(L) = IVLBDS(L)

!
!                 Keep track of the interval end times.
!
                  IF ( L .GT. 1 ) THEN
!
!                    Record the "end" time of the previous interval.
!                    The end time in this context is the last epoch,
!                    not the right hand interval boundary. The end time
!                    represents the upper bound of the interval over
!                    which data are available. The interval upper bound
!                    coincides with the upper end of a coverage gap.
!
                     ENDS(L-1) = EPOCHS(K-1)

                  END IF
!
!                 Set the other interval-related parameters.
!
!                 All interpolating polynomials will be cubic.
!
                  DEGREE = 3
                  RATE   = 1.D0 / TIKPER(I)

                  NPKTS (L) = MIN( NIVREC,  (NREC-K+1) )
                  RATES (L) = RATE
                  SUBTPS(L) = SUBTYP
                  DEGRES(L) = DEGREE

               END DO

!
!              Set the interpolation interval count.
!
               NMINI  = L
               NSTART = NMINI
!
!              The last epoch of the last interval is (in this test)
!              the last epoch; the interval upper bound is beyond that
!              epoch, since the interval has a gap.
!
               ENDS(NMINI)     = EPOCHS(NREC)

               IVLBDS(NMINI+1) = EPOCHS(NREC) + M

!
!              Add the interpolation intervals to our interval-level
!              expected coverage window for the Ith instrument.
!
               DO K = 1, NSTART
!
!                 Adjust the interpolation intervals to account for
!                 the segment boundaries.
!
                  CLSTRT(K)  =  MAX ( STARTS(K), DC(1) )
                  CLEND(K)   =  MIN ( ENDS(K),   DC(2) )

                  IF ( CLSTRT(K) .LE. CLEND(K) ) THEN
                     CALL WNINSD( CLSTRT(K), CLEND(K), XCVINT(LBCELL,I))
                  END IF

               END DO

!
!              If we're providing angular velocity for this segment,
!              then this segment contributes to the coverage window
!              for the angular-velocity only segments at the interval
!              level.
!
               IF ( USEAV ) THEN

                  DO K = 1, NSTART
!
!                    The interpolation intervals that have been
!                    adjusted to account for the segment boundaries
!                    are contained in CLSTRT and CLEND.
!
                     IF ( CLSTRT(K) .LE. CLEND(K) ) THEN

                        CALL WNINSD ( CLSTRT(K), &
                                      CLEND(K), &
                                      XAVINT(LBCELL,I) )

                        CALL CHCKXC ( .FALSE., ' ', OK )

                     END IF

                  END DO

               END IF

!
!              Create segment ID.
!
               SEGID = 'Segment # for instrument #.'

               CALL REPMI  ( SEGID, '#', J, SEGID )
               CALL REPMI  ( SEGID, '#', I, SEGID )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Write the current segment to our CK.
!
               CALL CKW06 ( HANDLE, INST(I), 'J2000', USEAV, &
                            DC(1),  DC(2),   SEGID,   NMINI, &
                            NPKTS,  SUBTPS,  DEGRES,  PKTS, &
                            RATES,  EPOCHS,  IVLBDS,  SELLST )
               CALL CHCKXC ( .FALSE., ' ', OK )



            ELSE
!
!              Oops.
!
               CALL SETMSG ( 'Unexpected data type: #' )
               CALL ERRINT ( '#', I                    )
               CALL SIGERR ( 'SPICE(BUG)'              )
               CALL CHCKXC ( .FALSE., ' ', OK )

            END IF



         END DO

      END DO


      CALL CKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Call DAFCLS as a last resort, since CKCLS may fail to
!     close the file.
!
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

! ******************************************************
! ******************************************************
! ******************************************************
!     CKCOV tests
! ******************************************************
! ******************************************************
! ******************************************************
!
!     We've written the CK.  It's time to check out CKCOV.
!
!
!     Check actual vs expected coverage as we vary the input
!     arguments to CKCOV.
!
!
!     Each test we do will be performed with both an empty
!     and non-empty input coverage window.
!
      DO L = 1, 2
!
!        We'll start out by testing the coverage summary at the
!        segment level.
!
         IF ( L .EQ. 1 ) THEN
!
!           We'll set COVER to be empty on input to CKCOV.
!
            CALL SCARDD ( 0, COVER )

            CVSTAT = 'empty'

         ELSE

            CVSTAT = 'non-empty'

         END IF


         DO I = 1, NINS
!
! --- Case: ------------------------------------------------------
!
            TITLE = 'Check segment-level '                       // &
                    'coverage for instrument #; COVER starts '   // &
                    'out #. Angular velocity not needed. '       // &
                    'TOL = 0.D0.'

            CALL REPMI ( TITLE, '#', I,      TITLE )
            CALL REPMC ( TITLE, '#', CVSTAT, TITLE )
            CALL TCASE ( TITLE )

!
!           Initialize COVER.
!
            CALL SSIZED ( MAXCOV, COVER )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Make a copy of the expected window.
!
            CALL COPYD ( XCVSEG(LBCELL,I),  TMPWIN )

            IF ( L .EQ. 2 ) THEN
!
!              Insert an interval into COVER.  This same interval
!              must be added to each window containing expected
!              coverage.
!
               CALL WNINSD ( 1.D6, 1.D7, COVER )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              The same interval is expected to appear in the output.
!
               CALL WNINSD ( 1.D6, 1.D7, TMPWIN )
               CALL CHCKXC ( .FALSE., ' ', OK )

            END IF


            CALL CKCOV ( CK,   INST(I), .FALSE., 'SEGMENT', &
                         0.D0, 'SCLK',  COVER              )

            CALL CHCKXC ( .FALSE., ' ', OK )
!
!           Check cardinality of coverage window.
!
            CALL CHCKSI ( 'CARDD(COVER)', CARDD( COVER  ), &
                          '=',            CARDD( TMPWIN ), &
                          0,              OK                )

!
!           Check coverage window.
!
            CALL CHCKAD ( 'COVER', COVER(1), &
                          '=',     TMPWIN(1),  CARDD(COVER), &
                          0.D0,    OK                        )

            IF ( I .EQ. 6 ) THEN

               CALL SCARDD ( 0, TMPWIN )
               CALL CHCKXC ( .FALSE., ' ', OK )

               IF ( L .EQ. 2 ) THEN
!
!                 The same interval is expected to appear in the output.
!
                  CALL WNINSD ( 1.D6, 1.D7, TMPWIN )
                  CALL CHCKXC ( .FALSE., ' ', OK )

               END IF

!
!              Compare CKCOV results against those from T_CKCOV.
!
               CALL T_CKCOV ( CK,   INST(I), .FALSE., 'SEGMENT', &
                              0.D0, 'SCLK',  TMPWIN             )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Check cardinality of coverage window.
!
               CALL CHCKSI ( 'CARDD(COVER)', CARDD( COVER  ), &
                             '=',            CARDD( TMPWIN ), &
                             0,              OK                )

!
!              Check coverage window.
!
               CALL CHCKAD ( 'COVER', COVER(1), &
                             '=',     TMPWIN(1),  CARDD(COVER), &
                             0.D0,    OK                        )
            END IF

         END DO


!
! --- Case: ------------------------------------------------------
!
         DO I = 1, NINS

            TITLE = 'INST: #;  LEVEL: SEGMENT;  NEEDAV: TRUE; '      // &
                    'TIMSYS: SCLK; TOL: 0.D0; COVER starts out #.'

            CALL REPMI ( TITLE, '#', I,      TITLE )
            CALL REPMC ( TITLE, '#', CVSTAT, TITLE )
            CALL TCASE ( TITLE )

!
!           Initialize COVER.
!
            CALL SSIZED ( MAXCOV, COVER )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Make a copy of the expected window.
!
            CALL COPYD ( XAVSEG(LBCELL,I),  TMPWIN )

            IF ( L .EQ. 2 ) THEN
!
!              Insert an interval into COVER.  This same interval
!              must be added to each window containing expected
!              coverage.
!
               CALL WNINSD ( 1.D6, 1.D7, COVER )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              The same interval is expected to appear in the output.
!
               CALL WNINSD ( 1.D6, 1.D7, TMPWIN )
               CALL CHCKXC ( .FALSE., ' ', OK )

            END IF

            CALL CKCOV ( CK,   INST(I), .TRUE., 'SEGMENT', &
                         0.D0, 'SCLK',  COVER              )

            CALL CHCKXC ( .FALSE., ' ', OK )
!
!           Check cardinality of coverage window.
!
            CALL CHCKSI ( 'CARDD(COVER)', CARDD( COVER  ), &
                          '=',            CARDD( TMPWIN ), &
                          0,              OK                         )

!
!           Check coverage window.
!
            CALL CHCKAD ( 'COVER', COVER(1), &
                          '=',     TMPWIN(1),  CARDD(COVER), &
                          0.D0,    OK                          )


            IF ( I .EQ. 6 ) THEN

               CALL SCARDD ( 0, TMPWIN )
               CALL CHCKXC ( .FALSE., ' ', OK )

               IF ( L .EQ. 2 ) THEN
!
!                 The same interval is expected to appear in the output.
!
                  CALL WNINSD ( 1.D6, 1.D7, TMPWIN )
                  CALL CHCKXC ( .FALSE., ' ', OK )

               END IF

!
!              Compare CKCOV results against those from T_CKCOV.
!
               CALL T_CKCOV ( CK,   INST(I), .TRUE., 'SEGMENT', &
                              0.D0, 'SCLK',  TMPWIN             )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Check cardinality of coverage window.
!
               CALL CHCKSI ( 'CARDD(COVER)', CARDD( COVER  ), &
                             '=',            CARDD( TMPWIN ), &
                             0,              OK                )

!
!              Check coverage window.
!
               CALL CHCKAD ( 'COVER', COVER(1), &
                             '=',     TMPWIN(1),  CARDD(COVER), &
                             0.D0,    OK                        )
            END IF

         END DO



!
! --- Case: ------------------------------------------------------
!
         DO I = 1, NINS

            TITLE = 'INST: #;  LEVEL: SEGMENT;  NEEDAV: FALSE; '      // &
                    'TIMSYS: SCLK; TOL: 1.D0; COVER starts out #.'

            CALL REPMI ( TITLE, '#', I,      TITLE )
            CALL REPMC ( TITLE, '#', CVSTAT, TITLE )
            CALL TCASE ( TITLE )


!
!           Initialize COVER.
!
            CALL SSIZED ( MAXCOV, COVER )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Adjust our expected result window by TOL.
!
            CALL COPYD ( XCVSEG(LBCELL,I), TMPWIN )

            TOL = 1.D0


            CALL WNEXPD ( TOL, TOL, TMPWIN )
!
!           Make sure the window doesn't start with a negative tick
!           value.
!
            TMPWIN(1) = MAX ( TMPWIN(1), 0.D0 )

            IF ( L .EQ. 2 ) THEN
!
!              Insert an interval into COVER.  This same interval
!              must be added to each window containing expected
!              coverage.
!
               CALL WNINSD ( 1.D6, 1.D7, COVER )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              The same interval is expected to appear in the output.
!
               CALL WNINSD ( 1.D6, 1.D7, TMPWIN )
               CALL CHCKXC ( .FALSE., ' ', OK )

            END IF


            CALL CKCOV ( CK,   INST(I), .FALSE., 'SEGMENT', &
                         TOL, 'SCLK',   COVER               )

            CALL CHCKXC ( .FALSE., ' ', OK )
!
!           Check cardinality of coverage window.
!
            CALL CHCKSI ( 'CARDD(COVER)', CARDD( COVER ), &
                          '=',            CARDD( TMPWIN), &
                          0,              OK              )

!
!           Check coverage window.
!
            CALL CHCKAD ( 'COVER', COVER(1), &
                          '=',     TMPWIN(1),  CARDD(COVER), &
                          0.D0,    OK                     )


            IF ( I .EQ. 6 ) THEN

               CALL SCARDD ( 0, TMPWIN )
               CALL CHCKXC ( .FALSE., ' ', OK )

               IF ( L .EQ. 2 ) THEN
!
!                 The same interval is expected to appear in the output.
!
                  CALL WNINSD ( 1.D6, 1.D7, TMPWIN )
                  CALL CHCKXC ( .FALSE., ' ', OK )

               END IF

!
!              Compare CKCOV results against those from T_CKCOV.
!
               CALL T_CKCOV ( CK,   INST(I), .FALSE., 'SEGMENT', &
                              TOL,  'SCLK',  TMPWIN             )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Check cardinality of coverage window.
!
               CALL CHCKSI ( 'CARDD(COVER)', CARDD( COVER  ), &
                             '=',            CARDD( TMPWIN ), &
                             0,              OK                )

!
!              Check coverage window.
!
               CALL CHCKAD ( 'COVER', COVER(1), &
                             '=',     TMPWIN(1),  CARDD(COVER), &
                             0.D0,    OK                        )
            END IF

         END DO



         DO I = 1, NINS
!
! --- Case: ------------------------------------------------------
!
            TITLE = 'INST: #;  LEVEL: SEGMENT;  NEEDAV: FALSE; '      // &
                    'TIMSYS: TDB; TOL: 0.D0; COVER: #.'

            CALL REPMI ( TITLE, '#', I,      TITLE )
            CALL REPMC ( TITLE, '#', CVSTAT, TITLE )
            CALL TCASE ( TITLE )


!
!           Initialize COVER.
!
            CALL SSIZED ( MAXCOV, COVER )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Make a copy of the expected window.
!
            CALL COPYD ( XCVSEG(LBCELL,I),  TMPWIN )

!
!           Convert the expected window to TDB.
!
            DO J = 1, CARDD(TMPWIN)

               CALL SCT2E ( CLKID(I), TMPWIN(J),   ET )
               TMPWIN(J)  =  ET

               CALL CHCKXC ( .FALSE., ' ', OK )

            END DO


            IF ( L .EQ. 2 ) THEN
!
!              Insert an interval into COVER.  This same interval
!              must be added to each window containing expected
!              coverage.
!
               CALL WNINSD ( 1.D6, 1.D7, COVER )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              The same interval is expected to appear in the output.
!
               CALL WNINSD ( 1.D6, 1.D7, TMPWIN )
               CALL CHCKXC ( .FALSE., ' ', OK )

            END IF

            CALL CKCOV ( CK,   INST(I), .FALSE., 'SEGMENT', &
                         0.D0, 'TDB',   COVER               )

            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Check cardinality of coverage window.
!
            CALL CHCKSI ( 'CARDD(COVER)', CARDD( COVER  ), &
                          '=',            CARDD( TMPWIN ), &
                          0,              OK              )

!
!           Check coverage window.
!
            CALL CHCKAD ( 'COVER', COVER(1), &
                          '=',     TMPWIN(1),  CARDD(TMPWIN), &
                          0.D0,    OK                        )

            IF ( I .EQ. 6 ) THEN

               CALL SCARDD ( 0, TMPWIN )
               CALL CHCKXC ( .FALSE., ' ', OK )

               IF ( L .EQ. 2 ) THEN
!
!                 The same interval is expected to appear in the output.
!
                  CALL WNINSD ( 1.D6, 1.D7, TMPWIN )
                  CALL CHCKXC ( .FALSE., ' ', OK )

               END IF

!
!              Compare CKCOV results against those from T_CKCOV.
!
               CALL T_CKCOV ( CK,   INST(I), .FALSE., 'SEGMENT', &
                              0.D0, 'TDB',   TMPWIN             )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Check cardinality of coverage window.
!
               CALL CHCKSI ( 'CARDD(COVER)', CARDD( COVER  ), &
                             '=',            CARDD( TMPWIN ), &
                             0,              OK                )

!
!              Check coverage window.
!
               CALL CHCKAD ( 'COVER', COVER(1), &
                             '=',     TMPWIN(1),  CARDD(COVER), &
                             0.D0,    OK                        )
            END IF

         END DO




         DO I = 1, NINS
!
! --- Case: ------------------------------------------------------
!
            TITLE = 'INST: #;  LEVEL: SEGMENT;  NEEDAV: FALSE; '      // &
                    'TIMSYS: TDB; TOL: 1.D0; COVER: #.'

            CALL REPMI ( TITLE, '#', I,      TITLE )
            CALL REPMC ( TITLE, '#', CVSTAT, TITLE )
            CALL TCASE ( TITLE )


!
!           Initialize COVER.
!
            CALL SSIZED ( MAXCOV, COVER )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Adjust our expected result window by TOL.
!
            CALL COPYD ( XCVSEG(LBCELL,I), TMPWIN )

            TOL = 1.D0

            CALL WNEXPD ( TOL, TOL, TMPWIN )

!
!           Make sure the window doesn't start with a negative tick
!           value.
!
            TMPWIN(1) = MAX ( TMPWIN(1), 0.D0 )

!
!           Convert the expected window to TDB.
!
            DO J = 1, CARDD(TMPWIN)

               CALL SCT2E ( CLKID(I), TMPWIN(J),   ET )
               TMPWIN(J)  =  ET

            END DO


            IF ( L .EQ. 2 ) THEN
!
!              Insert an interval into COVER.  This same interval
!              must be added to each window containing expected
!              coverage.
!
               CALL WNINSD ( 1.D6, 1.D7, COVER )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              The same interval is expected to appear in the output.
!
               CALL WNINSD ( 1.D6, 1.D7, TMPWIN )
               CALL CHCKXC ( .FALSE., ' ', OK )

            END IF

            CALL CKCOV ( CK,   INST(I), .FALSE., 'SEGMENT', &
                         1.D0, 'TDB',   COVER               )

            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Check cardinality of coverage window.
!
            CALL CHCKSI ( 'CARDD(COVER)', CARDD( COVER  ), &
                          '=',            CARDD( TMPWIN ), &
                          0,              OK              )

!
!           Check coverage window.
!
            CALL CHCKAD ( 'COVER', COVER(1), &
                          '=',     TMPWIN(1),  CARDD(TMPWIN), &
                          0.D0,    OK                        )




            IF ( I .EQ. 6 ) THEN

               CALL SCARDD ( 0, TMPWIN )
               CALL CHCKXC ( .FALSE., ' ', OK )

               IF ( L .EQ. 2 ) THEN
!
!                 The same interval is expected to appear in the output.
!
                  CALL WNINSD ( 1.D6, 1.D7, TMPWIN )
                  CALL CHCKXC ( .FALSE., ' ', OK )

               END IF

!
!              Compare CKCOV results against those from T_CKCOV.
!
               CALL T_CKCOV ( CK,   INST(I), .FALSE., 'SEGMENT', &
                              1.D0, 'TDB',   TMPWIN             )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Check cardinality of coverage window.
!
               CALL CHCKSI ( 'CARDD(COVER)', CARDD( COVER  ), &
                             '=',            CARDD( TMPWIN ), &
                             0,              OK                )

!
!              Check coverage window.
!
               CALL CHCKAD ( 'COVER', COVER(1), &
                             '=',     TMPWIN(1),  CARDD(COVER), &
                             0.D0,    OK                        )
            END IF


         END DO

!
!        INTERVAL level tests:
!
!
!        Now we'll repeat the previous tests, but this time the
!        coverage will be summarized at the interval level.
!

         DO I = 1, NINS
!
! --- Case: ------------------------------------------------------
!
            TITLE = 'INST: #;  LEVEL: INTERVAL;  NEEDAV: FALSE; ' // &
                    'TIMSYS: SCLK TOL: 0.D0; COVER: #.'

            CALL REPMI ( TITLE, '#', I,      TITLE )
            CALL REPMC ( TITLE, '#', CVSTAT, TITLE )
            CALL TCASE ( TITLE )

!
!           Initialize COVER.
!
            CALL SSIZED ( MAXCOV, COVER )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Make a copy of the expected window.
!
            CALL COPYD ( XCVINT(LBCELL,I),  TMPWIN )

            IF ( L .EQ. 2 ) THEN
!
!              Insert an interval into COVER.  This same interval
!              must be added to each window containing expected
!              coverage.
!
               CALL WNINSD ( 1.D6, 1.D7, COVER )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              The same interval is expected to appear in the output.
!
               CALL WNINSD ( 1.D6, 1.D7, TMPWIN )
               CALL CHCKXC ( .FALSE., ' ', OK )

            END IF

            CALL CKCOV ( CK,   INST(I), .FALSE., 'INTERVAL', &
                         0.D0, 'SCLK',  COVER              )

            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Check cardinality of coverage window.
!
            CALL CHCKSI ( 'CARDD(COVER)', CARDD( COVER  ), &
                          '=',            CARDD( TMPWIN ), &
                          0,              OK               )

!
!           Check coverage window.
!
            CALL CHCKAD ( 'COVER', COVER(1), &
                          '=',     TMPWIN(1),  CARDD(COVER), &
                          0.D0,    OK                       )


            IF ( I .EQ. 6 ) THEN

               CALL SCARDD ( 0, TMPWIN )
               CALL CHCKXC ( .FALSE., ' ', OK )

               IF ( L .EQ. 2 ) THEN
!
!                 The same interval is expected to appear in the output.
!
                  CALL WNINSD ( 1.D6, 1.D7, TMPWIN )
                  CALL CHCKXC ( .FALSE., ' ', OK )

               END IF

!
!              Compare CKCOV results against those from T_CKCOV.
!
               CALL T_CKCOV ( CK,   INST(I), .FALSE., 'INTERVAL', &
                              0.D0, 'SCLK',   TMPWIN             )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Check cardinality of coverage window.
!
               CALL CHCKSI ( 'CARDD(COVER)', CARDD( COVER  ), &
                             '=',            CARDD( TMPWIN ), &
                             0,              OK                )

!
!              Check coverage window.
!
               CALL CHCKAD ( 'COVER', COVER(1), &
                             '=',     TMPWIN(1),  CARDD(COVER), &
                             0.D0,    OK                        )
            END IF

         END DO


!
! --- Case: ------------------------------------------------------
!
         DO I = 1, NINS

            TITLE = 'INST: #;  LEVEL: INTERVAL;  NEEDAV: TRUE; ' // &
                    'TIMSYS: SCLK TOL: 0.D0; COVER: #.'

            CALL REPMI ( TITLE, '#', I,      TITLE )
            CALL REPMC ( TITLE, '#', CVSTAT, TITLE )
            CALL TCASE ( TITLE )

!
!           Initialize COVER.
!
            CALL SSIZED ( MAXCOV, COVER )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Make a copy of the expected window.
!
            CALL COPYD ( XAVINT(LBCELL,I),  TMPWIN )

            IF ( L .EQ. 2 ) THEN
!
!              Insert an interval into COVER.  This same interval
!              must be added to each window containing expected
!              coverage.
!
               CALL WNINSD ( 1.D6, 1.D7, COVER )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              The same interval is expected to appear in the output.
!
               CALL WNINSD ( 1.D6, 1.D7, TMPWIN )
               CALL CHCKXC ( .FALSE., ' ', OK )

            END IF

            CALL CKCOV ( CK,   INST(I), .TRUE., 'INTERVAL', &
                         0.D0, 'SCLK',  COVER              )

            CALL CHCKXC ( .FALSE., ' ', OK )
!
!           Check cardinality of coverage window.
!
            CALL CHCKSI ( 'CARDD(COVER)', CARDD( COVER  ), &
                          '=',            CARDD( TMPWIN ), &
                          0,              OK               )

!
!           Check coverage window.
!
            CALL CHCKAD ( 'COVER', COVER(1), &
                          '=',     TMPWIN(1),  CARDD(COVER), &
                          0.D0,    OK                          )

            IF ( I .EQ. 6 ) THEN

               CALL SCARDD ( 0, TMPWIN )
               CALL CHCKXC ( .FALSE., ' ', OK )

               IF ( L .EQ. 2 ) THEN
!
!                 The same interval is expected to appear in the output.
!
                  CALL WNINSD ( 1.D6, 1.D7, TMPWIN )
                  CALL CHCKXC ( .FALSE., ' ', OK )

               END IF

!
!              Compare CKCOV results against those from T_CKCOV.
!
               CALL T_CKCOV ( CK,   INST(I), .TRUE., 'INTERVAL', &
                              0.D0, 'SCLK',   TMPWIN             )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Check cardinality of coverage window.
!
               CALL CHCKSI ( 'CARDD(COVER)', CARDD( COVER  ), &
                             '=',            CARDD( TMPWIN ), &
                             0,              OK                )

!
!              Check coverage window.
!
               CALL CHCKAD ( 'COVER', COVER(1), &
                             '=',     TMPWIN(1),  CARDD(COVER), &
                             0.D0,    OK                        )
            END IF

         END DO



!
! --- Case: ------------------------------------------------------
!
         DO I = 1, NINS

            TITLE = 'INST: #;  LEVEL: INTERVAL;  NEEDAV: FALSE; ' // &
                    'TIMSYS: SCLK TOL: 1.D0; COVER: #.'

            CALL REPMI ( TITLE, '#', I,      TITLE )
            CALL REPMC ( TITLE, '#', CVSTAT, TITLE )
            CALL TCASE ( TITLE )

!
!           Initialize COVER.
!
            CALL SSIZED ( MAXCOV, COVER )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Adjust our expected result window by TOL.
!
            CALL COPYD ( XCVINT(LBCELL,I), TMPWIN )

            TOL = 1.D0

            CALL WNEXPD ( TOL, TOL, TMPWIN )

!
!           Make sure the window doesn't start with a negative tick
!           value.
!
            TMPWIN(1) = MAX ( TMPWIN(1), 0.D0 )


           IF ( L .EQ. 2 ) THEN
!
!              Insert an interval into COVER.  This same interval
!              must be added to each window containing expected
!              coverage.
!
               CALL WNINSD ( 1.D6, 1.D7, COVER )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              The same interval is expected to appear in the output.
!
               CALL WNINSD ( 1.D6, 1.D7, TMPWIN )
               CALL CHCKXC ( .FALSE., ' ', OK )

            END IF

            CALL CKCOV ( CK,   INST(I), .FALSE., 'INTERVAL', &
                         TOL, 'SCLK',   COVER               )

            CALL CHCKXC ( .FALSE., ' ', OK )
!
!           Check cardinality of coverage window.
!
            CALL CHCKSI ( 'CARDD(COVER)', CARDD( COVER ), &
                          '=',            CARDD( TMPWIN), &
                          0,              OK              )

!
!           Check coverage window.
!
            CALL CHCKAD ( 'COVER', COVER(1), &
                          '=',     TMPWIN(1),  CARDD(COVER), &
                          0.D0,    OK                     )

            IF ( I .EQ. 6 ) THEN

               CALL SCARDD ( 0, TMPWIN )
               CALL CHCKXC ( .FALSE., ' ', OK )

               IF ( L .EQ. 2 ) THEN
!
!                 The same interval is expected to appear in the output.
!
                  CALL WNINSD ( 1.D6, 1.D7, TMPWIN )
                  CALL CHCKXC ( .FALSE., ' ', OK )

               END IF

!
!              Compare CKCOV results against those from T_CKCOV.
!
               CALL T_CKCOV ( CK,   INST(I), .FALSE., 'INTERVAL', &
                              TOL,  'SCLK',   TMPWIN             )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Check cardinality of coverage window.
!
               CALL CHCKSI ( 'CARDD(COVER)', CARDD( COVER  ), &
                             '=',            CARDD( TMPWIN ), &
                             0,              OK                )

!
!              Check coverage window.
!
               CALL CHCKAD ( 'COVER', COVER(1), &
                             '=',     TMPWIN(1),  CARDD(COVER), &
                             0.D0,    OK                        )
            END IF

         END DO




         DO I = 1, NINS
!
! --- Case: ------------------------------------------------------
!
            TITLE = 'INST: #;  LEVEL: INTERVAL  NEEDAV: FALSE; ' // &
                    'TIMSYS: TDB; TOL: 0.D0; COVER: #.'

            CALL REPMI ( TITLE, '#', I,      TITLE )
            CALL REPMC ( TITLE, '#', CVSTAT, TITLE )
            CALL TCASE ( TITLE )


!
!           Initialize COVER.
!
            CALL SSIZED ( MAXCOV, COVER )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Make a copy of the expected window.
!
            CALL COPYD ( XCVINT(LBCELL,I),  TMPWIN )

!
!           Convert the expected window to TDB.
!
            DO J = 1, CARDD(TMPWIN)

               CALL SCT2E ( CLKID(I), TMPWIN(J),   ET )
               TMPWIN(J)  =  ET

            END DO


            IF ( L .EQ. 2 ) THEN
!
!              Insert an interval into COVER.  This same interval
!              must be added to each window containing expected
!              coverage.
!
               CALL WNINSD ( 1.D6, 1.D7, COVER )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              The same interval is expected to appear in the output.
!
               CALL WNINSD ( 1.D6, 1.D7, TMPWIN )
               CALL CHCKXC ( .FALSE., ' ', OK )

            END IF

            CALL CKCOV ( CK,   INST(I), .FALSE., 'INTERVAL', &
                         0.D0, 'TDB',   COVER               )

            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Check cardinality of coverage window.
!
            CALL CHCKSI ( 'CARDD(COVER)', CARDD( COVER  ), &
                          '=',            CARDD( TMPWIN ), &
                          0,              OK              )

!
!           Check coverage window.
!
            CALL CHCKAD ( 'COVER', COVER(1), &
                          '=',     TMPWIN(1),  CARDD(TMPWIN), &
                          0.D0,    OK                        )


            IF ( I .EQ. 6 ) THEN

               CALL SCARDD ( 0, TMPWIN )
               CALL CHCKXC ( .FALSE., ' ', OK )

               IF ( L .EQ. 2 ) THEN
!
!                 The same interval is expected to appear in the output.
!
                  CALL WNINSD ( 1.D6, 1.D7, TMPWIN )
                  CALL CHCKXC ( .FALSE., ' ', OK )

               END IF

!
!              Compare CKCOV results against those from T_CKCOV.
!
               CALL T_CKCOV ( CK,   INST(I), .FALSE., 'INTERVAL', &
                              0.D0, 'TDB',   TMPWIN             )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Check cardinality of coverage window.
!
               CALL CHCKSI ( 'CARDD(COVER)', CARDD( COVER  ), &
                             '=',            CARDD( TMPWIN ), &
                             0,              OK                )

!
!              Check coverage window.
!
               CALL CHCKAD ( 'COVER', COVER(1), &
                             '=',     TMPWIN(1),  CARDD(COVER), &
                             0.D0,    OK                        )
            END IF

         END DO



         DO I = 1, NINS
!
! --- Case: ------------------------------------------------------
!
            TITLE = 'INST: #;  LEVEL: INTERVAL  NEEDAV: FALSE; ' // &
                    'TIMSYS: TDB; TOL: 1.D0; COVER: #.'

            CALL REPMI ( TITLE, '#', I,      TITLE )
            CALL REPMC ( TITLE, '#', CVSTAT, TITLE )
            CALL TCASE ( TITLE )

!
!           Initialize COVER.
!
            CALL SSIZED ( MAXCOV, COVER )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Adjust our expected result window by TOL.
!
            CALL COPYD ( XCVINT(LBCELL,I), TMPWIN )

            TOL = 1.D0

            CALL WNEXPD ( TOL, TOL, TMPWIN )

!
!           Make sure the window doesn't start with a negative tick
!           value.
!
            TMPWIN(1) = MAX ( TMPWIN(1), 0.D0 )

!
!           Convert the expected window to TDB.
!
            DO J = 1, CARDD(TMPWIN)

               CALL SCT2E ( CLKID(I), TMPWIN(J),   ET )
               TMPWIN(J)  =  ET

            END DO


            IF ( L .EQ. 2 ) THEN
!
!              Insert an interval into COVER.  This same interval
!              must be added to each window containing expected
!              coverage.
!
               CALL WNINSD ( 1.D6, 1.D7, COVER )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              The same interval is expected to appear in the output.
!
               CALL WNINSD ( 1.D6, 1.D7, TMPWIN )
               CALL CHCKXC ( .FALSE., ' ', OK )

            END IF

            CALL CKCOV ( CK,   INST(I), .FALSE., 'INTERVAL', &
                         1.D0, 'TDB',   COVER               )

            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Check cardinality of coverage window.
!
            CALL CHCKSI ( 'CARDD(COVER)', CARDD( COVER  ), &
                          '=',            CARDD( TMPWIN ), &
                          0,              OK              )

!
!           Check coverage window.
!
            CALL CHCKAD ( 'COVER', COVER(1), &
                          '=',     TMPWIN(1),  CARDD(TMPWIN), &
                          0.D0,    OK                        )


            IF ( I .EQ. 6 ) THEN

               CALL SCARDD ( 0, TMPWIN )
               CALL CHCKXC ( .FALSE., ' ', OK )

               IF ( L .EQ. 2 ) THEN
!
!                 The same interval is expected to appear in the output.
!
                  CALL WNINSD ( 1.D6, 1.D7, TMPWIN )
                  CALL CHCKXC ( .FALSE., ' ', OK )

               END IF

!
!              Compare CKCOV results against those from T_CKCOV.
!
               CALL T_CKCOV ( CK,   INST(I), .FALSE., 'INTERVAL', &
                              1.D0, 'TDB',   TMPWIN             )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Check cardinality of coverage window.
!
               CALL CHCKSI ( 'CARDD(COVER)', CARDD( COVER  ), &
                             '=',            CARDD( TMPWIN ), &
                             0,              OK                )

!
!              Check coverage window.
!
               CALL CHCKAD ( 'COVER', COVER(1), &
                             '=',     TMPWIN(1),  CARDD(COVER), &
                             0.D0,    OK                        )
            END IF

         END DO


      END DO

!
!     Error cases:
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Try to find coverage using time system UTC.' )

      CALL CKCOV ( CK,     INST(2),  .FALSE., 'SEGMENT', &
                   0.D0,   'UTC',    COVER              )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Try to find coverage using negative tolerance.' )

      CALL CKCOV ( CK,     INST(2), .FALSE., 'SEGMENT', &
                   -1.D0,  'TDB',    COVER              )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Try to find coverage using level "file".' )

      CALL CKCOV ( CK,     INST(2), .FALSE., 'FILE', &
                   0.D0,   'TDB',   COVER             )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Try to find coverage for a transfer CK.' )

      CALL TXTOPN ( XFRCK, XUNIT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBT  ( CK, XUNIT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CLOSE ( XUNIT )

      CALL CKCOV ( XFRCK, INST(1), .FALSE., 'SEGMENT', &
                   0.D0,  'SCLK',  COVER                )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFORMAT)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Try to find coverage for an SPK.' )


      CALL TSTSPK ( SPK, .FALSE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CKCOV  ( SPK,  INST(1), .FALSE., 'SEGMENT', &
                    0.D0, 'SCLK',  COVER               )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFILETYPE)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Try to find coverage for an EK.' )

      CALL TSTEK  ( EK, 1, 20, .FALSE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CKCOV  ( EK,   INST(1), .FALSE., 'SEGMENT', &
                    0.D0, 'SCLK',  COVER               )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDARCHTYPE)', OK )



! ******************************************************
! ******************************************************
! ******************************************************
!     CKOBJ tests
! ******************************************************
! ******************************************************
! ******************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Find objects in our test CK.' )

      CALL SSIZEI ( NINS, IDS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZEI ( NINS, XIDS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, NINS
         CALL INSRTI ( INST(I), XIDS )
      END DO

      CALL CKOBJ  ( CK, IDS )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check cardinality of object set.
!
      CALL CHCKSI ( 'CARDI(IDS)', CARDI( IDS  ), &
                       '=',       CARDI( XIDS ), &
                       0,         OK             )

!
!     Check object set.
!
      CALL CHCKAI ( 'IDS',      IDS(1), &
                    '=',        XIDS(1),  CARDI(XIDS),   OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Find objects in our test CK.  Start with ' // &
                   'non-empty ID set.'                          )

      CALL SSIZEI ( NINS+1, IDS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZEI ( NINS+1, XIDS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL INSRTI ( -1000000, XIDS )

      DO I = 1, NINS
         CALL INSRTI ( INST(I), XIDS )
      END DO

      CALL INSRTI ( -1000000, IDS )

      CALL CKOBJ  ( CK, IDS )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check cardinality of object set.
!
      CALL CHCKSI ( 'CARDI(IDS)', CARDI( IDS  ), &
                       '=',       CARDI( XIDS ), &
                       0,         OK             )

!
!     Check object set.
!
      CALL CHCKAI ( 'IDS',      IDS(1), &
                    '=',        XIDS(1),  CARDI(XIDS),   OK )
!
!     Error cases:
!


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Try to find objects for a transfer CK.' )

!
!     Initialize the IDS set.
!
      CALL SSIZEI ( NINS, IDS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CKOBJ  ( XFRCK, IDS )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFORMAT)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Try to find objects for an SPK.' )

      CALL CKOBJ ( SPK, IDS )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFILETYPE)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Try to find objects for an EK.' )


      CALL CKOBJ  ( EK, IDS )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDARCHTYPE)', OK )

!
!     Clean up.
!
      CALL DELFIL ( CK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( XFRCK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( EK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_SUCCESS ( OK )
      RETURN
      END
