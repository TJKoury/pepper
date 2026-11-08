C$Procedure F_CKCOV ( CKCOV tests )

      SUBROUTINE F_CKCOV ( OK )

C$ Abstract
C
C     This routine tests the SPICELIB routines
C
C        CKCOV
C        CKOBJ
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
C     None.
C
C$ Keywords
C
C     None.
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'ck05.inc'
      INCLUDE 'ck06.inc'
      INCLUDE 'ckparam.inc'

      LOGICAL               OK

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OK         O   logical indicating test status.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     OK         is a logical that indicates the test status to the
C                caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting.  The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     None.
C
C$ Examples
C
C     None.
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
C    E. D. Wright   (JPL)
C    N. J. Bachman  (JPL)
C
C$ Version
C
C-    TSPICE Version 3.0.0, 05-FEB-2014 (NJB) (EDW)
C
C        Updated to support CK type 6.
C
C        All variables to SAVE for f2c'd version compiled with Borland.
C        Implemented a proper SPICE header.
C
C-    TSPICE Version 2.0.0, 30-NOV-2007 (NJB)
C
C        Bug fixes: changed interpolation interval start
C        and stop times for segments 3 and 5 to be consistent
C        with pattern described in accompanying comments.
C        Also modified SCLK kernel data generated at run-time
C        so that least significant clock field is consistent
C        with the "tick per second" parameter TIKPER.
C
C        These changes were made to make the test code more
C        easily understood and more maintainable. They
C        don't materially affect the test code's ability
C        to exercise the target code.
C
C-    TSPICE Version 1.0.0, 07-JAN-2005 (NJB)
C
C-&

C$ Index_Entries
C
C   Test CKCOV, CKOBJ
C
C-&

C
C     SPICELIB functions
C
      INTEGER               CARDD
      INTEGER               CARDI

      LOGICAL               EXISTS
      LOGICAL               ODD

C
C     Local parameters
C
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

C
C     MAXPKT is the size of the largest CK type 5 subtype's packet.
C
      INTEGER               MAXPKT
      PARAMETER           ( MAXPKT = C05PS2 )

C
C     Local variables
C
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

C
C     Saved variables
C
      SAVE

C
C     Initial values
C
      DATA                  INST   / -1000, -2000, -3000, 
     .                               -4000, -5000, -6000 /

      DATA                  NSEG   /  3,    3,     4,     
     .                                4,    4,     4     /

      DATA                  NR     /  4,    99,    199,   
     .                                2399, 2399,  2399  /

      DATA                  IVLN   /  4,    3,     7, 
     .                                20,   20,    20    /

      DATA                  TIKPER /  2,    4,     8,
     .                                16,   32,    32    /

      DATA                  Z      /  0.D0, 0.D0,  1.D0  /

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_CKCOV' )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Setup:  create and load SCLK definitions ' //
     .             'for each instrument.'                      )

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

C
C        Insert the modulus for the second SCLK field for the
C        current instrument.
C
         CALL REPMI ( DEFTXT(5), '**', TIKPER(I), DEFTXT(5) )

         CALL LMPOOL ( DEFTXT, DEFSIZ )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


C
C     We'll need a leapseconds kernel too.
C
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Setup:  create CK file.' )


C
C     Create a CK file with data for five objects.
C
      IF ( EXISTS(CK) ) THEN
         CALL DELFIL(CK)
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL CKOPN ( CK, CK, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Initialize the expected coverage windows.
C
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

C
C     Initializations to make compilers happy.
C
      FIRST = 0.D0
      LAST  = 0.D0
      T3END = 0.D0

C
C     For each instrument, we'll create a sequence of segments. Because
C     we have CKCOV code (in some cases, the code resides in supporting
C     utilities) unique to each data type, we'll create segments of all
C     data types:  all of the segments for the Ith instrument will of
C     data type I.  Characteristics of the segments such as presence of
C     angular velocity, spacing of epochs and interpolation intervals,
C     spacing of segments, and time ordering of segments relative to
C     each other will vary.
C
      DO I = 1, NINS

         DO J = 1, NSEG(I)
C
C           Create segments for instrument I.  All segments for
C           instrument I will use data type I.
C
C           Set the number of records in the Jth segment for instrument
C           I.
C
            NREC   = NR ( J )

C
C           Set the number of pointing records per interpolation
C           interval for instrument I.
C
            NIVREC = IVLN ( J )

C
C           The flag USEAV indicates how the angular velocity flag
C           will be set.  Odd-indexed segments get angular velocity.
C
            USEAV  = ODD ( J )
C
C           Proceed to create the Jth segment for instrument I.
C           The following code is data-type dependent.
C
            IF ( I .EQ. 1 ) THEN
C
C              This is the CK type 1 case.
C
C              The segments we create will be separated by a 3 tick gap.
C              Records will be 3*J ticks apart.
C
C              Set segment start and epochs.
C
               IF ( J .EQ. 1 ) THEN
                  FIRST = 0.D0
               ELSE
C
C                 LAST is left over from the previous J-loop iteration.
C
                  FIRST = LAST + 3.D0
               END IF

C
C              Set EPOCHS, QUATS, and AVVS.
C
C              Pointing data are not relevant for these tests,
C              but having distinct entries could be helpful for
C              debugging.  The Kth entry will be a frame rotation
C              by K milliradians about the Z-axis.
C
               DO K = 1, NREC
C
C                 As stated above, records will be 3*J ticks apart.
C
                  EPOCHS(K) =  FIRST + DBLE ( 3*J*(K-1) )

C                 The angle required by AXISAR is the negative of
C                 the frame rotation angle.
C
                  DLTANG = 1.D-3
                  ANGLE  =  - ( K * DLTANG )

                  CALL AXISAR ( Z,    ANGLE, CMAT       )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL M2Q    ( CMAT,        QUATS(0,K) )
                  CALL CHCKXC ( .FALSE., ' ', OK )

C
C                 Set angular velocity to be consistent with
C                 the rotation data.  Remember angular velocity
C                 units are radians/sec, so we must multiply
C                 radians/tick by ticks/second for instrument I.
C
                  CALL VSCL ( TIKPER(I)*DLTANG/(3*J), Z, AVVS(1,K) )

               END DO

C
C              Set segment end time.
C
               LAST = EPOCHS(NREC)

C
C              Add the segment's coverage interval to our segment-level
C              expected coverage window for the Ith instrument.
C
               CALL WNINSD ( FIRST, LAST, XCVSEG(LBCELL,I) )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              If we're providing angular velocity for this segment,
C              then this segment contributes to the coverage window
C              for the angular-velocity only segments at the interval
C              level.
C
               IF ( USEAV ) THEN
                  CALL WNINSD ( FIRST, LAST, XAVSEG(LBCELL,I) )
                  CALL CHCKXC ( .FALSE., ' ', OK )
               END IF

C
C              The singleton intervals defined by the pointing epochs
C              act as interpolation intervals for type 1 segments.
C              Add the interpolation intervals to our interval-level
C              expected coverage window for the Ith instrument.
C
               DO K = 1, NREC
                  CALL WNINSD ( EPOCHS(K), EPOCHS(K), XCVINT(LBCELL,I) )
                  CALL CHCKXC ( .FALSE., ' ', OK )
               END DO

C
C              If we're providing angular velocity for this segment,
C              then this segment contributes to the coverage window
C              for the angular-velocity only segments at the interval
C              level.
C
               IF ( USEAV ) THEN

                  DO K = 1, NREC

                     CALL WNINSD ( EPOCHS(K),
     .                             EPOCHS(K),
     .                             XAVINT(LBCELL,I) )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                  END DO

               END IF

C
C              Create segment ID.
C
               SEGID = 'Segment # for instrument #.'

               CALL REPMI  ( SEGID, '#', J, SEGID )
               CALL REPMI  ( SEGID, '#', I, SEGID )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Write the current segment to our CK.
C
               CALL CKW01 ( HANDLE,   FIRST,  LAST,   INST(I),
     .                      'J2000',  USEAV,  SEGID,  NREC,
     .                      EPOCHS,   QUATS,  AVVS            )
               CALL CHCKXC ( .FALSE., ' ', OK )



            ELSE IF ( I .EQ. 2 ) THEN
C
C              This is the CK type 2 case.
C
C              For type 2, angular velocity is present by definition.
C
               USEAV = .TRUE.

C
C              We're going to copy the data for the type 1 case, but
C              here, the segments we create will abut each other.
C              Records will be 2*J ticks apart.
C
C              Set segment start and epochs.
C
               IF ( J .EQ. 1 ) THEN
                  FIRST = 0.D0
               ELSE
C
C                 LAST is left over from the previous J-loop iteration.
C
                  FIRST = LAST
               END IF

C
C              Set EPOCHS, QUATS, and AVVS.
C
C              Pointing data are not relevant for these tests,
C              but having distinct entries could be helpful for
C              debugging.  The Kth entry will be a frame rotation
C              by K milliradians about the Z-axis.
C
               DO K = 1, NREC
C
C                 As stated above, records will be 2*J ticks apart.
C
                  EPOCHS(K) =  FIRST + DBLE ( (K-1)*J*2 )

C
C                 Each interpolation interval will be 1 tick long.
C
                  ENDS(K)   =  EPOCHS(K) + 1.D0


C                 The angle required by AXISAR is the negative of
C                 the frame rotation angle.
C
                  DLTANG = 1.D-3
                  ANGLE  =  - ( K * DLTANG )

                  CALL AXISAR ( Z,    ANGLE, CMAT       )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL M2Q    ( CMAT,        QUATS(0,K) )
                  CALL CHCKXC ( .FALSE., ' ', OK )

C
C                 Set angular velocity to be consistent with
C                 the rotation data.  Remember angular velocity
C                 units are radians/sec, so we must multiply
C                 radians/tick by ticks/second for instrument I.
C
                  CALL VSCL ( TIKPER(I)*DLTANG/(2*J), Z, AVVS(1,K) )

C
C                 Set the clock rate in seconds per tick for the
C                 Kth interpolation interval.
C
                  RATES(K) =  1.D0 / TIKPER(I)

               END DO

C
C              Set segment end time.  Note that this is the end of
C              the last interpolation interval.
C
               LAST = ENDS(NREC)

C
C              Add the segment's coverage interval to our segment-level
C              expected coverage window for the Ith instrument.
C
               CALL WNINSD ( FIRST, LAST, XCVSEG(LBCELL,I) )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Since we're providing angular velocity for this segment,
C              then this segment contributes to the coverage window
C              for the angular-velocity only segments at the interval
C              level.
C
               CALL WNINSD ( FIRST, LAST, XAVSEG(LBCELL,I) )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Add the interpolation intervals to our interval-level
C              expected coverage window for the Ith instrument.
C
               DO K = 1, NREC
                  CALL WNINSD ( EPOCHS(K), ENDS(K), XCVINT(LBCELL,I) )
                  CALL WNINSD ( EPOCHS(K), ENDS(K), XAVINT(LBCELL,I) )
               END DO

C
C              Create segment ID.
C
               SEGID = 'Segment # for instrument #.'

               CALL REPMI  ( SEGID, '#', J, SEGID )
               CALL REPMI  ( SEGID, '#', I, SEGID )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Write the current segment to our CK.
C
               CALL CKW02 ( HANDLE,   FIRST,  LAST,  INST(I),
     .                      'J2000',  SEGID,  NREC,  EPOCHS,
     .                      ENDS,     QUATS,  AVVS,  RATES   )

               CALL CHCKXC ( .FALSE., ' ', OK )


            ELSE IF ( I .EQ. 3 ) THEN
C
C              This is the CK type 3 case.
C
C              The segments we create will be separated by a 3 tick gap.
C              Records will be J ticks apart.
C
C              Set segment start and epochs.
C
               IF ( J .EQ. 1 ) THEN
                  FIRST = 0.D0
               ELSE
C
C                 LAST is left over from the previous J-loop iteration.
C
                  FIRST = LAST + 3.D0
               END IF

C
C              Set EPOCHS, QUATS, and AVVS.
C
C              Pointing data are not relevant for these tests,
C              but having distinct entries could be helpful for
C              debugging.  The Kth entry will be a frame rotation
C              by K milliradians about the Z-axis.
C
               DO K = 1, NREC
C
C                 As stated above, records will be J ticks apart.
C
                  EPOCHS(K) =  FIRST + DBLE ( (K-1)*J )

C                 The angle required by AXISAR is the negative of
C                 the frame rotation angle.
C
                  DLTANG = 1.D-3
                  ANGLE  =  - ( K * DLTANG )

                  CALL AXISAR ( Z,    ANGLE, CMAT       )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL M2Q    ( CMAT,        QUATS(0,K) )
                  CALL CHCKXC ( .FALSE., ' ', OK )

C
C                 Set angular velocity to be consistent with
C                 the rotation data.  Remember angular velocity
C                 units are radians/sec, so we must multiply
C                 radians/tick by ticks/second for instrument I.
C
                  CALL VSCL ( TIKPER(I)*DLTANG/J, Z, AVVS(1,K) )

               END DO

C
C              Set segment end time.
C
               LAST = EPOCHS(NREC)

C
C              Add the segment's coverage interval to our segment-level
C              expected coverage window for the Ith instrument.
C
               CALL WNINSD ( FIRST, LAST, XCVSEG(LBCELL,I) )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              If we're providing angular velocity for this segment,
C              then this segment contributes to the coverage window
C              for the angular-velocity only segments at the interval
C              level.
C
               IF ( USEAV ) THEN
                  CALL WNINSD ( FIRST, LAST, XAVSEG(LBCELL,I) )
                  CALL CHCKXC ( .FALSE., ' ', OK )
               END IF

C
C              Set the interval start times.  The first epoch
C              is always the start of an interpolation interval
C              in these tests.  Each interval has length NIVREC
C              records.
C
               L = 0

               DO K = 1,  NREC,  NIVREC
C
C                 Increment the interpolation interval; set the
C                 start time.
C
                  L         = L + 1

                  STARTS(L) = EPOCHS(K)

C
C                 Keep track of the interval end times.
C
                  IF ( L .GT. 1 ) THEN
C
C                    Record the end time of the previous interval.
C
                     ENDS(L-1) = EPOCHS(K-1)

                  END IF

               END DO

C
C              Set the interpolation interval count.
C
               NSTART = L

C
C              The end time of the last interval is (in this test)
C              always the last epoch.
C
               ENDS(NSTART) = EPOCHS(NREC)

C
C              Add the interpolation intervals to our interval-level
C              expected coverage window for the Ith instrument.
C
               DO K = 1, NSTART
                  CALL WNINSD ( STARTS(K), ENDS(K), XCVINT(LBCELL,I) )
               END DO

C
C              If we're providing angular velocity for this segment,
C              then this segment contributes to the coverage window
C              for the angular-velocity only segments at the interval
C              level.
C
               IF ( USEAV ) THEN

                  DO K = 1, NSTART

                     CALL WNINSD ( STARTS(K),
     .                             ENDS(K),
     .                             XAVINT(LBCELL,I) )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                  END DO

               END IF

C
C              Create segment ID.
C
               SEGID = 'Segment # for instrument #.'

               CALL REPMI  ( SEGID, '#', J, SEGID )
               CALL REPMI  ( SEGID, '#', I, SEGID )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Write the current segment to our CK.
C
               CALL CKW03 ( HANDLE,   FIRST,  LAST,   INST(I),
     .                      'J2000',  USEAV,  SEGID,  NREC,
     .                      EPOCHS,   QUATS,  AVVS,   NSTART,  STARTS )
               CALL CHCKXC ( .FALSE., ' ', OK )


C
C              If this is the last type 3 segment, save the end
C              time of the segment.
C
               IF ( J .EQ. NSEG(3) ) THEN
                  T3END = LAST
               END IF
C
C
C

            ELSE IF ( I .EQ. 4 ) THEN
C
C              This is the CK type 4 case.
C
C              The segments we create will be separated by a 3 tick gap.
C              Records will be J ticks apart.
C
C              Set segment start and epochs.
C
               IF ( J .EQ. 1 ) THEN
                  FIRST = 0.D0
               ELSE
C
C                 LAST is left over from the previous J-loop iteration.
C
                  FIRST = LAST + 3.D0
               END IF

C
C              Create segment ID.
C
               SEGID = 'Segment # for instrument #.'

               CALL REPMI  ( SEGID, '#', J, SEGID )
               CALL REPMI  ( SEGID, '#', I, SEGID )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Begin the segment.
C
               CALL CKW04B ( HANDLE,  FIRST, INST(I),
     .                       'J2000', USEAV, SEGID   )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Define the start epochs for the packets.
C
               DO K = 1, NREC
C
C                 Packet starts will be 1000*J ticks apart.
C
                  EPOCHS(K) =  FIRST + DBLE ( (K-1)*J*1000 )

               END DO

C
C              The segment end time matches the end time of the last
C              packet.
C
               LAST = EPOCHS(NREC)

C
C              Define the data packets for the current segment;
C              add each one to the segment.
C
               DO K = 1, NREC
C
C                 Fill in the current packet.  The packet structure
C                 is as follows:
C
C                   ----------------------------------------------------
C                   | The midpoint of the approximation interval       |
C                   ----------------------------------------------------
C                   | The radius of the approximation interval         |
C                   ----------------------------------------------------
C                   | Number of coefficients for q0                    |
C                   ----------------------------------------------------
C                   | Number of coefficients for q1                    |
C                   ----------------------------------------------------
C                   | Number of coefficients for q2                    |
C                   ----------------------------------------------------
C                   | Number of coefficients for q3                    |
C                   ----------------------------------------------------
C                   | Number of coefficients for AV1                   |
C                   ----------------------------------------------------
C                   | Number of coefficients for AV2                   |
C                   ----------------------------------------------------
C                   | Number of coefficients for AV3                   |
C                   ----------------------------------------------------
C                   | q0 Cheby coefficients                            |
C                   ----------------------------------------------------
C                   | q1 Cheby coefficients                            |
C                   ----------------------------------------------------
C                   | q2 Cheby coefficients                            |
C                   ----------------------------------------------------
C                   | q3 Cheby coefficients                            |
C                   ----------------------------------------------------
C                   | AV1 Cheby coefficients (optional)                |
C                   ----------------------------------------------------
C                   | AV2 Cheby coefficients (optional)                |
C                   ----------------------------------------------------
C                   | AV3 Cheby coefficients (optional)                |
C                   ----------------------------------------------------
C
C                 The interval radius will be 499 ticks.  This will
C                 put the intervals two ticks apart.
C
C                 The interval midpoint will be at the start time
C                 plus 499 ticks.
C
                  PKTS(1) = 499.D0 + EPOCHS(K)
                  PKTS(2) = 499.D0
C
C                 Our quaternions will be constant.
C
                  PKTS(3) = 1
                  PKTS(4) = 1
                  PKTS(5) = 1
                  PKTS(6) = 1
C
C                 Angular velocity will be constant at 0.
C
                  PKTS(7) = 1
                  PKTS(8) = 1
                  PKTS(9) = 1
C
C                 Cheby coefficients for the quaternion elements:
C
                  PKTS(10) = 1.D0
                  PKTS(11) = 2.D0
                  PKTS(12) = 3.D0
                  PKTS(13) = 4.D0
C
C                 Cheby coefficients for the angular velocity elements:
C
                  PKTS(14) = 0.D0
                  PKTS(15) = 0.D0
                  PKTS(16) = 0.D0

C
C                 The packet size depends on whether we're using
C                 angular velocity in this segment.
C
                  IF ( USEAV ) THEN
                     PKTSIZ = 16
                  ELSE
                     PKTSIZ = 13
                  END IF

C
C                 Add the current packet.
C
                  CALL CKW04A ( HANDLE, 1, PKTSIZ, PKTS, EPOCHS(K) )
                  CALL CHCKXC ( .FALSE., ' ', OK )

C
C                 Add the interpolation interval to our interval-level
C                 expected coverage window for the Ith instrument.
C
                  CALL WNINSD ( EPOCHS(K),
     .                          EPOCHS(K) + 2*PKTS(2),
     .                          XCVINT(LBCELL,I)       )
                  CALL CHCKXC ( .FALSE., ' ', OK )

C
C                 If we're providing angular velocity for this segment,
C                 then this segment contributes to the coverage window
C                 for the angular-velocity only segments at the interval
C                 level.
C
                  IF ( USEAV ) THEN

                     CALL WNINSD ( EPOCHS(K),
     .                             EPOCHS(K) + 2*PKTS(2),
     .                             XAVINT(LBCELL,I)       )

                     CALL CHCKXC ( .FALSE., ' ', OK )

                  END IF

               END DO

C
C              End the segment.
C
               CALL CKW04E ( HANDLE, LAST )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Add the segment's coverage interval to our segment-level
C              expected coverage window for the Ith instrument.
C
               CALL WNINSD ( FIRST, LAST, XCVSEG(LBCELL,I) )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              If we're providing angular velocity for this segment,
C              then this segment contributes to the coverage window
C              for the angular-velocity only segments at the interval
C              level.
C
               IF ( USEAV ) THEN
                  CALL WNINSD ( FIRST, LAST, XAVSEG(LBCELL,I) )
                  CALL CHCKXC ( .FALSE., ' ', OK )
               END IF


            ELSE IF ( I .EQ. 5 ) THEN
C
C              This is the CK type 5 case.
C
C              Set type 5 subtype.  We expect NSEG(5) == 4.
C
               IF ( NSEG(5) .NE. 4 ) THEN

                  CALL SETMSG ( 'Test cases for CK type 5 segments ' //
     .                          'use a different type 5 subtype for '//
     .                          'each segment.  The Ith segment is ' //
     .                          'mapped to subtype I-1.  Subtype '   //
     .                          'numbers range from 0 to 3. NSEG(5) '//
     .                          'was expected to be 4 but was #.'    )
                  CALL ERRINT ( '#', NSEG(5)                         )
                  CALL SIGERR ( 'SPICE(BUG)'                         )
                  CALL CHCKXC ( .FALSE., ' ', OK )

               END IF

               SUBTYP = J - 1

C
C              Set packet size.
C
               IF ( SUBTYP .EQ. 0 ) THEN
                  PKTSIZ = C05PS0

               ELSE IF ( SUBTYP .EQ. 1 ) THEN
                  PKTSIZ = C05PS1

               ELSE IF ( SUBTYP .EQ. 2 ) THEN
                  PKTSIZ = C05PS2

               ELSE IF ( SUBTYP .EQ. 3 ) THEN
                  PKTSIZ = C05PS3
               END IF


C              We'll mimic the construction of the type 3 segments,
C              but we'll put the segments in reverse time order
C              relative to each other.
C
C              T3END is supposed to have been initialized before
C              we get here.
C
C
C              We'll use M as a complementary index with respect to
C              J and NSEG(I):
C
               M      =  NSEG(I) + 1 - J

C
C              We must set NREC and USEAV specially for this
C              "backward" segment order.
C
               NREC   =  NR  ( M )
               USEAV  =  ODD ( M )

C
C              Set segment bounds insets:  except for the short
C              segment, the segment bounds will be *inside* the
C              coverage of the interpolation intervals.
C
               IF ( NREC .LT. 99 ) THEN
                  INSETS(1) = 0.D0
                  INSETS(2) = 0.D0
               ELSE
                  INSETS(1) = 3*M
                  INSETS(2) = 5*M
               END IF

C
C              So M will start at NSEG(I) and count down to 1.
C
C              The segments we create will be separated by a 3 tick gap.
C              Records will be M ticks apart.
C
C              Set segment end and epochs.
C
               IF ( M .EQ. NSEG(I) ) THEN
                  LAST = T3END
               ELSE
C
C                 FIRST is left over from the previous M-loop iteration.
C
                  LAST = FIRST - 3.D0
               END IF

C
C              Set EPOCHS, QUATS, and AVVS.
C
C              Pointing data are not relevant for these tests,
C              but having distinct entries could be helpful for
C              debugging.  The Kth entry will be a frame rotation
C              by K milliradians about the Z-axis.
C
               DO K = NREC, 1, -1
C
C                 As stated above, records will be M ticks apart.
C
                  EPOCHS(K) =  LAST - DBLE ( M*(NREC-K) )


C                 The angle required by AXISAR is the negative of
C                 the frame rotation angle.
C
                  DLTANG = 1.D-3
                  ANGLE  =  - ( K * DLTANG )

                  CALL AXISAR ( Z,    ANGLE, CMAT       )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL M2Q    ( CMAT,        QUATS(0,K) )
                  CALL CHCKXC ( .FALSE., ' ', OK )

C
C                 Set angular velocity to be consistent with
C                 the rotation data.  Remember angular velocity
C                 units are radians/sec, so we must multiply
C                 radians/tick by ticks/second for instrument I.
C
                  CALL VSCL ( TIKPER(I)*DLTANG/M, Z, AVVS(1,K) )

C
C                 Set packet contents.
C
                  CALL CLEARD ( MAXPKT, PACKET )

                  IF ( SUBTYP .EQ. 0 ) THEN
C
C                    Packets contain quaternions and quaternion
C                    derivatives.  We'll set the derivatives to zero.
C
                     CALL MOVED ( QUATS(0,K), 4, PACKET )

                  ELSE IF ( SUBTYP .EQ. 1 ) THEN
C
C                    Packets contain quaternions only.
C
                     CALL MOVED ( QUATS(0,K), 4, PACKET )

                  ELSE IF ( SUBTYP .EQ. 2 ) THEN
C
C                    Packets contain quaternions, quaternion
C                    derivatives, angular velocity, and angular
C                    velocity derivatives.  We'll set the derivatives
C                    to zero (even though this makes the angular
C                    velocity and quaternion derivatives
C                    incompatible---subtype 2 is meant to handle
C                    this).
C
                     CALL MOVED ( QUATS(0,K), 4, PACKET    )
                     CALL MOVED ( AVVS (1,K), 3, PACKET(9) )


                  ELSE IF ( SUBTYP .EQ. 3 ) THEN
C
C                    Packets contain quaternions and angular velocity.
C
                     CALL MOVED ( QUATS(0,K), 4, PACKET    )
                     CALL MOVED ( AVVS (1,K), 3, PACKET(5) )

                  END IF

C
C                 Insert packet into packet array.
C
                  L  =  1  +  (K-1)*PKTSIZ

                  CALL MOVED ( PACKET, PKTSIZ, PKTS(L) )

               END DO

C
C              Set segment start time.
C
               FIRST = EPOCHS(1)

C
C              Add the segment's coverage interval to our segment-level
C              expected coverage window for the Ith instrument.
C
               DC(1)  =  FIRST + INSETS(1)
               DC(2)  =  LAST  - INSETS(2)

               CALL WNINSD ( DC(1), DC(2), XCVSEG(LBCELL,I) )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              If we're providing angular velocity for this segment,
C              then this segment contributes to the coverage window
C              for the angular-velocity only segments at the interval
C              level.
C
               IF ( USEAV ) THEN
                  CALL WNINSD ( DC(1), DC(2), XAVSEG(LBCELL,I) )
                  CALL CHCKXC ( .FALSE., ' ', OK )
               END IF

C
C              Set the interval start times.  The first epoch
C              is always the start of an interpolation interval
C              in these tests.  Each interval has length NIVREC
C              records.
C
               L = 0

               DO K = 1,  NREC,  NIVREC
C
C                 Increment the interpolation interval; set the
C                 start time.
C
                  L         = L + 1

                  STARTS(L) = EPOCHS(K)

C
C                 Keep track of the interval end times.
C
                  IF ( L .GT. 1 ) THEN
C
C                    Record the end time of the previous interval.
C
                     ENDS(L-1) = EPOCHS(K-1)

                  END IF

               END DO

C
C              Set the interpolation interval count.
C
               NSTART = L

C
C              The end time of the last interval is (in this test)
C              always the last epoch.
C
               ENDS(NSTART) = EPOCHS(NREC)

C
C              Add the interpolation intervals to our interval-level
C              expected coverage window for the Ith instrument.
C
               DO K = 1, NSTART
C
C                 Adjust the interpolation intervals to account for
C                 the segment boundaries.
C
                  CLSTRT(K)  =  MAX ( STARTS(K), DC(1) )
                  CLEND(K)   =  MIN ( ENDS(K),   DC(2) )

                  IF ( CLSTRT(K) .LE. CLEND(K) ) THEN
                     CALL WNINSD( CLSTRT(K), CLEND(K), XCVINT(LBCELL,I))
                  END IF

               END DO

C
C              If we're providing angular velocity for this segment,
C              then this segment contributes to the coverage window
C              for the angular-velocity only segments at the interval
C              level.
C
               IF ( USEAV ) THEN

                  DO K = 1, NSTART
C
C                    The interpolation intervals that have been
C                    adjusted to account for the segment boundaries
C                    are contained in CLSTRT and CLEND.
C
                     IF ( CLSTRT(K) .LE. CLEND(K) ) THEN

                        CALL WNINSD ( CLSTRT(K),
     .                                CLEND(K),
     .                                XAVINT(LBCELL,I) )

                        CALL CHCKXC ( .FALSE., ' ', OK )

                     END IF

                  END DO

               END IF

C
C              Create segment ID.
C
               SEGID = 'Segment # for instrument #.'

               CALL REPMI  ( SEGID, '#', J, SEGID )
               CALL REPMI  ( SEGID, '#', I, SEGID )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Write the current segment to our CK.  All interpolating
C              polynomials will be cubic.
C
               DEGREE = 3
               RATE   = 1.D0 / TIKPER(I)

               CALL CKW05 ( HANDLE,   SUBTYP,   DEGREE, DC(1),  DC(2),
     .                      INST(I),  'J2000',  USEAV,  SEGID,  NREC,
     .                      EPOCHS,   PKTS,     RATE,   NSTART, STARTS )

               CALL CHCKXC ( .FALSE., ' ', OK )



            ELSE IF ( I .EQ. 6 ) THEN
C
C              This is the CK type 6 case.
C
C              We'll use the "select last" option.
C
               SELLST = .TRUE.
C
C              Set type 6 subtype.  We expect NSEG(6) == 4.
C
               IF ( NSEG(6) .NE. 4 ) THEN

                  CALL SETMSG ( 'Test cases for CK type 6 segments ' //
     .                          'use a different type 6 subtype for '//
     .                          'each segment.  The Ith segment is ' //
     .                          'mapped to subtype I-1.  Subtype '   //
     .                          'numbers range from 0 to 3. NSEG(6) '//
     .                          'was expected to be 4 but was #.'    )
                  CALL ERRINT ( '#', NSEG(6)                         )
                  CALL SIGERR ( 'SPICE(BUG)'                         )
                  CALL CHCKXC ( .FALSE., ' ', OK )
                  
               END IF

               SUBTYP = J - 1

C
C              Set packet size.
C
               IF ( SUBTYP .EQ. 0 ) THEN
                  PKTSIZ = C06PS0

               ELSE IF ( SUBTYP .EQ. 1 ) THEN
                  PKTSIZ = C06PS1

               ELSE IF ( SUBTYP .EQ. 2 ) THEN
                  PKTSIZ = C06PS2

               ELSE IF ( SUBTYP .EQ. 3 ) THEN
                  PKTSIZ = C06PS3
               END IF

C
C              We'll mimic the construction of the type 3 segments,
C              but we'll put the segments in reverse time order
C              relative to each other.
C
C              T3END is supposed to have been initialized before
C              we get here.
C
C              We'll use M as a complementary index with respect to
C              J and NSEG(I):
C
               M      =  NSEG(I) + 1 - J

C
C              We must set NREC and USEAV specially for this 
C              "backward" segment order.
C 
               NREC   =  NR  ( M )

               USEAV  =  ODD ( M )

C
C              Set segment bounds insets:  except for the short
C              segment, the segment bounds will be *inside* the 
C              coverage of the interpolation intervals.
C
               IF ( NREC .LT. 99 ) THEN
                  INSETS(1) = 0.D0
                  INSETS(2) = 0.D0
               ELSE
                  INSETS(1) = 3*M
                  INSETS(2) = 5*M
               END IF

C
C              So M will start at NSEG(I) and count down to 1.
C
C              The segments we create will be separated by a 3 tick gap.
C              Records will be M ticks apart.
C
C              Set segment end and epochs.
C
               IF ( M .EQ. NSEG(I) ) THEN
                  LAST = T3END
               ELSE
C
C                 FIRST is left over from the previous M-loop iteration.
C
                  LAST = FIRST - 3.D0
               END IF

C
C              Set EPOCHS, QUATS, and AVVS.
C
C              Pointing data are not relevant for these tests,
C              but having distinct entries could be helpful for
C              debugging.  The Kth entry will be a frame rotation
C              by K milliradians about the Z-axis.
C
               DO K = NREC, 1, -1
C
C                 As stated above, records will be M ticks apart.
C
                  EPOCHS(K) =  LAST - DBLE ( M*(NREC-K) )


C                 The angle required by AXISAR is the negative of 
C                 the frame rotation angle.
C
                  DLTANG = 1.D-3
                  ANGLE  =  - ( K * DLTANG )

                  CALL AXISAR ( Z,    ANGLE, CMAT       )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL M2Q    ( CMAT,        QUATS(0,K) )
                  CALL CHCKXC ( .FALSE., ' ', OK )

C
C                 Set angular velocity to be consistent with
C                 the rotation data.  Remember angular velocity
C                 units are radians/sec, so we must multiply 
C                 radians/tick by ticks/second for instrument I.
C
                  CALL VSCL ( TIKPER(I)*DLTANG/M, Z, AVVS(1,K) )

C
C                 Set packet contents.
C
                  CALL CLEARD ( MAXPKT, PACKET )

                  IF ( SUBTYP .EQ. 0 ) THEN
C
C                    Packets contain quaternions and quaternion
C                    derivatives.  We'll set the derivatives to zero.
C
                     CALL MOVED ( QUATS(0,K), 4, PACKET ) 

                  ELSE IF ( SUBTYP .EQ. 1 ) THEN
C
C                    Packets contain quaternions only.
C
                     CALL MOVED ( QUATS(0,K), 4, PACKET ) 

                  ELSE IF ( SUBTYP .EQ. 2 ) THEN
C
C                    Packets contain quaternions, quaternion
C                    derivatives, angular velocity, and angular
C                    velocity derivatives.  We'll set the derivatives
C                    to zero (even though this makes the angular
C                    velocity and quaternion derivatives
C                    incompatible---subtype 2 is meant to handle
C                    this).
C                     
                     CALL MOVED ( QUATS(0,K), 4, PACKET    ) 
                     CALL MOVED ( AVVS (1,K), 3, PACKET(9) ) 


                  ELSE IF ( SUBTYP .EQ. 3 ) THEN
C
C                    Packets contain quaternions and angular velocity.
C
                     CALL MOVED ( QUATS(0,K), 4, PACKET    ) 
                     CALL MOVED ( AVVS (1,K), 3, PACKET(5) ) 

                  END IF    

C
C                 Insert packet into packet array.
C
                  L  =  1  +  (K-1)*PKTSIZ

                  CALL MOVED ( PACKET, PKTSIZ, PKTS(L) )

               END DO
               
C
C              Set segment start time.
C
               FIRST = EPOCHS(1)

C
C              Add the segment's coverage interval to our segment-level
C              expected coverage window for the Ith instrument.
C
               DC(1)  =  FIRST + INSETS(1)
               DC(2)  =  LAST  - INSETS(2)

               CALL WNINSD ( DC(1), DC(2), XCVSEG(LBCELL,I) )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              If we're providing angular velocity for this segment,
C              then this segment contributes to the coverage window
C              for the angular-velocity only segments at the interval
C              level.
C
               IF ( USEAV ) THEN
                  CALL WNINSD ( DC(1), DC(2), XAVSEG(LBCELL,I) )
                  CALL CHCKXC ( .FALSE., ' ', OK )
               END IF

C
C              Set the interval start times.  The first epoch
C              is always the start of an interpolation interval
C              in these tests.  Each interval but the last
C              one in the segment has length NIVREC records.
C              NMINI counts the number of interpolation intervals
C              (equal to the number of mini-segments) in the 
C              segment.
C
C              Each interval has a gap at the end. The gap duration
C              is the (constant) separation between epochs, which
C              has been set to M ticks.
C               
               L = 0
               
C
C              Terminate this loop before the last record, so we don't
C              start an interpolation interval at that record.
C
               DO K = 1,  NREC-1,  NIVREC
C
C                 Increment the interpolation interval; set the 
C                 start time.
C
                  L         = L + 1

                  IVLBDS(L) = EPOCHS(K)
                  STARTS(L) = IVLBDS(L)

C
C                 Keep track of the interval end times.
C
                  IF ( L .GT. 1 ) THEN
C
C                    Record the "end" time of the previous interval.
C                    The end time in this context is the last epoch,
C                    not the right hand interval boundary. The end time
C                    represents the upper bound of the interval over
C                    which data are available. The interval upper bound
C                    coincides with the upper end of a coverage gap.
C
                     ENDS(L-1) = EPOCHS(K-1)

                  END IF
C
C                 Set the other interval-related parameters.
C
C                 All interpolating polynomials will be cubic.
C              
                  DEGREE = 3
                  RATE   = 1.D0 / TIKPER(I)

                  NPKTS (L) = MIN( NIVREC,  (NREC-K+1) )
                  RATES (L) = RATE
                  SUBTPS(L) = SUBTYP
                  DEGRES(L) = DEGREE

               END DO

C
C              Set the interpolation interval count.
C              
               NMINI  = L
               NSTART = NMINI
C
C              The last epoch of the last interval is (in this test)
C              the last epoch; the interval upper bound is beyond that
C              epoch, since the interval has a gap.
C
               ENDS(NMINI)     = EPOCHS(NREC)

               IVLBDS(NMINI+1) = EPOCHS(NREC) + M

C
C              Add the interpolation intervals to our interval-level
C              expected coverage window for the Ith instrument.
C                 
               DO K = 1, NSTART
C
C                 Adjust the interpolation intervals to account for
C                 the segment boundaries.
C
                  CLSTRT(K)  =  MAX ( STARTS(K), DC(1) )
                  CLEND(K)   =  MIN ( ENDS(K),   DC(2) )

                  IF ( CLSTRT(K) .LE. CLEND(K) ) THEN
                     CALL WNINSD( CLSTRT(K), CLEND(K), XCVINT(LBCELL,I))
                  END IF

               END DO

C
C              If we're providing angular velocity for this segment,
C              then this segment contributes to the coverage window
C              for the angular-velocity only segments at the interval
C              level.
C
               IF ( USEAV ) THEN

                  DO K = 1, NSTART
C
C                    The interpolation intervals that have been
C                    adjusted to account for the segment boundaries
C                    are contained in CLSTRT and CLEND.
C
                     IF ( CLSTRT(K) .LE. CLEND(K) ) THEN
                     
                        CALL WNINSD ( CLSTRT(K), 
     .                                CLEND(K), 
     .                                XAVINT(LBCELL,I) )

                        CALL CHCKXC ( .FALSE., ' ', OK )

                     END IF

                  END DO

               END IF

C
C              Create segment ID.
C
               SEGID = 'Segment # for instrument #.'

               CALL REPMI  ( SEGID, '#', J, SEGID )
               CALL REPMI  ( SEGID, '#', I, SEGID )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Write the current segment to our CK. 
C
               CALL CKW06 ( HANDLE, INST(I), 'J2000', USEAV,
     .                      DC(1),  DC(2),   SEGID,   NMINI,
     .                      NPKTS,  SUBTPS,  DEGRES,  PKTS,
     .                      RATES,  EPOCHS,  IVLBDS,  SELLST )
               CALL CHCKXC ( .FALSE., ' ', OK )



            ELSE
C
C              Oops.
C
               CALL SETMSG ( 'Unexpected data type: #' )
               CALL ERRINT ( '#', I                    )
               CALL SIGERR ( 'SPICE(BUG)'              )
               CALL CHCKXC ( .FALSE., ' ', OK )

            END IF



         END DO

      END DO


      CALL CKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Call DAFCLS as a last resort, since CKCLS may fail to
C     close the file.
C
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C ******************************************************
C ******************************************************
C ******************************************************
C     CKCOV tests
C ******************************************************
C ******************************************************
C ******************************************************
C
C     We've written the CK.  It's time to check out CKCOV.
C
C
C     Check actual vs expected coverage as we vary the input
C     arguments to CKCOV.
C
C
C     Each test we do will be performed with both an empty
C     and non-empty input coverage window.
C
      DO L = 1, 2
C
C        We'll start out by testing the coverage summary at the
C        segment level.
C
         IF ( L .EQ. 1 ) THEN
C
C           We'll set COVER to be empty on input to CKCOV.
C
            CALL SCARDD ( 0, COVER )

            CVSTAT = 'empty'

         ELSE

            CVSTAT = 'non-empty'

         END IF


         DO I = 1, NINS
C
C --- Case: ------------------------------------------------------
C
            TITLE = 'Check segment-level '                       //
     .              'coverage for instrument #; COVER starts '   //
     .              'out #. Angular velocity not needed. '       //
     .              'TOL = 0.D0.'

            CALL REPMI ( TITLE, '#', I,      TITLE )
            CALL REPMC ( TITLE, '#', CVSTAT, TITLE )
            CALL TCASE ( TITLE )

C
C           Initialize COVER.
C
            CALL SSIZED ( MAXCOV, COVER )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Make a copy of the expected window.
C
            CALL COPYD ( XCVSEG(LBCELL,I),  TMPWIN )

            IF ( L .EQ. 2 ) THEN
C
C              Insert an interval into COVER.  This same interval
C              must be added to each window containing expected
C              coverage.
C
               CALL WNINSD ( 1.D6, 1.D7, COVER )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              The same interval is expected to appear in the output.
C
               CALL WNINSD ( 1.D6, 1.D7, TMPWIN )
               CALL CHCKXC ( .FALSE., ' ', OK )

            END IF


            CALL CKCOV ( CK,   INST(I), .FALSE., 'SEGMENT',
     .                   0.D0, 'SCLK',  COVER              )

            CALL CHCKXC ( .FALSE., ' ', OK )
C
C           Check cardinality of coverage window.
C
            CALL CHCKSI ( 'CARDD(COVER)', CARDD( COVER  ),
     .                    '=',            CARDD( TMPWIN ),
     .                    0,              OK                )

C
C           Check coverage window.
C
            CALL CHCKAD ( 'COVER', COVER(1),
     .                    '=',     TMPWIN(1),  CARDD(COVER),
     .                    0.D0,    OK                        )

            IF ( I .EQ. 6 ) THEN
               
               CALL SCARDD ( 0, TMPWIN )
               CALL CHCKXC ( .FALSE., ' ', OK )

               IF ( L .EQ. 2 ) THEN
C
C                 The same interval is expected to appear in the output.
C
                  CALL WNINSD ( 1.D6, 1.D7, TMPWIN )
                  CALL CHCKXC ( .FALSE., ' ', OK )

               END IF

C
C              Compare CKCOV results against those from T_CKCOV.
C
               CALL T_CKCOV ( CK,   INST(I), .FALSE., 'SEGMENT',
     .                        0.D0, 'SCLK',  TMPWIN             )
               CALL CHCKXC ( .FALSE., ' ', OK )
               
C
C              Check cardinality of coverage window.
C
               CALL CHCKSI ( 'CARDD(COVER)', CARDD( COVER  ),
     .                       '=',            CARDD( TMPWIN ),
     .                       0,              OK                )

C
C              Check coverage window.
C
               CALL CHCKAD ( 'COVER', COVER(1),
     .                       '=',     TMPWIN(1),  CARDD(COVER),
     .                       0.D0,    OK                        )
            END IF

         END DO


C
C --- Case: ------------------------------------------------------
C
         DO I = 1, NINS

            TITLE = 'INST: #;  LEVEL: SEGMENT;  NEEDAV: TRUE; '      //
     .              'TIMSYS: SCLK; TOL: 0.D0; COVER starts out #.'

            CALL REPMI ( TITLE, '#', I,      TITLE )
            CALL REPMC ( TITLE, '#', CVSTAT, TITLE )
            CALL TCASE ( TITLE )

C
C           Initialize COVER.
C
            CALL SSIZED ( MAXCOV, COVER )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Make a copy of the expected window.
C
            CALL COPYD ( XAVSEG(LBCELL,I),  TMPWIN )

            IF ( L .EQ. 2 ) THEN
C
C              Insert an interval into COVER.  This same interval
C              must be added to each window containing expected
C              coverage.
C
               CALL WNINSD ( 1.D6, 1.D7, COVER )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              The same interval is expected to appear in the output.
C
               CALL WNINSD ( 1.D6, 1.D7, TMPWIN )
               CALL CHCKXC ( .FALSE., ' ', OK )

            END IF

            CALL CKCOV ( CK,   INST(I), .TRUE., 'SEGMENT',
     .                   0.D0, 'SCLK',  COVER              )

            CALL CHCKXC ( .FALSE., ' ', OK )
C
C           Check cardinality of coverage window.
C
            CALL CHCKSI ( 'CARDD(COVER)', CARDD( COVER  ),
     .                    '=',            CARDD( TMPWIN ),
     .                    0,              OK                         )

C
C           Check coverage window.
C
            CALL CHCKAD ( 'COVER', COVER(1),
     .                    '=',     TMPWIN(1),  CARDD(COVER),
     .                    0.D0,    OK                          )


            IF ( I .EQ. 6 ) THEN

               CALL SCARDD ( 0, TMPWIN )
               CALL CHCKXC ( .FALSE., ' ', OK )

               IF ( L .EQ. 2 ) THEN
C
C                 The same interval is expected to appear in the output.
C
                  CALL WNINSD ( 1.D6, 1.D7, TMPWIN )
                  CALL CHCKXC ( .FALSE., ' ', OK )

               END IF

C
C              Compare CKCOV results against those from T_CKCOV.
C
               CALL T_CKCOV ( CK,   INST(I), .TRUE., 'SEGMENT',
     .                        0.D0, 'SCLK',  TMPWIN             )
               CALL CHCKXC ( .FALSE., ' ', OK )
               
C
C              Check cardinality of coverage window.
C
               CALL CHCKSI ( 'CARDD(COVER)', CARDD( COVER  ),
     .                       '=',            CARDD( TMPWIN ),
     .                       0,              OK                )

C
C              Check coverage window.
C
               CALL CHCKAD ( 'COVER', COVER(1),
     .                       '=',     TMPWIN(1),  CARDD(COVER),
     .                       0.D0,    OK                        )
            END IF

         END DO



C
C --- Case: ------------------------------------------------------
C
         DO I = 1, NINS

            TITLE = 'INST: #;  LEVEL: SEGMENT;  NEEDAV: FALSE; '      //
     .              'TIMSYS: SCLK; TOL: 1.D0; COVER starts out #.'

            CALL REPMI ( TITLE, '#', I,      TITLE )
            CALL REPMC ( TITLE, '#', CVSTAT, TITLE )
            CALL TCASE ( TITLE )


C
C           Initialize COVER.
C
            CALL SSIZED ( MAXCOV, COVER )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Adjust our expected result window by TOL.
C
            CALL COPYD ( XCVSEG(LBCELL,I), TMPWIN )

            TOL = 1.D0


            CALL WNEXPD ( TOL, TOL, TMPWIN )
C
C           Make sure the window doesn't start with a negative tick
C           value.
C
            TMPWIN(1) = MAX ( TMPWIN(1), 0.D0 )

            IF ( L .EQ. 2 ) THEN
C
C              Insert an interval into COVER.  This same interval
C              must be added to each window containing expected
C              coverage.
C
               CALL WNINSD ( 1.D6, 1.D7, COVER )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              The same interval is expected to appear in the output.
C
               CALL WNINSD ( 1.D6, 1.D7, TMPWIN )
               CALL CHCKXC ( .FALSE., ' ', OK )

            END IF


            CALL CKCOV ( CK,   INST(I), .FALSE., 'SEGMENT',
     .                   TOL, 'SCLK',   COVER               )

            CALL CHCKXC ( .FALSE., ' ', OK )
C
C           Check cardinality of coverage window.
C
            CALL CHCKSI ( 'CARDD(COVER)', CARDD( COVER ),
     .                    '=',            CARDD( TMPWIN),
     .                    0,              OK              )

C
C           Check coverage window.
C
            CALL CHCKAD ( 'COVER', COVER(1),
     .                    '=',     TMPWIN(1),  CARDD(COVER),
     .                    0.D0,    OK                     )


            IF ( I .EQ. 6 ) THEN

               CALL SCARDD ( 0, TMPWIN )
               CALL CHCKXC ( .FALSE., ' ', OK )

               IF ( L .EQ. 2 ) THEN
C
C                 The same interval is expected to appear in the output.
C
                  CALL WNINSD ( 1.D6, 1.D7, TMPWIN )
                  CALL CHCKXC ( .FALSE., ' ', OK )

               END IF

C
C              Compare CKCOV results against those from T_CKCOV.
C
               CALL T_CKCOV ( CK,   INST(I), .FALSE., 'SEGMENT',
     .                        TOL,  'SCLK',  TMPWIN             )
               CALL CHCKXC ( .FALSE., ' ', OK )
               
C
C              Check cardinality of coverage window.
C
               CALL CHCKSI ( 'CARDD(COVER)', CARDD( COVER  ),
     .                       '=',            CARDD( TMPWIN ),
     .                       0,              OK                )

C
C              Check coverage window.
C
               CALL CHCKAD ( 'COVER', COVER(1),
     .                       '=',     TMPWIN(1),  CARDD(COVER),
     .                       0.D0,    OK                        )
            END IF

         END DO



         DO I = 1, NINS
C
C --- Case: ------------------------------------------------------
C
            TITLE = 'INST: #;  LEVEL: SEGMENT;  NEEDAV: FALSE; '      //
     .              'TIMSYS: TDB; TOL: 0.D0; COVER: #.'

            CALL REPMI ( TITLE, '#', I,      TITLE )
            CALL REPMC ( TITLE, '#', CVSTAT, TITLE )
            CALL TCASE ( TITLE )


C
C           Initialize COVER.
C
            CALL SSIZED ( MAXCOV, COVER )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Make a copy of the expected window.
C
            CALL COPYD ( XCVSEG(LBCELL,I),  TMPWIN )

C
C           Convert the expected window to TDB.
C
            DO J = 1, CARDD(TMPWIN)

               CALL SCT2E ( CLKID(I), TMPWIN(J),   ET )
               TMPWIN(J)  =  ET

               CALL CHCKXC ( .FALSE., ' ', OK )

            END DO


            IF ( L .EQ. 2 ) THEN
C
C              Insert an interval into COVER.  This same interval
C              must be added to each window containing expected
C              coverage.
C
               CALL WNINSD ( 1.D6, 1.D7, COVER )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              The same interval is expected to appear in the output.
C
               CALL WNINSD ( 1.D6, 1.D7, TMPWIN )
               CALL CHCKXC ( .FALSE., ' ', OK )

            END IF

            CALL CKCOV ( CK,   INST(I), .FALSE., 'SEGMENT',
     .                   0.D0, 'TDB',   COVER               )

            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Check cardinality of coverage window.
C
            CALL CHCKSI ( 'CARDD(COVER)', CARDD( COVER  ),
     .                    '=',            CARDD( TMPWIN ),
     .                    0,              OK              )

C
C           Check coverage window.
C
            CALL CHCKAD ( 'COVER', COVER(1),
     .                    '=',     TMPWIN(1),  CARDD(TMPWIN),
     .                    0.D0,    OK                        )

            IF ( I .EQ. 6 ) THEN

               CALL SCARDD ( 0, TMPWIN )
               CALL CHCKXC ( .FALSE., ' ', OK )

               IF ( L .EQ. 2 ) THEN
C
C                 The same interval is expected to appear in the output.
C
                  CALL WNINSD ( 1.D6, 1.D7, TMPWIN )
                  CALL CHCKXC ( .FALSE., ' ', OK )

               END IF

C
C              Compare CKCOV results against those from T_CKCOV.
C
               CALL T_CKCOV ( CK,   INST(I), .FALSE., 'SEGMENT',
     .                        0.D0, 'TDB',   TMPWIN             )
               CALL CHCKXC ( .FALSE., ' ', OK )
               
C
C              Check cardinality of coverage window.
C
               CALL CHCKSI ( 'CARDD(COVER)', CARDD( COVER  ),
     .                       '=',            CARDD( TMPWIN ),
     .                       0,              OK                )

C
C              Check coverage window.
C
               CALL CHCKAD ( 'COVER', COVER(1),
     .                       '=',     TMPWIN(1),  CARDD(COVER),
     .                       0.D0,    OK                        )
            END IF

         END DO




         DO I = 1, NINS
C
C --- Case: ------------------------------------------------------
C
            TITLE = 'INST: #;  LEVEL: SEGMENT;  NEEDAV: FALSE; '      //
     .              'TIMSYS: TDB; TOL: 1.D0; COVER: #.'

            CALL REPMI ( TITLE, '#', I,      TITLE )
            CALL REPMC ( TITLE, '#', CVSTAT, TITLE )
            CALL TCASE ( TITLE )


C
C           Initialize COVER.
C
            CALL SSIZED ( MAXCOV, COVER )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Adjust our expected result window by TOL.
C
            CALL COPYD ( XCVSEG(LBCELL,I), TMPWIN )

            TOL = 1.D0

            CALL WNEXPD ( TOL, TOL, TMPWIN )

C
C           Make sure the window doesn't start with a negative tick
C           value.
C
            TMPWIN(1) = MAX ( TMPWIN(1), 0.D0 )

C
C           Convert the expected window to TDB.
C
            DO J = 1, CARDD(TMPWIN)

               CALL SCT2E ( CLKID(I), TMPWIN(J),   ET )
               TMPWIN(J)  =  ET

            END DO


            IF ( L .EQ. 2 ) THEN
C
C              Insert an interval into COVER.  This same interval
C              must be added to each window containing expected
C              coverage.
C
               CALL WNINSD ( 1.D6, 1.D7, COVER )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              The same interval is expected to appear in the output.
C
               CALL WNINSD ( 1.D6, 1.D7, TMPWIN )
               CALL CHCKXC ( .FALSE., ' ', OK )

            END IF

            CALL CKCOV ( CK,   INST(I), .FALSE., 'SEGMENT',
     .                   1.D0, 'TDB',   COVER               )

            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Check cardinality of coverage window.
C
            CALL CHCKSI ( 'CARDD(COVER)', CARDD( COVER  ),
     .                    '=',            CARDD( TMPWIN ),
     .                    0,              OK              )

C
C           Check coverage window.
C
            CALL CHCKAD ( 'COVER', COVER(1),
     .                    '=',     TMPWIN(1),  CARDD(TMPWIN),
     .                    0.D0,    OK                        )




            IF ( I .EQ. 6 ) THEN

               CALL SCARDD ( 0, TMPWIN )
               CALL CHCKXC ( .FALSE., ' ', OK )

               IF ( L .EQ. 2 ) THEN
C
C                 The same interval is expected to appear in the output.
C
                  CALL WNINSD ( 1.D6, 1.D7, TMPWIN )
                  CALL CHCKXC ( .FALSE., ' ', OK )

               END IF

C
C              Compare CKCOV results against those from T_CKCOV.
C
               CALL T_CKCOV ( CK,   INST(I), .FALSE., 'SEGMENT',
     .                        1.D0, 'TDB',   TMPWIN             )
               CALL CHCKXC ( .FALSE., ' ', OK )
               
C
C              Check cardinality of coverage window.
C
               CALL CHCKSI ( 'CARDD(COVER)', CARDD( COVER  ),
     .                       '=',            CARDD( TMPWIN ),
     .                       0,              OK                )

C
C              Check coverage window.
C
               CALL CHCKAD ( 'COVER', COVER(1),
     .                       '=',     TMPWIN(1),  CARDD(COVER),
     .                       0.D0,    OK                        )
            END IF


         END DO

C
C        INTERVAL level tests:
C
C
C        Now we'll repeat the previous tests, but this time the
C        coverage will be summarized at the interval level.
C

         DO I = 1, NINS
C
C --- Case: ------------------------------------------------------
C
            TITLE = 'INST: #;  LEVEL: INTERVAL;  NEEDAV: FALSE; ' //
     .              'TIMSYS: SCLK TOL: 0.D0; COVER: #.'

            CALL REPMI ( TITLE, '#', I,      TITLE )
            CALL REPMC ( TITLE, '#', CVSTAT, TITLE )
            CALL TCASE ( TITLE )

C
C           Initialize COVER.
C
            CALL SSIZED ( MAXCOV, COVER )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Make a copy of the expected window.
C
            CALL COPYD ( XCVINT(LBCELL,I),  TMPWIN )

            IF ( L .EQ. 2 ) THEN
C
C              Insert an interval into COVER.  This same interval
C              must be added to each window containing expected
C              coverage.
C
               CALL WNINSD ( 1.D6, 1.D7, COVER )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              The same interval is expected to appear in the output.
C
               CALL WNINSD ( 1.D6, 1.D7, TMPWIN )
               CALL CHCKXC ( .FALSE., ' ', OK )

            END IF

            CALL CKCOV ( CK,   INST(I), .FALSE., 'INTERVAL',
     .                   0.D0, 'SCLK',  COVER              )

            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Check cardinality of coverage window.
C
            CALL CHCKSI ( 'CARDD(COVER)', CARDD( COVER  ),
     .                    '=',            CARDD( TMPWIN ),
     .                    0,              OK               )

C
C           Check coverage window.
C
            CALL CHCKAD ( 'COVER', COVER(1),
     .                    '=',     TMPWIN(1),  CARDD(COVER),
     .                    0.D0,    OK                       )


            IF ( I .EQ. 6 ) THEN

               CALL SCARDD ( 0, TMPWIN )
               CALL CHCKXC ( .FALSE., ' ', OK )

               IF ( L .EQ. 2 ) THEN
C
C                 The same interval is expected to appear in the output.
C
                  CALL WNINSD ( 1.D6, 1.D7, TMPWIN )
                  CALL CHCKXC ( .FALSE., ' ', OK )

               END IF

C
C              Compare CKCOV results against those from T_CKCOV.
C
               CALL T_CKCOV ( CK,   INST(I), .FALSE., 'INTERVAL',
     .                        0.D0, 'SCLK',   TMPWIN             )
               CALL CHCKXC ( .FALSE., ' ', OK )
               
C
C              Check cardinality of coverage window.
C
               CALL CHCKSI ( 'CARDD(COVER)', CARDD( COVER  ),
     .                       '=',            CARDD( TMPWIN ),
     .                       0,              OK                )

C
C              Check coverage window.
C
               CALL CHCKAD ( 'COVER', COVER(1),
     .                       '=',     TMPWIN(1),  CARDD(COVER),
     .                       0.D0,    OK                        )
            END IF

         END DO


C
C --- Case: ------------------------------------------------------
C
         DO I = 1, NINS

            TITLE = 'INST: #;  LEVEL: INTERVAL;  NEEDAV: TRUE; ' //
     .              'TIMSYS: SCLK TOL: 0.D0; COVER: #.'

            CALL REPMI ( TITLE, '#', I,      TITLE )
            CALL REPMC ( TITLE, '#', CVSTAT, TITLE )
            CALL TCASE ( TITLE )

C
C           Initialize COVER.
C
            CALL SSIZED ( MAXCOV, COVER )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Make a copy of the expected window.
C
            CALL COPYD ( XAVINT(LBCELL,I),  TMPWIN )

            IF ( L .EQ. 2 ) THEN
C
C              Insert an interval into COVER.  This same interval
C              must be added to each window containing expected
C              coverage.
C
               CALL WNINSD ( 1.D6, 1.D7, COVER )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              The same interval is expected to appear in the output.
C
               CALL WNINSD ( 1.D6, 1.D7, TMPWIN )
               CALL CHCKXC ( .FALSE., ' ', OK )

            END IF

            CALL CKCOV ( CK,   INST(I), .TRUE., 'INTERVAL',
     .                   0.D0, 'SCLK',  COVER              )

            CALL CHCKXC ( .FALSE., ' ', OK )
C
C           Check cardinality of coverage window.
C
            CALL CHCKSI ( 'CARDD(COVER)', CARDD( COVER  ),
     .                    '=',            CARDD( TMPWIN ),
     .                    0,              OK               )

C
C           Check coverage window.
C
            CALL CHCKAD ( 'COVER', COVER(1),
     .                    '=',     TMPWIN(1),  CARDD(COVER),
     .                    0.D0,    OK                          )

            IF ( I .EQ. 6 ) THEN

               CALL SCARDD ( 0, TMPWIN )
               CALL CHCKXC ( .FALSE., ' ', OK )

               IF ( L .EQ. 2 ) THEN
C
C                 The same interval is expected to appear in the output.
C
                  CALL WNINSD ( 1.D6, 1.D7, TMPWIN )
                  CALL CHCKXC ( .FALSE., ' ', OK )

               END IF

C
C              Compare CKCOV results against those from T_CKCOV.
C
               CALL T_CKCOV ( CK,   INST(I), .TRUE., 'INTERVAL',
     .                        0.D0, 'SCLK',   TMPWIN             )
               CALL CHCKXC ( .FALSE., ' ', OK )
               
C
C              Check cardinality of coverage window.
C
               CALL CHCKSI ( 'CARDD(COVER)', CARDD( COVER  ),
     .                       '=',            CARDD( TMPWIN ),
     .                       0,              OK                )

C
C              Check coverage window.
C
               CALL CHCKAD ( 'COVER', COVER(1),
     .                       '=',     TMPWIN(1),  CARDD(COVER),
     .                       0.D0,    OK                        )
            END IF

         END DO



C
C --- Case: ------------------------------------------------------
C
         DO I = 1, NINS

            TITLE = 'INST: #;  LEVEL: INTERVAL;  NEEDAV: FALSE; ' //
     .              'TIMSYS: SCLK TOL: 1.D0; COVER: #.'

            CALL REPMI ( TITLE, '#', I,      TITLE )
            CALL REPMC ( TITLE, '#', CVSTAT, TITLE )
            CALL TCASE ( TITLE )

C
C           Initialize COVER.
C
            CALL SSIZED ( MAXCOV, COVER )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Adjust our expected result window by TOL.
C
            CALL COPYD ( XCVINT(LBCELL,I), TMPWIN )

            TOL = 1.D0

            CALL WNEXPD ( TOL, TOL, TMPWIN )

C
C           Make sure the window doesn't start with a negative tick
C           value.
C
            TMPWIN(1) = MAX ( TMPWIN(1), 0.D0 )


           IF ( L .EQ. 2 ) THEN
C
C              Insert an interval into COVER.  This same interval
C              must be added to each window containing expected
C              coverage.
C
               CALL WNINSD ( 1.D6, 1.D7, COVER )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              The same interval is expected to appear in the output.
C
               CALL WNINSD ( 1.D6, 1.D7, TMPWIN )
               CALL CHCKXC ( .FALSE., ' ', OK )

            END IF

            CALL CKCOV ( CK,   INST(I), .FALSE., 'INTERVAL',
     .                   TOL, 'SCLK',   COVER               )

            CALL CHCKXC ( .FALSE., ' ', OK )
C
C           Check cardinality of coverage window.
C
            CALL CHCKSI ( 'CARDD(COVER)', CARDD( COVER ),
     .                    '=',            CARDD( TMPWIN),
     .                    0,              OK              )

C
C           Check coverage window.
C
            CALL CHCKAD ( 'COVER', COVER(1),
     .                    '=',     TMPWIN(1),  CARDD(COVER),
     .                    0.D0,    OK                     )

            IF ( I .EQ. 6 ) THEN

               CALL SCARDD ( 0, TMPWIN )
               CALL CHCKXC ( .FALSE., ' ', OK )

               IF ( L .EQ. 2 ) THEN
C
C                 The same interval is expected to appear in the output.
C
                  CALL WNINSD ( 1.D6, 1.D7, TMPWIN )
                  CALL CHCKXC ( .FALSE., ' ', OK )

               END IF

C
C              Compare CKCOV results against those from T_CKCOV.
C
               CALL T_CKCOV ( CK,   INST(I), .FALSE., 'INTERVAL',
     .                        TOL,  'SCLK',   TMPWIN             )
               CALL CHCKXC ( .FALSE., ' ', OK )
               
C
C              Check cardinality of coverage window.
C
               CALL CHCKSI ( 'CARDD(COVER)', CARDD( COVER  ),
     .                       '=',            CARDD( TMPWIN ),
     .                       0,              OK                )

C
C              Check coverage window.
C
               CALL CHCKAD ( 'COVER', COVER(1),
     .                       '=',     TMPWIN(1),  CARDD(COVER),
     .                       0.D0,    OK                        )
            END IF

         END DO




         DO I = 1, NINS
C
C --- Case: ------------------------------------------------------
C
            TITLE = 'INST: #;  LEVEL: INTERVAL  NEEDAV: FALSE; ' //
     .              'TIMSYS: TDB; TOL: 0.D0; COVER: #.'

            CALL REPMI ( TITLE, '#', I,      TITLE )
            CALL REPMC ( TITLE, '#', CVSTAT, TITLE )
            CALL TCASE ( TITLE )


C
C           Initialize COVER.
C
            CALL SSIZED ( MAXCOV, COVER )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Make a copy of the expected window.
C
            CALL COPYD ( XCVINT(LBCELL,I),  TMPWIN )

C
C           Convert the expected window to TDB.
C
            DO J = 1, CARDD(TMPWIN)

               CALL SCT2E ( CLKID(I), TMPWIN(J),   ET )
               TMPWIN(J)  =  ET

            END DO


            IF ( L .EQ. 2 ) THEN
C
C              Insert an interval into COVER.  This same interval
C              must be added to each window containing expected
C              coverage.
C
               CALL WNINSD ( 1.D6, 1.D7, COVER )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              The same interval is expected to appear in the output.
C
               CALL WNINSD ( 1.D6, 1.D7, TMPWIN )
               CALL CHCKXC ( .FALSE., ' ', OK )

            END IF

            CALL CKCOV ( CK,   INST(I), .FALSE., 'INTERVAL',
     .                   0.D0, 'TDB',   COVER               )

            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Check cardinality of coverage window.
C
            CALL CHCKSI ( 'CARDD(COVER)', CARDD( COVER  ),
     .                    '=',            CARDD( TMPWIN ),
     .                    0,              OK              )

C
C           Check coverage window.
C
            CALL CHCKAD ( 'COVER', COVER(1),
     .                    '=',     TMPWIN(1),  CARDD(TMPWIN),
     .                    0.D0,    OK                        )


            IF ( I .EQ. 6 ) THEN

               CALL SCARDD ( 0, TMPWIN )
               CALL CHCKXC ( .FALSE., ' ', OK )

               IF ( L .EQ. 2 ) THEN
C
C                 The same interval is expected to appear in the output.
C
                  CALL WNINSD ( 1.D6, 1.D7, TMPWIN )
                  CALL CHCKXC ( .FALSE., ' ', OK )

               END IF

C
C              Compare CKCOV results against those from T_CKCOV.
C
               CALL T_CKCOV ( CK,   INST(I), .FALSE., 'INTERVAL',
     .                        0.D0, 'TDB',   TMPWIN             )
               CALL CHCKXC ( .FALSE., ' ', OK )
               
C
C              Check cardinality of coverage window.
C
               CALL CHCKSI ( 'CARDD(COVER)', CARDD( COVER  ),
     .                       '=',            CARDD( TMPWIN ),
     .                       0,              OK                )

C
C              Check coverage window.
C
               CALL CHCKAD ( 'COVER', COVER(1),
     .                       '=',     TMPWIN(1),  CARDD(COVER),
     .                       0.D0,    OK                        )
            END IF

         END DO



         DO I = 1, NINS
C
C --- Case: ------------------------------------------------------
C
            TITLE = 'INST: #;  LEVEL: INTERVAL  NEEDAV: FALSE; ' //
     .              'TIMSYS: TDB; TOL: 1.D0; COVER: #.'

            CALL REPMI ( TITLE, '#', I,      TITLE )
            CALL REPMC ( TITLE, '#', CVSTAT, TITLE )
            CALL TCASE ( TITLE )

C
C           Initialize COVER.
C
            CALL SSIZED ( MAXCOV, COVER )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Adjust our expected result window by TOL.
C
            CALL COPYD ( XCVINT(LBCELL,I), TMPWIN )

            TOL = 1.D0

            CALL WNEXPD ( TOL, TOL, TMPWIN )

C
C           Make sure the window doesn't start with a negative tick
C           value.
C
            TMPWIN(1) = MAX ( TMPWIN(1), 0.D0 )

C
C           Convert the expected window to TDB.
C
            DO J = 1, CARDD(TMPWIN)

               CALL SCT2E ( CLKID(I), TMPWIN(J),   ET )
               TMPWIN(J)  =  ET

            END DO


            IF ( L .EQ. 2 ) THEN
C
C              Insert an interval into COVER.  This same interval
C              must be added to each window containing expected
C              coverage.
C
               CALL WNINSD ( 1.D6, 1.D7, COVER )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              The same interval is expected to appear in the output.
C
               CALL WNINSD ( 1.D6, 1.D7, TMPWIN )
               CALL CHCKXC ( .FALSE., ' ', OK )

            END IF

            CALL CKCOV ( CK,   INST(I), .FALSE., 'INTERVAL',
     .                   1.D0, 'TDB',   COVER               )

            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Check cardinality of coverage window.
C
            CALL CHCKSI ( 'CARDD(COVER)', CARDD( COVER  ),
     .                    '=',            CARDD( TMPWIN ),
     .                    0,              OK              )

C
C           Check coverage window.
C
            CALL CHCKAD ( 'COVER', COVER(1),
     .                    '=',     TMPWIN(1),  CARDD(TMPWIN),
     .                    0.D0,    OK                        )


            IF ( I .EQ. 6 ) THEN

               CALL SCARDD ( 0, TMPWIN )
               CALL CHCKXC ( .FALSE., ' ', OK )

               IF ( L .EQ. 2 ) THEN
C
C                 The same interval is expected to appear in the output.
C
                  CALL WNINSD ( 1.D6, 1.D7, TMPWIN )
                  CALL CHCKXC ( .FALSE., ' ', OK )

               END IF

C
C              Compare CKCOV results against those from T_CKCOV.
C
               CALL T_CKCOV ( CK,   INST(I), .FALSE., 'INTERVAL',
     .                        1.D0, 'TDB',   TMPWIN             )
               CALL CHCKXC ( .FALSE., ' ', OK )
               
C
C              Check cardinality of coverage window.
C
               CALL CHCKSI ( 'CARDD(COVER)', CARDD( COVER  ),
     .                       '=',            CARDD( TMPWIN ),
     .                       0,              OK                )

C
C              Check coverage window.
C
               CALL CHCKAD ( 'COVER', COVER(1),
     .                       '=',     TMPWIN(1),  CARDD(COVER),
     .                       0.D0,    OK                        )
            END IF

         END DO


      END DO

C
C     Error cases:
C

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Try to find coverage using time system UTC.' )

      CALL CKCOV ( CK,     INST(2),  .FALSE., 'SEGMENT',
     .             0.D0,   'UTC',    COVER              )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Try to find coverage using negative tolerance.' )

      CALL CKCOV ( CK,     INST(2), .FALSE., 'SEGMENT',
     .             -1.D0,  'TDB',    COVER              )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Try to find coverage using level "file".' )

      CALL CKCOV ( CK,     INST(2), .FALSE., 'FILE',
     .             0.D0,   'TDB',   COVER             )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Try to find coverage for a transfer CK.' )

      CALL TXTOPN ( XFRCK, XUNIT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBT  ( CK, XUNIT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CLOSE ( XUNIT )

      CALL CKCOV ( XFRCK, INST(1), .FALSE., 'SEGMENT',
     .             0.D0,  'SCLK',  COVER                )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFORMAT)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Try to find coverage for an SPK.' )


      CALL TSTSPK ( SPK, .FALSE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CKCOV  ( SPK,  INST(1), .FALSE., 'SEGMENT',
     .              0.D0, 'SCLK',  COVER               )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFILETYPE)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Try to find coverage for an EK.' )

      CALL TSTEK  ( EK, 1, 20, .FALSE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CKCOV  ( EK,   INST(1), .FALSE., 'SEGMENT',
     .              0.D0, 'SCLK',  COVER               )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDARCHTYPE)', OK )



C ******************************************************
C ******************************************************
C ******************************************************
C     CKOBJ tests
C ******************************************************
C ******************************************************
C ******************************************************

C
C --- Case: ------------------------------------------------------
C
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

C
C     Check cardinality of object set.
C
      CALL CHCKSI ( 'CARDI(IDS)', CARDI( IDS  ),
     .                 '=',       CARDI( XIDS ),
     .                 0,         OK             )

C
C     Check object set.
C
      CALL CHCKAI ( 'IDS',      IDS(1),
     .              '=',        XIDS(1),  CARDI(XIDS),   OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Find objects in our test CK.  Start with ' //
     .             'non-empty ID set.'                          )

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

C
C     Check cardinality of object set.
C
      CALL CHCKSI ( 'CARDI(IDS)', CARDI( IDS  ),
     .                 '=',       CARDI( XIDS ),
     .                 0,         OK             )

C
C     Check object set.
C
      CALL CHCKAI ( 'IDS',      IDS(1),
     .              '=',        XIDS(1),  CARDI(XIDS),   OK )
C
C     Error cases:
C


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Try to find objects for a transfer CK.' )

C
C     Initialize the IDS set.
C
      CALL SSIZEI ( NINS, IDS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CKOBJ  ( XFRCK, IDS )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFORMAT)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Try to find objects for an SPK.' )

      CALL CKOBJ ( SPK, IDS )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFILETYPE)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Try to find objects for an EK.' )


      CALL CKOBJ  ( EK, IDS )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDARCHTYPE)', OK )

C
C     Clean up.
C
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
