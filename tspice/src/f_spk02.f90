!$Procedure F_SPK02 ( SPK type 2 tests )

      SUBROUTINE F_SPK02 ( OK )
      use pepper_legacy
      use pepper_inc_spk02
      use pepper_inc_spkrec

! $ Abstract
!
!     Exercise routines associated with SPK data type 2.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TEST FAMILY
!
! $ Declarations

      IMPLICIT NONE


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
!     This routine tests routines specific to SPK data type 2.
!
!     In addition, the type 2 reader and evaluator routines
!     are indirectly exercised by this test family.
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
!     N.J. Bachman     (JPL)
!
! $ Version
!
!-    TSPICE Version 2.0.0, 17-JAN-2014 (NJB)
!
!        Updated to use new short error messages for coverage gaps.
!        Added test cases for new SPKE02 and SPKW02 error checks.
!        Deleted declarations of unused variables. Re-ordered header
!        sections.
!
!-    TSPICE Version 1.0.0, 20-OCT-2005 (NJB)
!
!
!-&

!
!     SPICELIB functions
!

!
!     Local Parameters
!
      CHARACTER*(*)         SPK2
      PARAMETER           ( SPK2   = 'test2.bsp' )

      CHARACTER*(*)         SPK2E
      PARAMETER           ( SPK2E  = 'test2err.bsp' )

      CHARACTER*(*)         SPK2S
      PARAMETER           ( SPK2S  = 'test2sub.bsp' )

      CHARACTER*(*)         SPK2B
      PARAMETER           ( SPK2B  = 'test2big.bsp' )


      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-11 )


      INTEGER               BIGN
      PARAMETER           ( BIGN   = 10101 )

      INTEGER               CHBDEG
      PARAMETER           ( CHBDEG = 2 )

      INTEGER               DSCSIZ
      PARAMETER           ( DSCSIZ = 5 )

      INTEGER               NCHBRC
      PARAMETER           ( NCHBRC = 4 )

      INTEGER               ND
      PARAMETER           ( ND     = 2 )

      INTEGER               NEPOCH
      PARAMETER           ( NEPOCH = NCHBRC + 1 )

      INTEGER               NI
      PARAMETER           ( NI     = 6 )

      INTEGER               SIDLEN
      PARAMETER           ( SIDLEN = 60 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

!
!     Local Variables
!
      CHARACTER*(SIDLEN)    SEGID
      CHARACTER*(SIDLEN)    SEGID2
      CHARACTER*(FRNMLN)    XREF

      DOUBLE PRECISION      BEPLST (    BIGN+1 )
      DOUBLE PRECISION      CHBCF2 ( 0:CHBDEG, 3, NCHBRC )
      DOUBLE PRECISION      CHBCFB ( 0:CHBDEG, 3, BIGN   )
      DOUBLE PRECISION      DC     (    ND     )
      DOUBLE PRECISION      DESCR  (    DSCSIZ )
      DOUBLE PRECISION      DSCEPC (    NEPOCH )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      INTLEN
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      MIDPT
      DOUBLE PRECISION      RADIUS
      DOUBLE PRECISION      RECORD ( MAXREC )
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      T
      DOUBLE PRECISION      THETA
      DOUBLE PRECISION      XSTATE ( 6 )

      INTEGER               HANDLE
      INTEGER               I
      INTEGER               IC     ( NI )
      INTEGER               J
      INTEGER               K
      INTEGER               N
      INTEGER               NEWH
      INTEGER               XBODY
      INTEGER               XCENTR

      LOGICAL               FOUND


!
!     Saved values
!
      SAVE                  DSCEPC
      SAVE                  CHBCF2

!
!     Initial values
!

!
!     Epochs associated with coefficient sets.
!
      DATA  DSCEPC / 100.D0, 200.D0, 300.D0, 400.D0, 500.D0 /


!
!     Chebyshev coefficients for testing SPKW02.
!
      DATA  CHBCF2 / 1.0101D0, 1.0102D0, 1.0103D0, &
                     1.0201D0, 1.0202D0, 1.0203D0, &
                     1.0301D0, 1.0302D0, 1.0303D0, &
                     2.0101D0, 2.0102D0, 2.0103D0, &
                     2.0201D0, 2.0202D0, 2.0203D0, &
                     2.0301D0, 2.0302D0, 2.0303D0, &
                     3.0101D0, 3.0102D0, 3.0103D0, &
                     3.0201D0, 3.0202D0, 3.0203D0, &
                     3.0301D0, 3.0302D0, 3.0303D0, &
                     4.0101D0, 4.0102D0, 4.0103D0, &
                     4.0201D0, 4.0202D0, 4.0203D0, &
                     4.0301D0, 4.0302D0, 4.0303D0  /


!
!     Statement functions
!
!
!     T(n,theta) represents the Chebyshev polynomial
!
!       T ( theta )
!        n
!
      T ( N, THETA ) = COS(  N*ACOS( MIN( 1.D0, MAX(-1.D0, THETA) ) )  )




!
!     Open the test family.
!
      CALL TOPEN ( 'F_SPK02' )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Setup' )

!
!     Create and load a leapseconds kernel.
!
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Test SPKW02:  start out with error handling.
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad coefficient set count.' )

      IF ( EXISTS(SPK2E) ) THEN

         CALL DELFIL ( SPK2E )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF

      CALL SPKOPN ( SPK2E, 'Type 2 SPK internal file name', 4, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XBODY  = 301
      XCENTR = 3
      XREF   = 'J2000'
      SEGID  = 'SPK Type 2 test segment'

      INTLEN = DSCEPC(2) - DSCEPC(1)

      CALL SPKW02 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    DSCEPC(1), &
                    DSCEPC(NCHBRC+1), &
                    SEGID, &
                    INTLEN, &
                    0, &
                    CHBDEG, &
                    CHBCF2, &
                    DSCEPC(1) )

      CALL CHCKXC ( .TRUE., 'SPICE(NUMCOEFFSNOTPOS)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Polynomial degree too high.' )

!
!     POLYDG = MAXDEG + 1
!
      CALL SPKW02 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    DSCEPC(1), &
                    DSCEPC(NCHBRC+1), &
                    SEGID, &
                    INTLEN, &
                    NCHBRC, &
                    28, &
                    CHBCF2, &
                    DSCEPC(1) )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDEGREE)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Polynomial degree too low.' )

      CALL SPKW02 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    DSCEPC(1), &
                    DSCEPC(NCHBRC+1), &
                    SEGID, &
                    INTLEN, &
                    NCHBRC, &
                    -1, &
                    CHBCF2, &
                    DSCEPC(1) )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDEGREE)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid interval length.' )

      INTLEN = DSCEPC(1) - DSCEPC(2)

      CALL SPKW02 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    DSCEPC(1), &
                    DSCEPC(NCHBRC+1), &
                    SEGID, &
                    INTLEN, &
                    NCHBRC, &
                    CHBDEG, &
                    CHBCF2, &
                    DSCEPC(1) )

      CALL CHCKXC ( .TRUE., 'SPICE(INTLENNOTPOS)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid reference frame.' )

      INTLEN = DSCEPC(2) - DSCEPC(1)

      CALL SPKW02 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    'SPUD', &
                    DSCEPC(1), &
                    DSCEPC(NCHBRC+1), &
                    SEGID, &
                    INTLEN, &
                    NCHBRC, &
                    CHBDEG, &
                    CHBCF2, &
                    DSCEPC(1) )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDREFFRAME)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Descriptor times out of order.' )

      INTLEN = DSCEPC(2) - DSCEPC(1)

      CALL SPKW02 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    DSCEPC(NCHBRC+1), &
                    DSCEPC(1), &
                    SEGID, &
                    INTLEN, &
                    NCHBRC, &
                    CHBDEG, &
                    CHBCF2, &
                    DSCEPC(1) )

      CALL CHCKXC ( .TRUE., 'SPICE(BADDESCRTIMES)', OK )

!      CALL DAFCLS ( HANDLE )
!      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Descriptor start time is too early.' )

      INTLEN = DSCEPC(2) - DSCEPC(1)

      CALL SPKW02 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    DSCEPC(1) - 1.D-3, &
                    DSCEPC(NCHBRC+1), &
                    SEGID, &
                    INTLEN, &
                    NCHBRC, &
                    CHBDEG, &
                    CHBCF2, &
                    DSCEPC(1) )

      CALL CHCKXC ( .TRUE., 'SPICE(COVERAGEGAP)', OK )

      CALL SPKW02 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    DSCEPC(1) - 1.D-13, &
                    DSCEPC(NCHBRC+1), &
                    SEGID, &
                    INTLEN, &
                    NCHBRC, &
                    CHBDEG, &
                    CHBCF2, &
                    DSCEPC(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Descriptor stop time is too late.' )

      INTLEN = DSCEPC(2) - DSCEPC(1)

      CALL SPKW02 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    DSCEPC(1), &
                    DSCEPC(NCHBRC+1) + 1.D-3, &
                    SEGID, &
                    INTLEN, &
                    NCHBRC, &
                    CHBDEG, &
                    CHBCF2, &
                    DSCEPC(1) )

      CALL CHCKXC ( .TRUE., 'SPICE(COVERAGEGAP)', OK )

      CALL SPKW02 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    DSCEPC(1), &
                    DSCEPC(NCHBRC+1) + 1.D-13, &
                    SEGID, &
                    INTLEN, &
                    NCHBRC, &
                    CHBDEG, &
                    CHBCF2, &
                    DSCEPC(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Close this file.  Note that the file contains no segments,
!     so SPKCLS won't close it.
!
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Test SPKE02: bad coefficient count.' )

      CALL CLEARD ( MAXREC, RECORD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      RECORD(1) = 0

      CALL SPKE02 ( 0.D0, RECORD, STATE )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Test SPKE02: bad interval radius.' )

      CALL CLEARD ( MAXREC, RECORD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      RECORD(1) = 5
      RECORD(3) = 0.D0

      CALL SPKE02 ( 0.D0, RECORD, STATE )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDRADIUS)', OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Test SPKW02: write small segment.' )


      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'

!
!     Create a segment identifier.
!
      SEGID = 'SPK type 2 test segment'

!
!     Open a new SPK file.
!
      IF ( EXISTS(SPK2) ) THEN
         CALL DELFIL(SPK2)
      END IF

      CALL SPKOPN ( SPK2, 'Type 2 SPK internal file name', 4, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


      INTLEN = DSCEPC(2) - DSCEPC(1)

!
!     Create a type 2 segment.
!
      CALL SPKW02 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    DSCEPC(1), &
                    DSCEPC(NCHBRC+1), &
                    SEGID, &
                    INTLEN, &
                    NCHBRC, &
                    CHBDEG, &
                    CHBCF2, &
                    DSCEPC(1) )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close the SPK file.
!
      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Test SPKR02, SPKE02: read small segment.' )

!
!     Load the SPK file.
!
      CALL SPKLEF ( SPK2, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Look up states for each epoch in our list.  Compare.
!
      DO I = 1, NCHBRC
!
!        Generate look-up epoch ET.
!
         RADIUS = 0.5D0 * INTLEN
         MIDPT  = DSCEPC(I) + RADIUS

         ET     = MIDPT + 0.5D0 * RADIUS

         CALL SPKGEO ( XBODY, ET, XREF, XCENTR, STATE, LT )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Evaluate the position "manually."
!
         THETA = ( ET - MIDPT ) / RADIUS

         DO J = 1, 3

            XSTATE(J) = 0.D0

            DO K = 0, CHBDEG

               XSTATE(J) = XSTATE(J)  +  CHBCF2(K,J,I) * T( K, THETA )

            END DO

         END DO

         CALL CHCKAD ( 'type 2 position', &
                       STATE, &
                       '~', &
                       XSTATE, &
                       3, &
                       TIGHT, &
                       OK             )
      END DO
!
!     Unload the SPK file.
!
      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Test SPKS02: write new file having a segment '  // &
                   'created by subsetting small segment from SPK2.' )


      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'

!
!     Create a segment identifier.
!
      SEGID = 'SPK type 2 test subset segment'

!
!     Open a new SPK file.
!
      IF ( EXISTS(SPK2S) ) THEN
         CALL DELFIL(SPK2S)
      END IF

      CALL SPKOPN ( SPK2S, 'Type 2 SPK internal file name', 0, NEWH )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Open SPK2 and extract segment descriptor and ID of first segment.
!
      CALL DAFOPR ( SPK2, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create a type 2 segment in new file.  Shorten the time
!     coverage by knocking off the coverage contributed by
!     the first and last packets of the source segment.
!
      CALL SPKSUB ( HANDLE, &
                    DESCR, &
                    SEGID, &
                    DSCEPC(2), &
                    DSCEPC(NEPOCH-1), &
                    NEWH               )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close the new SPK file.
!
      CALL SPKCLS ( NEWH )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close the old SPK file.
!
      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Test SPKS02: check descriptor bounds on ' // &
                   'subsetted file.'                          )

!
!     Open SPK2S and extract segment descriptor and ID of first
!     segment.
!
      CALL DAFOPR ( SPK2S, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFUS  ( DESCR, ND, NI, DC, IC )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the time bounds.
!
      I = NEPOCH-1

      CALL CHCKSD ( 'Segment start', DC(1), '=', DSCEPC(2), 0.D0, OK )
      CALL CHCKSD ( 'Segment end',   DC(2), '=', DSCEPC(I), 0.D0, OK )

      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Test SPKS02: read states from subsetted file.' )

!
!     Load the SPK file.
!
      CALL SPKLEF ( SPK2S, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
      INTLEN = DSCEPC(2) - DSCEPC(1)

!
!     Look up states for each epoch in our list.  Compare.
!
      DO I = 2, NCHBRC-1
!
!        Generate look-up epoch ET.
!
         RADIUS = 0.5D0 * INTLEN
         MIDPT  = DSCEPC(I) + RADIUS

         ET     = MIDPT + 0.5D0 * RADIUS

         CALL SPKGEO ( XBODY, ET, XREF, XCENTR, STATE, LT )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Evaluate the position "manually."
!
         THETA = ( ET - MIDPT ) / RADIUS

         DO J = 1, 3

            XSTATE(J) = 0.D0

            DO K = 0, CHBDEG

               XSTATE(J) = XSTATE(J)  +  CHBCF2(K,J,I) * T( K, THETA )

            END DO

         END DO

         CALL CHCKAD ( 'type 2 position', &
                       STATE, &
                       '~', &
                       XSTATE, &
                       3, &
                       TIGHT, &
                       OK             )
      END DO
!
!     Unload the SPK file.
!
      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKR02/SPKE02 test:  create a large segment with '// &
                   'multiple directories.'                            )

!
!     Create the coefficient and epoch values we'll use. We're going to
!     follow a pattern similar to that used for the smaller segments
!     created so far: each coefficient will have the value
!
!        I + J*10**-5 + K*10**-10
!
!     where I is the coefficient set index, J is the component (X,Y, or
!     Z) index, and K-1 is the associated degree.
!
      DO I = 1, BIGN

         DO J = 1, 3

            DO K = 0, CHBDEG

               CHBCFB(K,J,I) = I   +   J * 1.D-5   +   K * 1.D-10

            END DO

         END DO

      END DO

      DO I = 1, BIGN+1
!
!        Initialize the Ith epoch.
!
         BEPLST(I) = 100 * I

      END DO


      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'

!
!     Create a segment identifier.
!
      SEGID = 'SPK type 2 big test segment'

!
!     Open a new SPK file.
!
      IF ( EXISTS(SPK2B) ) THEN
         CALL DELFIL(SPK2B)
      END IF

      CALL SPKOPN ( SPK2B, 'Type 2 SPK internal file name', 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


      INTLEN = DSCEPC(2) - DSCEPC(1)

!
!     Create a type 2 segment.
!
      CALL SPKW02 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    BEPLST(1), &
                    BEPLST(BIGN+1), &
                    SEGID, &
                    INTLEN, &
                    BIGN, &
                    CHBDEG, &
                    CHBCFB, &
                    BEPLST(1) )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close the SPK file.
!
      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Test SPKS02: read states from large file.' )

!
!     Load the SPK file.
!
      CALL SPKLEF ( SPK2B, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
      INTLEN = DSCEPC(2) - DSCEPC(1)

!
!     Look up states for each epoch in our list.  Compare.
!
      DO I = 1, BIGN
!
!        Generate look-up epoch ET.
!
         RADIUS = 0.5D0 * INTLEN
         MIDPT  = BEPLST(I) + RADIUS

         ET     = MIDPT + 0.5D0 * RADIUS

         CALL SPKGEO ( XBODY, ET, XREF, XCENTR, STATE, LT )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Evaluate the position "manually."
!
         THETA = ( ET - MIDPT ) / RADIUS

         DO J = 1, 3

            XSTATE(J) = 0.D0

            DO K = 0, CHBDEG

               XSTATE(J) = XSTATE(J)  +  CHBCFB(K,J,I) * T( K, THETA )

            END DO

         END DO

         CALL CHCKAD ( 'type 2 position', &
                       STATE, &
                       '~', &
                       XSTATE, &
                       3, &
                       TIGHT, &
                       OK             )
      END DO
!
!     Unload the SPK file.
!
      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Deleting SPK files at clean-up time.' )

!
!     Clean up the SPK files.
!
      CALL DELFIL ( SPK2  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK2E )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK2S )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK2B )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


