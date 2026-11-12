!$Procedure F_SPK14 ( SPK type 14 tests )

      SUBROUTINE F_SPK14 ( OK )
      use pepper_legacy

! $ Abstract
!
!     Exercise routines associated with SPK data type 14.
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
!     This routine tests routines specific to SPK data type 14.
!
!     In addition, the type 14 reader and evaluator routines
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
!-    TSPICE Version 1.1.0, 13-DEC-2011 (EDW)
!
!       All variables to SAVE for f2c'd version compiled with Borland.
!       Reordered header to match NAIF header specification.
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
      CHARACTER*(*)         UTC1
      PARAMETER           ( UTC1   = '1999 JUL 1' )

      CHARACTER*(*)         REF1
      PARAMETER           ( REF1   = 'J2000' )

      CHARACTER*(*)         BSP14
      PARAMETER           ( BSP14   = 'test14.bsp' )

      CHARACTER*(*)         BSP14E
      PARAMETER           ( BSP14E  = 'test14err.bsp' )

      CHARACTER*(*)         BSP14S
      PARAMETER           ( BSP14S  = 'test14sub.bsp' )

      CHARACTER*(*)         BSP14B
      PARAMETER           ( BSP14B  = 'test14big.bsp' )


      DOUBLE PRECISION      BIGSTP
      PARAMETER           ( BIGSTP = 1.D1 )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-11 )

      DOUBLE PRECISION      LOOSE
      PARAMETER           ( LOOSE  = 1.D-3 )

      DOUBLE PRECISION      SLOPPY
      PARAMETER           ( SLOPPY = 1.D-1 )



      INTEGER               BIGN
      PARAMETER           ( BIGN   = 10101 )

      INTEGER               BIGID
      PARAMETER           ( BIGID  = -10000 )

      INTEGER               BIGCTR
      PARAMETER           ( BIGCTR = 5 )

      INTEGER               CHBDEG
      PARAMETER           ( CHBDEG = 2 )

      INTEGER               DSCSIZ
      PARAMETER           ( DSCSIZ = 5 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 240 )

      INTEGER               NCHBRC
      PARAMETER           ( NCHBRC = 4 )

      INTEGER               ND
      PARAMETER           ( ND     = 2 )

      INTEGER               NEPOCH
      PARAMETER           ( NEPOCH = NCHBRC + 1 )

      INTEGER               NI
      PARAMETER           ( NI     = 6 )

      INTEGER               POLDEG
      PARAMETER           ( POLDEG = 3 )

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
      DOUBLE PRECISION      CHBR14 ( 2+6*(CHBDEG+1), NCHBRC )
      DOUBLE PRECISION      CHBCFB ( 0:CHBDEG,   6,  BIGN   )
      DOUBLE PRECISION      CHBCFF ( 0:CHBDEG,   6          )
      DOUBLE PRECISION      DC     (    ND     )
      DOUBLE PRECISION      DESCR  (    DSCSIZ )
      DOUBLE PRECISION      DSCEPC (    NEPOCH )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      INTLEN
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      MIDPT
      DOUBLE PRECISION      PACKET ( 2 + 6*(CHBDEG+1) )
      DOUBLE PRECISION      RADIUS
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
      INTEGER               PAKSIZ
      INTEGER               XBODY
      INTEGER               XCENTR

      LOGICAL               FOUND


!
!     Saved values
!
      SAVE

!
!     Initial values
!

!
!     Epochs associated with coefficient sets.
!
      DATA  DSCEPC / 100.D0, 200.D0, 300.D0, 400.D0, 500.D0 /


!
!     Chebyshev coefficients for testing SPK type 14 routines:
!
      DATA (  CHBR14(I,1),   I = 1,  2 + 6*(CHBDEG+1)  ) &
       / &
              150.D0, &
              50.D0, &
              1.0101D0, 1.0102D0, 1.0103D0, &
              1.0201D0, 1.0202D0, 1.0203D0, &
              1.0301D0, 1.0302D0, 1.0303D0, &
              1.0401D0, 1.0402D0, 1.0403D0, &
              1.0501D0, 1.0502D0, 1.0503D0, &
              1.0601D0, 1.0602D0, 1.0603D0 &
       /


      DATA (  CHBR14(I,2),   I = 1,  2 + 6*(CHBDEG+1)  ) &
       / &
              250.D0, &
              50.D0, &
              2.0101D0, 2.0102D0, 2.0103D0, &
              2.0201D0, 2.0202D0, 2.0203D0, &
              2.0301D0, 2.0302D0, 2.0303D0, &
              2.0401D0, 2.0402D0, 2.0403D0, &
              2.0501D0, 2.0502D0, 2.0503D0, &
              2.0601D0, 2.0602D0, 2.0603D0 &
       /


      DATA (  CHBR14(I,3),   I = 1,  2 + 6*(CHBDEG+1)  ) &
       / &
              350.D0, &
              50.D0, &
              3.0101D0, 3.0102D0, 3.0103D0, &
              3.0201D0, 3.0202D0, 3.0203D0, &
              3.0301D0, 3.0302D0, 3.0303D0, &
              3.0401D0, 3.0402D0, 3.0403D0, &
              3.0501D0, 3.0502D0, 3.0503D0, &
              3.0601D0, 3.0602D0, 3.0603D0 &
       /


      DATA (  CHBR14(I,4),   I = 1,  2 + 6*(CHBDEG+1)  ) &
       / &
              450.D0, &
              50.D0, &
              4.0101D0, 4.0102D0, 4.0103D0, &
              4.0201D0, 4.0202D0, 4.0203D0, &
              4.0301D0, 4.0302D0, 4.0303D0, &
              4.0401D0, 4.0402D0, 4.0403D0, &
              4.0501D0, 4.0502D0, 4.0503D0, &
              4.0601D0, 4.0602D0, 4.0603D0 &
       /

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
      CALL TOPEN ( 'F_SPK14' )


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
!     Test SPK14B:  start out with error handling.
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad coefficient set count.' )

      IF ( EXISTS(BSP14E) ) THEN

         CALL DELFIL ( BSP14E )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF

      CALL SPKOPN ( BSP14E, 'Type 14 SPK internal file name', 4, &
                    HANDLE                                       )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XBODY  = 301
      XCENTR = 3
      XREF   = 'J2000'
      SEGID  = 'SPK Type 14 test segment'


      CALL SPK14B ( HANDLE, &
                    SEGID, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    DSCEPC(1), &
                    DSCEPC(NCHBRC+1), &
                    -1               )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDARGUMENT)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid reference frame.' )

      CALL SPK14B ( HANDLE, &
                    SEGID, &
                    XBODY, &
                    XCENTR, &
                    'SPUD', &
                    DSCEPC(1), &
                    DSCEPC(NCHBRC+1), &
                    NCHBRC            )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDREFFRAME)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Descriptor times out of order.' )

      CALL SPK14B ( HANDLE, &
                    SEGID, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    DSCEPC(NCHBRC+1), &
                    DSCEPC(1), &
                    NCHBRC            )

      CALL CHCKXC ( .TRUE., 'SPICE(BADDESCRTIMES)', OK )


!
!     Close this file.  Note that the file contains no segments,
!     so SPKCLS won't close it.
!
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Test SPK14a, SPK14b, SPK14c: write small segment.' )


      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'

!
!     Create a segment identifier.
!
      SEGID = 'SPK type 14 test segment'

!
!     Open a new SPK file.
!
      IF ( EXISTS(BSP14) ) THEN
         CALL DELFIL(BSP14)
      END IF

      CALL SPKOPN ( BSP14, 'Type 14 SPK internal file name', 4, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create a type 14 segment.
!
!
!     Begin the segment.
!
      CALL SPK14B ( HANDLE, &
                    SEGID, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    DSCEPC(1), &
                    DSCEPC(NCHBRC+1), &
                    CHBDEG           )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Add data.
!
      CALL SPK14A ( HANDLE, NCHBRC, CHBR14, DSCEPC )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     End the segment.
!
      CALL SPK14E ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Close the SPK file.
!
      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Test SPKR014, SPKE014: read small segment.' )

!
!     Load the SPK file.
!
      CALL SPKLEF ( BSP14, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Look up states for each epoch in our list.  Compare.
!
      INTLEN = DSCEPC(2) - DSCEPC(1)

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
!        Capture the Ith coefficient packet in an array that is
!        more easily indexed.
!
         PAKSIZ = (CHBDEG+1) * 6

         CALL MOVED ( CHBR14(3,I), PAKSIZ, CHBCFF )

!
!        Evaluate the position "manually."
!
         THETA = ( ET - MIDPT ) / RADIUS

         DO J = 1, 6

            XSTATE(J) = 0.D0

            DO K = 0, CHBDEG

               XSTATE(J  ) =  XSTATE(J) + CHBCFF(K,J) * T( K, THETA )

            END DO

         END DO

         CALL CHCKAD ( 'type 14 position', &
                       STATE, &
                       '~', &
                       XSTATE, &
                       3, &
                       TIGHT, &
                       OK             )

         CALL CHCKAD ( 'type 14 velocity', &
                       STATE(4), &
                       '~', &
                       XSTATE(4), &
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
      CALL TCASE ( 'Test SPKS14: write new file having a segment '  // &
                   'created by subsetting small segment from BSP14.' )


      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'

!
!     Create a segment identifier.
!
      SEGID = 'SPK type 14 test subset segment'

!
!     Open a new SPK file.
!
      IF ( EXISTS(BSP14S) ) THEN
         CALL DELFIL(BSP14S)
      END IF

      CALL SPKOPN ( BSP14S, 'Type 14 SPK internal file name', 0, NEWH )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Open SPK14 and extract segment descriptor and ID of first segment.
!
      CALL DAFOPR ( BSP14, HANDLE )
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
!     Create a type 14 segment in new file.  Shorten the time
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
      CALL TCASE ( 'Test SPKS14: check descriptor bounds on ' // &
                   'subsetted file.'                          )

!
!     Open SPK14S and extract segment descriptor and ID of first
!     segment.
!
      CALL DAFOPR ( BSP14S, HANDLE )
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
      CALL TCASE ( 'Test SPKS14: read states from subsetted file.' )

!
!     Load the SPK file.
!
      CALL SPKLEF ( BSP14S, HANDLE )
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
!        Capture the Ith coefficient packet in an array that is
!        more easily indexed.
!
         PAKSIZ = (CHBDEG+1) * 6

         CALL MOVED ( CHBR14(3,I), PAKSIZ, CHBCFF )

!
!        Evaluate the position "manually."
!
         THETA = ( ET - MIDPT ) / RADIUS

          DO J = 1, 6

            XSTATE(J) = 0.D0

            DO K = 0, CHBDEG

               XSTATE(J  ) =  XSTATE(J) + CHBCFF(K,J) * T( K, THETA )

            END DO

         END DO

         CALL CHCKAD ( 'type 14 position', &
                       STATE, &
                       '~', &
                       XSTATE, &
                       3, &
                       TIGHT, &
                       OK             )

         CALL CHCKAD ( 'type 14 velocity', &
                       STATE(4), &
                       '~', &
                       XSTATE(4), &
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
      CALL TCASE ( 'SPKR14/SPKE14 test:  create a large segment with '// &
                   'multiple directories.'                            )

!
!     Create the coefficient and epoch values we'll use. We're going to
!     follow a pattern similar to that used for the smaller segments
!     created so far: each coefficient will have the value
!
!        I + J*10**-4 + K*10**-8
!
!     where I is the coefficient set index, J is the component (X,Y, or
!     Z) index, and K-1 is the associated degree.
!
      DO I = 1, BIGN

         DO J = 1, 6

            DO K = 0, CHBDEG

               CHBCFB(K, J,   I ) = I   +   J * 1.D-4   +   K * 1.D-8

            END DO

         END DO

      END DO

      DO I = 1, BIGN+1
!
!        Initialize the Ith epoch.
!
         BEPLST(I) = 100 * I

      END DO


      XBODY  = 14
      XCENTR = 10
      XREF   = 'J2000'

!
!     Create a segment identifier.
!
      SEGID = 'SPK type 14 big test segment'

!
!     Open a new SPK file.
!
      IF ( EXISTS(BSP14B) ) THEN
         CALL DELFIL(BSP14B)
      END IF

      CALL SPKOPN ( BSP14B, 'Type 14 SPK internal file name', 0, &
                    HANDLE                                      )
      CALL CHCKXC ( .FALSE., ' ', OK )



!     Begin the segment.
!
      CALL SPK14B ( HANDLE, &
                    SEGID, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    BEPLST(1), &
                    BEPLST(BIGN+1), &
                    CHBDEG            )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Add data.
!
      INTLEN = BEPLST(2) - BEPLST(1)

      DO I = 1, BIGN

         RADIUS    = 0.5D0 * INTLEN
         MIDPT     = BEPLST(I) + RADIUS

         PACKET(1) = MIDPT
         PACKET(2) = RADIUS

         CALL MOVED ( CHBCFB(0,1,I), 6*(CHBDEG+1), PACKET(3) )

         CALL SPK14A ( HANDLE, 1, PACKET, BEPLST(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


!
!     End the segment.
!
      CALL SPK14E ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close the SPK file.
!
      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Test SPKR14, SPKE14: read states from large ' // &
                   'file.' )

!
!     Load the SPK file.
!
      CALL SPKLEF ( BSP14B, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XBODY  = 14
      XCENTR = 10
      XREF   = 'J2000'
      INTLEN = BEPLST(2) - BEPLST(1)

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
!        Capture the Ith coefficient packet in an array that is
!        more easily indexed.
!
         PAKSIZ = (CHBDEG+1) * 6

         CALL MOVED ( CHBCFB(0,1,I), PAKSIZ, CHBCFF )

!
!        Evaluate the position "manually."
!
         THETA = ( ET - MIDPT ) / RADIUS

         DO J = 1, 6

            XSTATE(J) = 0.D0

            DO K = 0, CHBDEG

               XSTATE(J)   =   XSTATE(J) + CHBCFF(K,J) * T( K, THETA )

            END DO

         END DO

         CALL CHCKAD ( 'type 14 position', &
                       STATE, &
                       '~', &
                       XSTATE, &
                       3, &
                       TIGHT, &
                       OK             )

         CALL CHCKAD ( 'type 14 velocity', &
                       STATE(4), &
                       '~', &
                       XSTATE(4), &
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
      CALL DELFIL ( BSP14  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( BSP14E )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( BSP14S )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( BSP14B )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


