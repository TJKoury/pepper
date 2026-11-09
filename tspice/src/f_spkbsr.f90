!$Procedure F_SPKBSR ( Family of tests for T_SSFS )

      SUBROUTINE F_SPKBSR ( OK )

! $ Abstract
!
!     This routine tests the SPK segment selection and buffering system,
!     which is implemented by T_SSFS.
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
!     OK       is a logical that indicates the test status to the
!              caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  This routine does not generate any errors. Routines in its
!         call tree may generate errors that are either intentional and
!         trapped or unintentional and need reporting. The test family
!         utilities manage this.
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    TSPICE Version 2.1.0, 05-JUN-2021 (BVS) (JDR)
!
!        Added missing 'D' to literal DP constants.
!
!-    TSPICE Version 2.0.0, 17-MAR-2014 (NJB)
!
!        Table size parameters
!
!           FTSIZE
!           ITSIZE
!           STSIZE
!
!        were modified for compatibility with
!
!           T_SPKBSR
!
!-    TSPICE Version 1.1.0, 13-DEC-2011 (EDW)
!
!        All variables to SAVE for f2c'd version compiled with Borland.
!        Implemented a proper SPICE header.
!
!-    TSPICE Version 1.0.0, 28-NOV-2001 (NJB)
!
!-&


! $ Index_Entries
!
!     Test SPK segment buffering system
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               FTSIZE
      PARAMETER           ( FTSIZE = 500 )

      INTEGER               STSIZE
      PARAMETER           ( STSIZE = 1000 )

      INTEGER               BTSIZE
      PARAMETER           ( BTSIZE = 100 )

      INTEGER               DSCSIZ
      PARAMETER           ( DSCSIZ = 5 )

      INTEGER               ND
      PARAMETER           ( ND     = 2 )

      INTEGER               NI
      PARAMETER           ( NI     = 6 )

      INTEGER               SIDLEN
      PARAMETER           ( SIDLEN = 40 )

      INTEGER               MAXSEG
      PARAMETER           ( MAXSEG = 16000 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               NBODY
      PARAMETER           ( NBODY  = 4 )

      INTEGER               NSPK
      PARAMETER           ( NSPK   = 10 )

!
!     The number of segments in the respective SPK files:
!
      INTEGER               NSEG1
      PARAMETER           ( NSEG1  = 1 )

      INTEGER               NSEG2
      PARAMETER           ( NSEG2  = STSIZE/2 )

      INTEGER               NSEG3
      PARAMETER           ( NSEG3  = STSIZE/2 )

      INTEGER               NSEG4
      PARAMETER           ( NSEG4  = STSIZE )

      INTEGER               NSEG5
      PARAMETER           ( NSEG5  = STSIZE+10 )

      INTEGER               NSEG6
      PARAMETER           ( NSEG6  = BTSIZE )

      INTEGER               NSEG7
      PARAMETER           ( NSEG7  = 23 )

      INTEGER               NSEG8
      PARAMETER           ( NSEG8  = NSEG7 )

      INTEGER               NSEG9
      PARAMETER           ( NSEG9  = NBODY * STSIZE )

      INTEGER               NSEG10
      PARAMETER           ( NSEG10 = NBODY * (STSIZE-3) )


!
!     Other parameters:
!
      INTEGER               FILSIZ
      PARAMETER           ( FILSIZ = 255 )

      INTEGER               SCALE
      PARAMETER           ( SCALE  = 10000 )

      INTEGER               SMSLEN
      PARAMETER           ( SMSLEN = 25 )

      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 50 )

!
!     Local variables
!
      CHARACTER*(SIDLEN)    SEGID
      CHARACTER*(SMSLEN)    SMSG
      CHARACTER*(FILSIZ)    SPKCPY ( FTSIZE )
      CHARACTER*(FILSIZ)    SPKS   ( NSPK )
      CHARACTER*(SIDLEN)    XSEGID ( MAXSEG )

      DOUBLE PRECISION      DESCR  ( DSCSIZ )
      DOUBLE PRECISION      T
      DOUBLE PRECISION      TBEGS  ( MAXSEG )
      DOUBLE PRECISION      TENDS  ( MAXSEG )
      DOUBLE PRECISION      XDESCR ( DSCSIZ, MAXSEG )

      INTEGER               BODY
      INTEGER               CPYHAN ( FTSIZE )
      INTEGER               HANDLE
      INTEGER               HNDLES ( NSPK )
      INTEGER               I
      INTEGER               IDS    ( MAXSEG )
      INTEGER               J
      INTEGER               NSEG   ( NSPK )
      INTEGER               SEGNO
      INTEGER               SPKNO

      LOGICAL               FOUND

!
!     Saved variables
!
      SAVE

!
!     Initial values
!
      DATA                  SPKS   / 'sfs1.bsp', 'sfs2.bsp', &
                                     'sfs3.bsp', 'sfs4.bsp', &
                                     'sfs5.bsp', 'sfs6.bsp', &
                                     'sfs7.bsp', 'sfs8.bsp', &
                                     'sfs9.bsp', 'sfs10.bsp'   /

      DATA                  NSEG   / NSEG1,  NSEG2,  NSEG3,  NSEG4, &
                                     NSEG5,  NSEG6,  NSEG7,  NSEG8, &
                                     NSEG9,  NSEG10                /


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_SPKBSR' )




      CALL TCASE ( 'The first SPK file contains 1 segment for body ' // &
                   '1. Make sure we can look up data from this file.' )

!
!     Create the first SPK file.
!
      BODY     = 1
      TBEGS(1) = SCALE + 0.D0
      TENDS(1) = SCALE + 1.D0
      SPKNO    = 1

      XSEGID(1) = 'File: # Segno: #'
      CALL REPMC ( XSEGID(1), '#', SPKS(1), XSEGID(1) )
      CALL REPMI ( XSEGID(1), '#', 1,       XSEGID(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CRDAF ( 'SPK',  SPKS(1),  NSEG(1), &
                      BODY,  TBEGS,    TENDS,   XSEGID )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_SLEF ( SPKS(1), HNDLES(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_SSFS ( BODY, TBEGS(1)+0.5D0, HANDLE, DESCR, SEGID, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     In this case, the segment should be found.  Make sure we get
!     back the right handle and segment identifier.
!
      CALL CHCKSL ( 'FOUND',  FOUND,      .TRUE.,        OK )
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(1), 0, OK )
      CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(1),    OK )

!
!     Check the descriptor as well.  However, don't check the
!     segment addresses.
!
      CALL T_CRDESC ( 'SPK', 1, BODY, TBEGS(1), TENDS(1), XDESCR(1,1) )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,1), DSCSIZ-1, &
                     0.D0,   OK )





      CALL TCASE ( 'Try to look up data for a different body in ' // &
                   'SPK 1.  Also look up data for body 1 for a '  // &
                   'time which is not covered.'                    )

      CALL T_SSFS ( 2, TBEGS(1)+0.5D0, HANDLE, DESCR, SEGID, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     In this case, the segment should not be found.
!
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,    OK )


      CALL T_SSFS ( 1, TBEGS(1)+10, HANDLE, DESCR, SEGID, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     In this case, the segment should not be found.
!
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,    OK )






      CALL TCASE ( 'Create a second SPK containing data for body 1 '// &
                   'and body 2.  Load this SPK, ' // &
                   'then look up a state covered by the new file.'    )

      BODY   = 1
      SPKNO  = 2

      DO I = 1, NSEG(SPKNO)

         IF ( I .LE. NSEG(SPKNO)/2 ) THEN
            IDS(I)   = 2
         ELSE
            IDS(I)   = 1
         END IF

         TBEGS(I) = SPKNO * SCALE + I - 1
         TENDS(I) = TBEGS(I)          + 1

         XSEGID(I) = 'File: # Segno: #'

         CALL REPMC ( XSEGID(I), '#', SPKS(SPKNO), XSEGID(I) )
         CALL REPMI ( XSEGID(I), '#', I,           XSEGID(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


      CALL T_CRDAF ( 'SPK',  SPKS(SPKNO),  NSEG(SPKNO), &
                      IDS,   TBEGS,        TENDS,       XSEGID )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_SLEF ( SPKS(SPKNO), HNDLES(SPKNO) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SEGNO = NSEG(SPKNO)

      CALL T_SSFS ( BODY, TBEGS(SEGNO)+0.5D0, HANDLE, DESCR, SEGID, &
                    FOUND                                             )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     In this case, the segment should be found.  Make sure we get
!     back the right handle and segment identifier.
!
      CALL CHCKSL ( 'FOUND',  FOUND,      .TRUE.,            OK )
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(SPKNO), 0, OK )
      CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(SEGNO),    OK )

!
!     Check the descriptor as well.  However, don't check the
!     segment addresses.
!
      CALL T_CRDESC ( 'SPK', SEGNO, BODY, TBEGS(SEGNO), TENDS(SEGNO), &
                      XDESCR(1,SEGNO)                                )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,SEGNO), DSCSIZ-1, &
                     0.D0,   OK                                    )




      CALL TCASE ( 'Look up data for body 2.  This should cause '// &
                   'an OLD FILES search.'                          )

      BODY  = 2
      SPKNO = 2
      SEGNO = 1

      CALL T_SSFS ( BODY, TBEGS(1)+0.5D0, HANDLE, DESCR, SEGID, &
                    FOUND                                             )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     In this case, the segment should be found.  Make sure we get
!     back the right handle and segment identifier.
!
      CALL CHCKSL ( 'FOUND',  FOUND,      .TRUE.,            OK )
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(SPKNO), 0, OK )
      CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(SEGNO),    OK )

!
!     Check the descriptor as well.  However, don't check the
!     segment addresses.
!
      CALL T_CRDESC ( 'SPK', SEGNO, BODY, TBEGS(SEGNO), TENDS(SEGNO), &
                       XDESCR(1,SEGNO)                              )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,SEGNO), DSCSIZ-1, &
                     0.D0,   OK                                    )



      CALL TCASE ( 'Create a third SPK containing data for body 3. ' // &
                   'Load this SPK, then look up a state covered '    // &
                   'by the new file. This should cause the segment ' // &
                   'list for body 1 to get dumped.'    )

      BODY   = 3
      SPKNO  = 3

      DO I = 1, NSEG(SPKNO)

         IDS(I) = BODY

         TBEGS(I) = SPKNO * SCALE + I - 1
         TENDS(I) = TBEGS(I)          + 1

         XSEGID(I) = 'File: # Segno: #'

         CALL REPMC ( XSEGID(I), '#', SPKS(SPKNO), XSEGID(I) )
         CALL REPMI ( XSEGID(I), '#', I,           XSEGID(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


      CALL T_CRDAF ( 'SPK',  SPKS(SPKNO),  NSEG(SPKNO), &
                      IDS,   TBEGS,        TENDS,       XSEGID )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_SLEF ( SPKS(SPKNO), HNDLES(SPKNO) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SEGNO = NSEG(SPKNO)

      CALL T_SSFS ( BODY, TBEGS(SEGNO)+0.5D0, HANDLE, DESCR, SEGID, &
                    FOUND                                             )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     In this case, the segment should be found.  Make sure we get
!     back the right handle and segment identifier.
!
      CALL CHCKSL ( 'FOUND',  FOUND,      .TRUE.,            OK )
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(SPKNO), 0, OK )
      CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(SEGNO),    OK )

!
!     Check the descriptor as well.  However, don't check the
!     segment addresses.
!
      CALL T_CRDESC ( 'SPK', SEGNO, BODY, TBEGS(SEGNO), TENDS(SEGNO), &
                      XDESCR(1,SEGNO)                               )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,SEGNO), DSCSIZ-1, &
                     0.D0,   OK                                    )





      CALL TCASE ( 'Create another SPK for body 1 and load it. '     // &
                   'The segment count in this file is such that all '// &
                   'other body lists must be dumped to make room. '  // &
                   'Then make a request that is satisfied by SPK 1. '// &
                   'The segment in SPK 1 cannot be added to the '    // &
                   'segment table.'                                  )


      BODY   = 1
      SPKNO  = 4

      DO I = 1, NSEG(SPKNO)

         IDS(I)   = BODY

         TBEGS(I) = SPKNO * SCALE + I - 1
         TENDS(I) = TBEGS(I)          + 1

         XSEGID(I) = 'File: # Segno: #'

         CALL REPMC ( XSEGID(I), '#', SPKS(SPKNO), XSEGID(I) )
         CALL REPMI ( XSEGID(I), '#', I,           XSEGID(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL T_CRDAF ( 'SPK',  SPKS(SPKNO),  NSEG(SPKNO), &
                      IDS,   TBEGS,        TENDS,       XSEGID )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_SLEF ( SPKS(SPKNO), HNDLES(SPKNO) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SPKNO = 1
      SEGNO = 1

      TBEGS(SEGNO) = SPKNO * SCALE + SEGNO - 1
      TENDS(SEGNO) = SPKNO * SCALE + SEGNO

      CALL T_SSFS ( BODY, TBEGS(SEGNO)+0.5D0, HANDLE, DESCR, SEGID, &
                    FOUND                                             )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     In this case, the segment should be found.  Make sure we get
!     back the right handle and segment identifier.
!

      XSEGID(SEGNO) = 'File: # Segno: #'
      CALL REPMC ( XSEGID(SEGNO), '#', SPKS(SPKNO), XSEGID(SEGNO) )
      CALL REPMI ( XSEGID(SEGNO), '#', 1,           XSEGID(SEGNO) )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND',  FOUND,      .TRUE.,            OK )
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(SPKNO), 0, OK )
      CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(SEGNO),    OK )

!
!     Check the descriptor as well.  However, don't check the
!     segment addresses.
!
      CALL T_CRDESC ( 'SPK', SEGNO, BODY, TBEGS(SEGNO), TENDS(SEGNO), &
                       XDESCR(1,SEGNO)                               )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,SEGNO), DSCSIZ-1, &
                     0.D0,   OK                                    )





      CALL TCASE ( 'Start a segment list for body 1 by making a '     // &
                   'request that is satisfied by SPK 1.  Then build ' // &
                   'a file (SPK 5) with too many segments for body 1 '// &
                   'to be buffered.  Make a request that is '         // &
                   'satisfied by SPK 5. This tests the logic for '    // &
                   'searching the subset of a segment list that '     // &
                   'must be dumped due to lack of room.'           )



!
!     Set up by making a request that will be satisfied by the segment
!     in SPK 1.  This builds up the segment list for body 1.
!
      BODY     = 1
      TBEGS(1) = SCALE + 0.D0
      TENDS(1) = SCALE + 1.D0
      SPKNO    = 1

      XSEGID(1) = 'File: # Segno: #'
      CALL REPMC ( XSEGID(1), '#', SPKS(1), XSEGID(1) )
      CALL REPMI ( XSEGID(1), '#', 1,       XSEGID(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_SSFS ( BODY, TBEGS(1)+0.5D0, HANDLE, DESCR, SEGID, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Go ahead and make the new file.
!
      BODY   = 1
      SPKNO  = 5

      DO I = 1, NSEG(SPKNO)

         IDS(I)   = BODY

         IF ( ( I .EQ. 10 ) .OR. ( I .EQ. STSIZE+1 ) ) THEN
!
!           We want the lower bound of the re-use interval to
!           match the right endpoint of the segment's coverage
!           interval.
!
            TBEGS(I-1) = SPKNO * SCALE + I
            TENDS(I-1) = TBEGS(I-1)        + 1.0D0

            TBEGS(I)   = SPKNO * SCALE + I - 1
            TENDS(I)   = TBEGS(I)          + 1

            TBEGS(I+1) = TBEGS(I)
            TENDS(I+1) = TENDS(I)

            TBEGS(I+2) = TENDS(I)          + 1
            TENDS(I+2) = TBEGS(I+2)        + 1


         ELSE IF ( I .EQ. STSIZE+6 ) THEN
!
!           Create a singleton segment.
!
            TBEGS(I)   =  SPKNO * SCALE + I - 1
            TENDS(I)   =  TBEGS(I)

         ELSE IF ( I .EQ. STSIZE+7 ) THEN
!
!           Create an invisible segment.
!
            TBEGS(I)   =  SPKNO * SCALE + I - 1
            TENDS(I)   =  TBEGS(I) - 1

         ELSE IF (     (   I .LT. 9    ) &
                  .OR. ( ( I .GT. 12   ) .AND. ( I .LT. STSIZE ) ) &
                  .OR. (   I .GT. STSIZE+3 )                      ) THEN

            TBEGS(I) = SPKNO * SCALE + I - 1
            TENDS(I) = TBEGS(I)          + 1

         END IF

         XSEGID(I) = 'File: # Segno: #'

         CALL REPMC ( XSEGID(I), '#', SPKS(SPKNO), XSEGID(I) )
         CALL REPMI ( XSEGID(I), '#', I,           XSEGID(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL T_CRDAF ( 'SPK',  SPKS(SPKNO),  NSEG(SPKNO), &
                      IDS,   TBEGS,        TENDS,       XSEGID )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_SLEF ( SPKS(SPKNO), HNDLES(SPKNO) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SEGNO = 1

      CALL T_SSFS ( BODY, TBEGS(SEGNO)+0.5D0, HANDLE, DESCR, SEGID, &
                    FOUND                                             )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     In this case, the segment should be found.  Make sure we get
!     back the right handle and segment identifier.
!

      XSEGID(SEGNO) = 'File: # Segno: #'
      CALL REPMC ( XSEGID(SEGNO), '#', SPKS(SPKNO), XSEGID(SEGNO) )
      CALL REPMI ( XSEGID(SEGNO), '#', 1,           XSEGID(SEGNO) )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND',  FOUND,      .TRUE.,            OK )
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(SPKNO), 0, OK )
      CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(SEGNO),    OK )

!
!     Check the descriptor as well.  However, don't check the
!     segment addresses.
!
      CALL T_CRDESC ( 'SPK', SEGNO, BODY, TBEGS(SEGNO), TENDS(SEGNO), &
                       XDESCR(1,SEGNO)                               )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,SEGNO), DSCSIZ-1, &
                     0.D0,   OK                                    )






      CALL TCASE ( 'Create an SPK containing data for BTSIZE new '// &
                   'bodies. Look up data for each.'               )


!
!     Unload all SPKs.
!
      DO I = 1, NSPK
         CALL  T_SUEF( HNDLES(I) )
      END DO

      SPKNO  = 6

      DO I = 1, NSEG(SPKNO)

         IDS(I)   = BTSIZE+I

         TBEGS(I) = SPKNO * SCALE + I - 1
         TENDS(I) = TBEGS(I)          + 1

         XSEGID(I) = 'File: # Segno: #'

         CALL REPMC ( XSEGID(I), '#', SPKS(SPKNO), XSEGID(I) )
         CALL REPMI ( XSEGID(I), '#', I,           XSEGID(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL T_CRDAF ( 'SPK',  SPKS(SPKNO),  NSEG(SPKNO), &
                      IDS,   TBEGS,        TENDS,       XSEGID )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_SLEF ( SPKS(SPKNO), HNDLES(SPKNO) )
      CALL CHCKXC ( .FALSE., ' ', OK )


      DO I = 1, NSEG(SPKNO)

         BODY  = IDS(I)
         SEGNO = I

         CALL T_SSFS ( BODY, TBEGS(SEGNO)+0.5D0, HANDLE, DESCR, SEGID, &
                       FOUND                                          )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        In this case, the segment should be found.  Make sure we get
!        back the right handle and segment identifier.
!
         XSEGID(SEGNO) = 'File: # Segno: #'
         CALL REPMC ( XSEGID(SEGNO), '#', SPKS(SPKNO), XSEGID(SEGNO) )
         CALL REPMI ( XSEGID(SEGNO), '#', SEGNO,       XSEGID(SEGNO) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSL ( 'FOUND',  FOUND,      .TRUE.,            OK )
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(SPKNO), 0, OK )
         CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(SEGNO),    OK )

!
!        Check the descriptor as well.  However, don't check the
!        segment addresses.
!
         CALL T_CRDESC ( 'SPK', SEGNO, BODY, TBEGS(SEGNO), TENDS(SEGNO), &
                          XDESCR(1,SEGNO)                              )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,SEGNO), DSCSIZ-1, &
                        0.D0,   OK                                    )

      END DO







      CALL TCASE ( 'The body table should be full now; the '          // &
                   'segment table should have room.  Cause a body '   // &
                   'list to be dumped to make room in the body table.' )


!
!     Create a list for body 1 more expensive than those for the
!     bodies in SPK 6.  Body 1's list will be placed at the head of
!     the body table.
!
      BODY     = 1
      SPKNO    = 2
      SEGNO    = NSEG(SPKNO)
      I        = SEGNO
      TBEGS(I) = SPKNO * SCALE + I - 1
      TENDS(I) = TBEGS(I)          + 1

      XSEGID(1) = 'File: # Segno: #'
      CALL REPMC ( XSEGID(1), '#', SPKS(SPKNO), XSEGID(1) )
      CALL REPMI ( XSEGID(1), '#', SEGNO,       XSEGID(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL T_SLEF ( SPKS(2), HNDLES(2) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_SSFS ( BODY, TBEGS(I)+0.5D0, HANDLE, DESCR, SEGID, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     In this case, the segment should be found.  Make sure we get
!     back the right handle and segment identifier.
!
      CALL CHCKSL ( 'FOUND',  FOUND,      .TRUE.,            OK )
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(SPKNO), 0, OK )
      CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(1),        OK )

!
!     Check the descriptor as well.  However, don't check the
!     segment addresses.
!
      CALL T_CRDESC( 'SPK', SEGNO, BODY, TBEGS(I), TENDS(I), &
                     XDESCR(1,1) )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,1), DSCSIZ-1, &
                     0.D0,   OK                                 )

!
!     Now do a look up for body 2.  This will require dumping lists
!     from SPK 6.
!
      BODY     = 2
      SPKNO    = 2
      SEGNO    = 1
      I        = SEGNO
      TBEGS(I) = SPKNO * SCALE + I - 1
      TENDS(I) = TBEGS(I)          + 1



      XSEGID(SEGNO) = 'File: # Segno: #'
      CALL REPMC ( XSEGID(SEGNO), '#', SPKS(SPKNO), XSEGID(SEGNO) )
      CALL REPMI ( XSEGID(SEGNO), '#', SEGNO,       XSEGID(SEGNO) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_SSFS ( BODY, TBEGS(I)+0.5D0, HANDLE, DESCR, SEGID, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     In this case, the segment should be found.  Make sure we get
!     back the right handle and segment identifier.
!
      CALL CHCKSL ( 'FOUND',  FOUND,      .TRUE.,            OK )
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(SPKNO), 0, OK )
      CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(SEGNO),    OK )

!
!     Check the descriptor as well.  However, don't check the
!     segment addresses.
!
      CALL T_CRDESC ( 'SPK', 1, BODY, TBEGS(1), TENDS(1), XDESCR(1,1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,1), DSCSIZ-1, &
                     0.D0,   OK )






      CALL TCASE ( 'Look up data from a representative subset ' // &
                   'of the segments in SPK 5.'                     )

      SPKNO = 5

      CALL T_SLEF ( SPKS(SPKNO), HNDLES(SPKNO) )
      CALL CHCKXC ( .FALSE., ' ', OK )


      DO I = 1, NSEG(SPKNO)

         IDS(I)   = BODY

         IF ( ( I .EQ. 10 ) .OR. ( I .EQ. STSIZE+1 ) ) THEN
!
!           We want the lower bound of the re-use interval to
!           match the right endpoint of the segment's coverage
!           interval.
!
            TBEGS(I-1) = SPKNO * SCALE + I
            TENDS(I-1) = TBEGS(I-1)        + 1.0D0

            TBEGS(I)   = SPKNO * SCALE + I - 1
            TENDS(I)   = TBEGS(I)          + 1

            TBEGS(I+1) = TBEGS(I)
            TENDS(I+1) = TENDS(I)

            TBEGS(I+2) = TENDS(I)          + 1
            TENDS(I+2) = TBEGS(I+2)        + 1

         ELSE IF ( I .EQ. STSIZE+6 ) THEN
!
!           Create a singleton segment.
!
            TBEGS(I)   =  SPKNO * SCALE + I - 1
            TENDS(I)   =  TBEGS(I)

         ELSE IF ( I .EQ. STSIZE+7 ) THEN
!
!           Create an invisible segment.
!
            TBEGS(I)   =  SPKNO * SCALE + I - 1
            TENDS(I)   =  TBEGS(I) - 1

         ELSE IF (     ( I .LT. 10   ) &
                  .OR. ( ( I .GT. 12 ) .AND. ( I .LT. STSIZE ) ) &
                  .OR. ( I .GT. STSIZE+3 )                        ) THEN

            TBEGS(I) = SPKNO * SCALE + I - 1
            TENDS(I) = TBEGS(I)          + 1

         END IF

         XSEGID(I) = 'File: # Segno: #'

         CALL REPMC ( XSEGID(I), '#', SPKS(SPKNO), XSEGID(I) )
         CALL REPMI ( XSEGID(I), '#', I,           XSEGID(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


      I = 1

      DO WHILE ( I .LE. NSEG(SPKNO) )

         BODY  = 1
         SEGNO = I

         CALL T_SSFS ( BODY, TBEGS(SEGNO)+0.5D0, HANDLE, DESCR, SEGID, &
                       FOUND                                          )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        In this case, the segment should be found.  Make sure we get
!        back the right handle and segment identifier.
!
         XSEGID(SEGNO) = 'File: # Segno: #'
         CALL REPMC ( XSEGID(SEGNO), '#', SPKS(SPKNO), XSEGID(SEGNO) )
         CALL REPMI ( XSEGID(SEGNO), '#', SEGNO,       XSEGID(SEGNO) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSL ( 'FOUND',  FOUND,      .TRUE.,            OK )
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(SPKNO), 0, OK )
         CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(SEGNO),    OK )

!
!        Check the descriptor as well.  However, don't check the
!        segment addresses.
!
         CALL T_CRDESC ( 'SPK', SEGNO, BODY, TBEGS(SEGNO), TENDS(SEGNO), &
                         XDESCR(1,SEGNO)                               )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,SEGNO), DSCSIZ-1, &
                        0.D0,   OK                                   )

!
!        Skip some tests that are unlikely to reveal bugs, as well as
!        those which would give anomalous results due to the structure
!        of SPK 5.
!
         IF ( I .EQ. 3 ) THEN
            I = (STSIZE/2)-2
         ELSE IF ( I .EQ. (STSIZE/2)+3 ) THEN
            I = STSIZE-2
         ELSE IF ( I .EQ. STSIZE ) THEN
            I = STSIZE+5
         ELSE IF ( I .EQ. STSIZE+5 ) THEN
            I = STSIZE+8
         ELSE
            I = I + 1
         END IF

      END DO


!
!     Try a search w/o buffering case where no segment is found.
!
      CALL TCASE ( 'Search w/o buffering, no segment should be found.' )
      SPKNO = 5
      BODY  = 1
      T     = 2 * TENDS( NSEG(SPKNO) )

      CALL T_SSFS ( BODY, T, HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND',  FOUND, .FALSE., OK )


!
!     Return on entry in RETURN mode, if the error status is set.
!
      CALL TCASE ( 'Make sure T_SSFS returns on entry when RETURN()'// &
                   'is .TRUE.'                                       )

      SMSG = 'Return on entry'

      CALL SIGERR ( SMSG )

      CALL T_SSFS ( 1, 0.D0, HANDLE, DESCR, SEGID, FOUND )

!
!     Depending on whether we're calling a version of T_SBSR that does
!     coverage checking, the error status may be reset.
!
      IF ( RETURN() ) THEN
         CALL CHCKXC ( .TRUE., SMSG, OK )
      ELSE
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF


!
!     Unload the SPK files.
!
      DO I = 1, NSPK
         CALL T_SUEF ( HNDLES(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END DO

!
!     Make sure an error is signaled if no SPKs are loaded.
!
      CALL TCASE ( 'Make sure an error is signaled if no SPKs '// &
                   'are loaded.'                               )
      CALL T_SSFS ( 1, 0.D0, HANDLE, DESCR, SEGID, FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(NOLOADEDFILES)', OK )


!
!     Load SPK1 and look up a state from it to create a cheap list.
!     Make the cheap list the second list by looking up data from
!     it after looking up data for body BTSIZE+1.
!
      CALL TCASE ( 'Test removal of cheap list when adding a new ' // &
                   'body; cheap list is 2nd.'                       )

      CALL T_SLEF ( SPKS(1), HNDLES(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Now load the SPK containing 100 bodies.  Look up data for
!     each one.  The last one will cause the list for body 1 to
!     be dumped.
!
      SPKNO = 6
      CALL T_SLEF ( SPKS(SPKNO), HNDLES(SPKNO) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, NSEG(SPKNO)

         IDS(I)   = BTSIZE+I

         TBEGS(I) = SPKNO * SCALE + I - 1
         TENDS(I) = TBEGS(I)          + 1

         XSEGID(I) = 'File: # Segno: #'

         CALL REPMC ( XSEGID(I), '#', SPKS(SPKNO), XSEGID(I) )
         CALL REPMI ( XSEGID(I), '#', I,           XSEGID(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


      DO I = 1, NSEG(SPKNO)

         BODY  = IDS(I)
         SEGNO = I

         CALL T_SSFS ( BODY, TBEGS(SEGNO)+0.5D0, HANDLE, DESCR, SEGID, &
                       FOUND                                          )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        In this case, the segment should be found.  Make sure we get
!        back the right handle and segment identifier.
!
         XSEGID(SEGNO) = 'File: # Segno: #'
         CALL REPMC ( XSEGID(SEGNO), '#', SPKS(SPKNO), XSEGID(SEGNO) )
         CALL REPMI ( XSEGID(SEGNO), '#', SEGNO,       XSEGID(SEGNO) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSL ( 'FOUND',  FOUND,      .TRUE.,            OK )
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(SPKNO), 0, OK )
         CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(SEGNO),    OK )

!
!        Check the descriptor as well.  However, don't check the
!        segment addresses.
!
         CALL T_CRDESC ( 'SPK', SEGNO, BODY, TBEGS(SEGNO), TENDS(SEGNO), &
                         XDESCR(1,SEGNO)                               )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,SEGNO), DSCSIZ-1, &
                        0.D0,   OK                                    )

         IF ( I .EQ. 1 ) THEN
!
!           Create a cheap list for body 1.
!
           CALL T_SLEF ( SPKS(SPKNO), HNDLES(SPKNO) )
           CALL CHCKXC ( .FALSE., ' ', OK )
           TBEGS(1) = SCALE
           CALL T_SSFS ( 1, TBEGS(1)+0.5D0, HANDLE, DESCR, SEGID, FOUND)
           CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

      END DO



      CALL TCASE ( 'Test ability to make room by deleting a body ' // &
                   'table entry with an empty list.'                )


!
!     Create an example of the list in question by forcing a search
!     without buffering on body 1, where the highest priority file
!     contains too many segments to buffer.  However, we want this
!     list to have a high expense, so load an SPK with many segments
!     for this body and search it first.
!
      SPKNO = 5
      CALL T_SLEF ( SPKS(SPKNO), HNDLES(SPKNO) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      BODY = 1
      T    = SCALE * SPKNO + (STSIZE+1) - 1 + 0.5D0

      CALL T_SSFS ( 1, T, HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )


!
!     Now look up data for the first NSEG-1 bodies in SPK 6.  This
!     should fill up the body table.
!
      SPKNO = 6

      DO I = 1, NSEG(SPKNO)-1

         IDS(I)   = BTSIZE+I

         TBEGS(I) = SPKNO * SCALE + I - 1
         TENDS(I) = TBEGS(I)          + 1

         BODY     = IDS(I)
         SEGNO   = I

         CALL T_SSFS ( BODY, TBEGS(SEGNO)+0.5D0, HANDLE, DESCR, SEGID, &
                       FOUND                                          )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        In this case, the segment should be found.  Make sure we get
!        back the right handle and segment identifier.
!
         XSEGID(SEGNO) = 'File: # Segno: #'
         CALL REPMC ( XSEGID(SEGNO), '#', SPKS(SPKNO), XSEGID(SEGNO) )
         CALL REPMI ( XSEGID(SEGNO), '#', SEGNO,       XSEGID(SEGNO) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSL ( 'FOUND',  FOUND,      .TRUE.,            OK )
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(SPKNO), 0, OK )
         CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(SEGNO),    OK )

!
!        Check the descriptor as well.  However, don't check the
!        segment addresses.
!
         CALL T_CRDESC ( 'SPK', SEGNO, BODY, TBEGS(SEGNO), TENDS(SEGNO), &
                         XDESCR(1,SEGNO)                               )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,SEGNO), DSCSIZ-1, &
                        0.D0,   OK                                    )

      END DO


!
!     Try some cases where the re-use interval matches the selected
!     segment's coverage interval.
!
      CALL TCASE ( 'Search w/o buffering case, selected segment is ' // &
                   'in dumped list, coverage interval matches '      // &
                   're-use interval, request time is in center of '  // &
                   're-use interval.'                                )

!
!     Set up the case by unloading the currently loaded SPKs.  Load
!     SPK 1 and look up a state from it.
!

!
!     Unload the SPK files.
!
      DO I = 1, NSPK-1
         CALL T_SUEF ( HNDLES(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END DO

!
!     Load SPK 1 and look up a state from this file.
!
      CALL T_SLEF ( SPKS(1), HNDLES(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      BODY     = 1
      TBEGS(1) = SCALE + 0.D0
      TENDS(1) = SCALE + 1.D0
      SPKNO    = 1

      XSEGID(1) = 'File: # Segno: #'
      CALL REPMC ( XSEGID(1), '#', SPKS(1), XSEGID(1) )
      CALL REPMI ( XSEGID(1), '#', 1,       XSEGID(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_SSFS ( BODY, TBEGS(1)+0.5D0, HANDLE, DESCR, SEGID, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Now load SPK 5.  Look up a state from segment 9, where the
!     request time is to the right of a segment whose right endpoint
!     is at the left endpoint of the re-use interval.
!
      CALL T_SLEF ( SPKS(5), HNDLES(5) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SPKNO  =  5
      BODY   =  1
      SEGNO  =  9


      TBEGS(SEGNO) = SPKNO * SCALE + SEGNO + 1
      TENDS(SEGNO) = TBEGS(SEGNO)  + 1

      T            = TBEGS(SEGNO) + 0.25D0

      CALL T_SSFS ( BODY, T, HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     In this case, segment 9 should match.
!
      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )

      SEGNO = 9

      XSEGID(1) = 'File: # Segno: #'
      CALL REPMC ( XSEGID(1), '#', SPKS(5), XSEGID(1) )
      CALL REPMI ( XSEGID(1), '#', SEGNO,   XSEGID(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(SPKNO), 0, OK )
      CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(1),        OK )

!
!     Check the descriptor as well.  However, don't check the
!     segment addresses.
!
      CALL T_CRDESC ( 'SPK', SEGNO, BODY, TBEGS(SEGNO), TENDS(SEGNO), &
                      XDESCR(1,SEGNO)                                )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,SEGNO), DSCSIZ-1, &
                     0.D0,   OK                                    )



!
!     Create a situation where the segment list for body 1 contributed
!     by SPK 5 gets dumped, and where the request is satisfied by
!     a segment in SPK 1.
!
      CALL TCASE ( 'Dump segment list from SPK 5; find segment '// &
                   'for body 1 in SPK 1.'                        )

      CALL T_SLEF ( SPKS(1), HNDLES(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_SLEF ( SPKS(5), HNDLES(5) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      BODY     = 1
      TBEGS(1) = SCALE
      TENDS(1) = SCALE + 1.D0
      T        =  0.5D0 * (  TBEGS(1) + TENDS(1)  )

      CALL T_SSFS ( BODY, T, HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )

!
!     Check handle, segment descriptor and ID.
!
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(1), 0, OK )

      CALL T_CRDESC ( 'SPK', 1, BODY, TBEGS(1), TENDS(1), XDESCR(1,1) )
      CALL T_CHDS   ( 'DESCR', DESCR, '=', XDESCR(1,1), DSCSIZ-1, &
                       0.D0,   OK                                     )


      XSEGID(1) = 'File: # Segno: #'
      CALL REPMC ( XSEGID(1), '#', SPKS(1), XSEGID(1) )
      CALL REPMI ( XSEGID(1), '#', 1,       XSEGID(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(1), OK )



      CALL TCASE ( 'Dump segment list from SPK 5.  While searching '// &
                   'list for segment for body 1, make lower bound ' // &
                   'of re-use interval match lower bound of '       // &
                   'segment descriptor.'                              )


!
!     Make SPK 1 higher priority than SPK 5.
!
      CALL T_SLEF ( SPKS(1), HNDLES(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Place request time in the "hole" between segments STSIZE+1 and
!     STSIZE+3.
!
      I          = STSIZE+1

      TBEGS(I-1) = SPKNO * SCALE + I
      TENDS(I-1) = TBEGS(I-1)        + 1.0D0

      TBEGS(I)   = SPKNO * SCALE + I - 1
      TENDS(I)   = TBEGS(I)          + 1

      TBEGS(I+1) = TBEGS(I)
      TENDS(I+1) = TENDS(I)

      TBEGS(I+2) = TENDS(I)          + 1
      TENDS(I+2) = TBEGS(I+2)        + 1

      T          = TBEGS(I-1) + 0.5D0


      CALL T_SSFS ( BODY, T, HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     In this case, segment STSIZE should match.
!
      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )

      SEGNO = STSIZE

      XSEGID(1) = 'File: # Segno: #'
      CALL REPMC ( XSEGID(1), '#', SPKS(5), XSEGID(1) )
      CALL REPMI ( XSEGID(1), '#', SEGNO,   XSEGID(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(SPKNO), 0, OK )
      CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(1),        OK )

!
!     Check the descriptor as well.  However, don't check the
!     segment addresses.
!
      I          = SEGNO + 1
      TBEGS(I-1) = SPKNO * SCALE + I
      TENDS(I-1) = TBEGS(I-1)        + 1.0D0

      CALL T_CRDESC ( 'SPK', SEGNO, BODY, TBEGS(SEGNO), TENDS(SEGNO), &
                      XDESCR(1,SEGNO)                                )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,SEGNO), DSCSIZ-1, &
                     0.D0,   OK                                     )




!
!     Check correct handling of re-use intervals.  Create a new
!     SPK file that contains coverage that exemplifies the various
!     masking possibilities that may occur.
!
      CALL TCASE ( 'Check re-use for a 1-body segment list.' )

      SPKNO = 7

!
!     Segment 1:
!
      BODY      = 1
      IDS(1)    = BODY

      TBEGS(1) = SPKNO * SCALE
      TENDS(1) = TBEGS(1) + 1.D0


!
!     Segments 2-3:
!
      BODY      = 2
      IDS(2)    = BODY
      IDS(3)    = BODY


      TBEGS(3) = SPKNO * SCALE
      TENDS(3) = TBEGS(3) + 1.D0

      TBEGS(2) = TENDS(3) + 1.D0
      TENDS(2) = TBEGS(2) + 1.D0


!
!     Segments 4-6:
!
      BODY      = 3
      IDS(4)    = BODY
      IDS(5)    = BODY
      IDS(6)    = BODY

      TBEGS(6) = SPKNO * SCALE
      TENDS(6) = TBEGS(6) + 3.D0

      TBEGS(5) = TENDS(6) - 1.D0
      TENDS(5) = TBEGS(5) + 3.D0

      TBEGS(4) = TBEGS(5) + 1.D0
      TENDS(4) = TENDS(5) - 1.D0


!
!     Segments 7-9:
!
      BODY     = 4
      IDS(7)   = BODY
      IDS(8)   = BODY
      IDS(9)   = BODY

      TBEGS(9) = SPKNO * SCALE
      TENDS(9) = TBEGS(9) + 3.D0

      TBEGS(8) = TBEGS(9)
      TENDS(8) = TENDS(9)

      TBEGS(7) = TBEGS(9) - 2.D0
      TENDS(7) = TBEGS(9) + 3.D0

!
!     Segments 10-12:
!
      BODY      = 5
      IDS(10)   = BODY
      IDS(11)   = BODY
      IDS(12)   = BODY

      TBEGS(12) = SPKNO * SCALE
      TENDS(12) = TBEGS(12) + 3.D0

      TBEGS(11) = TBEGS(12) - 2.D0
      TENDS(11) = TBEGS(11) + 3.D0

      TBEGS(10) = TBEGS(11) - 2.D0
      TENDS(10) = TENDS(12) + 1.D0

!
!     Segments 13-14:
!
      BODY      = 6
      IDS(13)   = BODY
      IDS(14)   = BODY

!
!     Singleton segment:
!
      TBEGS(13) = SPKNO * SCALE
      TENDS(13) = TBEGS(13)

!
!     Invisible segment:
!
      TBEGS(14) = TENDS(13) + 3.D0
      TENDS(14) = TBEGS(14) - 1.D0

!
!     Three more segments for body 4:
!
      IDS(15)   =  4
      IDS(16)   =  4
      IDS(17)   =  4

      TBEGS(15) =  SPKNO * SCALE + 10.D0
      TENDS(15) =  TBEGS(15)     +  3.D0

      TBEGS(16) =  TBEGS(15) + 1.D0
      TENDS(16) =  TENDS(15) - 1.D0

      TBEGS(17) =  TBEGS(16)
      TENDS(17) =  TENDS(16)

!
!     Three more segments for body 5:
!
      BODY      = 5
      IDS(18)   =  BODY
      IDS(19)   =  BODY
      IDS(20)   =  BODY

      TBEGS(20) =  SPKNO * SCALE + 10.D0
      TENDS(20) =  TBEGS(20)     +  3.D0

      TBEGS(19) = TBEGS(20) - 2.D0
      TENDS(19) = TBEGS(19) + 3.D0

      TBEGS(18) = TBEGS(19) - 2.D0
      TENDS(18) = TENDS(20) + 1.D0


!
!     Create a segment for body 6 with the following topology:
!
!
!              +++++++           segment 21
!                    +++++++             22
!        +++++++                         23
!
!
      BODY      =  6
      IDS(21)   =  BODY
      IDS(22)   =  BODY
      IDS(23)   =  BODY

      TBEGS(21) =  SPKNO * SCALE + 10.D0
      TENDS(21) =  TBEGS(21)     +  3.D0

      TBEGS(22) =  TENDS(21)
      TENDS(22) =  TBEGS(21) + 3.D0

      TBEGS(23) =  TBEGS(21) - 3.D0
      TENDS(23) =  TBEGS(21)


!
!     Create the eighth SPK, which is just a copy of the 7th, except
!     for descriptors and segment IDs.
!
      SPKNO = 8

      DO SEGNO = 1, NSEG(SPKNO)

         CALL T_CRDESC ( 'SPK',         SEGNO,        IDS(SEGNO), &
                          TBEGS(SEGNO), TENDS(SEGNO), XDESCR(1,SEGNO) )

         XSEGID(SEGNO) = 'File: # Segno: #'
         CALL REPMC ( XSEGID(SEGNO), '#', SPKS(SPKNO), XSEGID(SEGNO) )
         CALL REPMI ( XSEGID(SEGNO), '#', SEGNO,       XSEGID(SEGNO) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL T_CRDAF ( 'SPK',  SPKS(SPKNO),  NSEG(SPKNO), &
                      IDS,   TBEGS,        TENDS,   XSEGID )



!
!     Create the segment descriptors and segment identifiers for
!     this SPK file.
!
      SPKNO = 7

      DO SEGNO = 1, NSEG(SPKNO)

         CALL T_CRDESC ( 'SPK',        SEGNO,        IDS(SEGNO), &
                         TBEGS(SEGNO), TENDS(SEGNO), XDESCR(1,SEGNO) )

         XSEGID(SEGNO) = 'File: # Segno: #'
         CALL REPMC ( XSEGID(SEGNO), '#', SPKS(SPKNO), XSEGID(SEGNO) )
         CALL REPMI ( XSEGID(SEGNO), '#', SEGNO,       XSEGID(SEGNO) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     Unload the other SPK files.  Create and load the SPK file.
!
!
!     Unload the SPK files.  Again.
!
      DO I = 1, SPKNO-1
         CALL T_SUEF ( HNDLES(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END DO

      CALL T_CRDAF ( 'SPK',  SPKS(SPKNO),  NSEG(SPKNO), &
                      IDS,   TBEGS,        TENDS,      XSEGID )



!      CALL BYEBYE ( 'SUCCESS' )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_SLEF ( SPKS(SPKNO), HNDLES(SPKNO) )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Time for tests.
!
!
!     Make sure we can re-use data from the first segment for body 1.
!
      SPKNO = 7
      BODY  = IDS(1)
      T     = 0.5D0 * (  TBEGS(1) + TENDS(1)  )

      DO I = 1, 3

         CALL T_SSFS ( BODY, T, HANDLE, DESCR, SEGID, FOUND )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )

!
!        Check handle, segment descriptor and ID.
!
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(SPKNO), 0, OK )

         CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,1), DSCSIZ-1, &
                        0.D0,   OK                                )

         CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(1), OK )

      END DO


      T = TBEGS(1) - 1.D0

      CALL T_SSFS ( BODY, T, HANDLE, DESCR, SEGID, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND',  FOUND, .FALSE., OK )


      T = TENDS(1) + 1.D0

      CALL T_SSFS ( BODY, T, HANDLE, DESCR, SEGID, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND',  FOUND, .FALSE., OK )


      T = TBEGS(1)

      CALL T_SSFS ( BODY, T, HANDLE, DESCR, SEGID, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )


      T = TENDS(1)

      CALL T_SSFS ( BODY, T, HANDLE, DESCR, SEGID, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )

!
!     Check out behavior for coverage consisting of two non-overlapping
!     segments.  The coverage topology is as follows:
!
!
!                      ++++++++++    segment 2
!        +++++++++++                         3
!
!
!
      CALL TCASE ( 'Coverage is union of two disjoint intervals. '// &
                   'Test re-use of each.'                           )



      BODY  = IDS(2)
      T     = 0.5D0 * (  TBEGS(2) + TENDS(2)  )

      DO I = 1, 3

         CALL T_SSFS ( BODY, T, HANDLE, DESCR, SEGID, FOUND )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )

!
!        Check handle, segment descriptor and ID.
!
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(SPKNO), 0, OK )

         CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,2), DSCSIZ-1, &
                        0.D0,   OK                                )

         CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(2), OK )

      END DO


      T     = 0.5D0 * (  TBEGS(3) + TENDS(3)  )

      DO I = 1, 3

         CALL T_SSFS ( BODY, T, HANDLE, DESCR, SEGID, FOUND )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )

!
!        Check handle, segment descriptor and ID.
!
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(SPKNO), 0, OK )

         CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,3), DSCSIZ-1, &
                        0.D0,   OK                                )

         CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(3), OK )

      END DO

!
!     Hit the endpoints of the left interval.
!
      T  = TBEGS(3)

      CALL T_SSFS ( BODY, T, HANDLE, DESCR, SEGID, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )

!
!     Check handle, segment descriptor and ID.
!
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(SPKNO), 0, OK )

      CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,3), DSCSIZ-1, &
                     0.D0,   OK                                )

      CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(3), OK )


      T  = TENDS(3)

      CALL T_SSFS ( BODY, T, HANDLE, DESCR, SEGID, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )

!
!     Check handle, segment descriptor and ID.
!
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(SPKNO), 0, OK )

      CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,3), DSCSIZ-1, &
                     0.D0,   OK                                )

      CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(3), OK )




!
!     Segments 4-6:
!
!
!     Check out behavior for coverage consisting of three segments
!     whose coverage is as shown:
!
!
!                 +++++++          segment 4
!              +++++++++++++               5
!        +++++++++++                       6
!
!
      CALL TCASE ( 'Segments 4-6:  three-segment overlapping case #1.' )



      BODY  = IDS(5)
      T     = TENDS(6) + 0.25D0

      DO I = 1, 3

         CALL T_SSFS ( BODY, T, HANDLE, DESCR, SEGID, FOUND )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )

!
!        Check handle, segment descriptor and ID.
!
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(SPKNO), 0, OK )

         CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,5), DSCSIZ-1, &
                        0.D0,   OK                                )

         CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(5), OK )

      END DO


      BODY  = IDS(4)
      T     = TBEGS(6) + 0.25D0

      DO I = 1, 3

         CALL T_SSFS ( BODY, T, HANDLE, DESCR, SEGID, FOUND )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )

!
!        Check handle, segment descriptor and ID.
!
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(SPKNO), 0, OK )

         CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,6), DSCSIZ-1, &
                        0.D0,   OK                                )

         CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(6), OK )

      END DO




      T = TBEGS(5) + 0.25D0

      CALL T_SSFS ( BODY, T, HANDLE, DESCR, SEGID, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )


      T = TBEGS(6) - 0.25D0

      CALL T_SSFS ( BODY, T, HANDLE, DESCR, SEGID, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND',  FOUND, .FALSE., OK )








!
!     Segments 7-9:
!
!
!     Check out behavior for coverage consisting of three segments
!     whose coverage is as shown:
!
!        +++++++++++           segment 7
!             +++++++++++              8
!             +++++++++++              9
!
      CALL TCASE ( 'Segments 7-9:  three-segment overlapping case #2.' )

!
!     Get the right side of the re-use interval to coincide with
!     the left endpoint of a descriptor, where ET lies to the left
!     of the segment, in the CHECK LIST state:
!
      BODY  = IDS(7)
      T     = TBEGS(7) + 0.25D0

      CALL T_SSFS ( BODY, T, HANDLE, DESCR, SEGID, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )

      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(SPKNO), 0, OK )

      CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,7), DSCSIZ-1, &
                     0.D0,   OK                                )

      CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(7), OK )





!
!     Check out behavior for coverage consisting of three segments
!     whose coverage is as shown:
!
!
!       ++++++++++++++++++        segment 10
!           +++++++                       11
!               ++++++++                  12
!
!
      CALL TCASE ( 'Three-segment overlapping case #2.' )



      BODY  = IDS(10)
      T     = TENDS(12) + 0.25D0

      DO I = 1, 3

         CALL T_SSFS ( BODY, T, HANDLE, DESCR, SEGID, FOUND )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )

!
!        Check handle, segment descriptor and ID.
!
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(SPKNO), 0, OK )

         CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,10), DSCSIZ-1, &
                        0.D0,   OK                                 )

         CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(10), OK )

      END DO


      T = TENDS(10) + 1.D0

      CALL T_SSFS ( BODY, T, HANDLE, DESCR, SEGID, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND',  FOUND, .FALSE., OK )



      T = TBEGS(10) + 0.25D0

      DO I = 1, 3

         CALL T_SSFS ( BODY, T, HANDLE, DESCR, SEGID, FOUND )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )

!
!        Check handle, segment descriptor and ID.
!
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(SPKNO), 0, OK )

         CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,10), DSCSIZ-1, &
                        0.D0,   OK                                  )

         CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(10), OK )

      END DO


      T = TBEGS(11) - 0.25D0

      CALL T_SSFS ( BODY, T, HANDLE, DESCR, SEGID, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )

      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(SPKNO), 0, OK )

      CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,10), DSCSIZ-1, &
                     0.D0,   OK                                 )

      CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(10), OK )



!
!     Check out behavior for coverage consisting of three segments
!     whose coverage is as shown:
!
!
!       ++++++++++++++++++        segment 15
!            +++++++                      16
!            +++++++                      17
!
!
      CALL TCASE ( 'ET > segment upper bound.  Lower bound of '// &
                   're-use interval = upper bound of segment.'   )



      BODY  = IDS(15)
      T     = TENDS(17) + 0.5D0

      CALL T_SSFS ( BODY, T, HANDLE, DESCR, SEGID, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )

!
!     Check handle, segment descriptor and ID.
!
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(SPKNO), 0, OK )

      CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,15), DSCSIZ-1, &
                     0.D0,   OK                                  )

      CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(15), OK )




!
!     Check out behavior for coverage consisting of three segments
!     whose coverage is as shown:
!
!
!              +++++++           segment 21
!                    +++++++             22
!        +++++++                         23
!
!

      CALL TCASE ( 'ET is in segment.  Lower bound of '        // &
                   're-use interval = lower bound of segment.'   )

      BODY      =  6
      IDS(21)   =  BODY
      IDS(22)   =  BODY
      IDS(23)   =  BODY

      TBEGS(21) =  SPKNO * SCALE + 10.D0
      TENDS(21) =  TBEGS(21)     +  3.D0

      TBEGS(22) =  TENDS(21)
      TENDS(22) =  TBEGS(21) + 3.D0

      TBEGS(23) =  TBEGS(21) - 3.D0
      TENDS(23) =  TBEGS(21)



      BODY  = IDS(21)
      T     = TBEGS(21) + 0.5D0

      CALL T_SSFS ( BODY, T, HANDLE, DESCR, SEGID, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )

!
!     Check handle, segment descriptor and ID.
!
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(SPKNO), 0, OK )

      CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,21), DSCSIZ-1, &
                     0.D0,   OK                                 )

      CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(21), OK )






!
!     Check out behavior for coverage consisting singleton and
!     invisible segments.
!
!
      CALL TCASE ( 'Look up data from a singleton segment.' )

      T    = TBEGS(13)
      BODY = IDS(13)

      CALL T_SSFS ( BODY, T, HANDLE, DESCR, SEGID, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )

      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(SPKNO), 0, OK )

      CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,13), DSCSIZ-1, &
                     0.D0,   OK                                  )

      CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(13), OK )


!
!     Exercise the logic for handling singleton and invisible
!     segments during a NEW BODY search.
!
      CALL TCASE ( 'Look up data from a singleton segment, this time '// &
                   'in a NEW SEGMENTS search.'                       )

      SPKNO = 8

      CALL T_SLEF ( SPKS(SPKNO), HNDLES(SPKNO) )
      CALL CHCKXC ( .FALSE., ' ', OK )


      SEGNO = 13

      CALL T_CRDESC ( 'SPK',        SEGNO,        IDS(SEGNO), &
                      TBEGS(SEGNO), TENDS(SEGNO), XDESCR(1,SEGNO) )

      XSEGID(SEGNO) = 'File: # Segno: #'
      CALL REPMC ( XSEGID(SEGNO), '#', SPKS(SPKNO), XSEGID(SEGNO) )
      CALL REPMI ( XSEGID(SEGNO), '#', SEGNO,       XSEGID(SEGNO) )
      CALL CHCKXC ( .FALSE., ' ', OK )


      T    = TBEGS(13)
      BODY = IDS(13)

      CALL T_SSFS ( BODY, T, HANDLE, DESCR, SEGID, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )

      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(SPKNO), 0, OK )

      CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,13), DSCSIZ-1, &
                     0.D0,   OK                                 )

      CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(13), OK )





      CALL TCASE ( 'Prepare for search w/o buffering tests: '// &
                   'create an SPK with STSIZE segments for ' // &
                   'bodies 1-NBODY.'                                )

!
!     Create an SPK file with STSIZE segments for bodies 1-NBODY.
!
      SPKNO = 9

      DO BODY = 1, NBODY

         DO I = 1, STSIZE

            J         = (BODY-1)*STSIZE + I
            IDS(J)    = BODY

            TBEGS(J)  = SPKNO * SCALE + I - 1
            TENDS(J)  = TBEGS(J)          + 1

            XSEGID(J) = 'File: # Segno: #  Body:  #'

            CALL REPMC ( XSEGID(J), '#', SPKS(SPKNO), XSEGID(J) )
            CALL REPMI ( XSEGID(J), '#', J,           XSEGID(J) )
            CALL REPMI ( XSEGID(J), '#', BODY,        XSEGID(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END DO

      CALL T_CRDAF ( 'SPK',  SPKS(SPKNO),  NSEG(SPKNO), &
                      IDS,   TBEGS,        TENDS,       XSEGID )

      CALL CHCKXC ( .FALSE., ' ', OK )



      CALL TCASE ( 'Prepare for search w/o buffering tests: '// &
                   'create an SPK with STSIZE segments for ' // &
                   'bodies 1-NBODY.'                                )

!
!     Create an SPK file with STSIZE segments for bodies 1-NBODY.
!

      SPKNO = 10

      DO BODY = 1, NBODY

         DO I = 1, STSIZE-3

            J         = (BODY-1)*(STSIZE-3) + I
            IDS(J)    = BODY

            TBEGS(J)  = SPKNO * SCALE + I - 1
            TENDS(J)  = TBEGS(J)          + 1

            XSEGID(J) = 'File: # Segno: #  Body:  #'

            CALL REPMC ( XSEGID(J), '#', SPKS(SPKNO), XSEGID(J) )
            CALL REPMI ( XSEGID(J), '#', J,           XSEGID(J) )
            CALL REPMI ( XSEGID(J), '#', BODY,        XSEGID(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END DO

      CALL T_CRDAF ( 'SPK',  SPKS(SPKNO),  NSEG(SPKNO), &
                      IDS,   TBEGS,        TENDS,       XSEGID )

      CALL CHCKXC ( .FALSE., ' ', OK )




      CALL TCASE ( 'Search w/o buffering, ET < segment begin, ' // &
                   're-use interval right endpoint < segment '  // &
                   'begin.'                                     )


!
!     Unload the SPK files.  Again.
!
      DO I = 1, NSPK
         CALL T_SUEF ( HNDLES(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END DO

!
!     Load SPKs 7 and 9.
!
      CALL T_SLEF ( SPKS(7),  HNDLES(7)  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_SLEF ( SPKS(9), HNDLES(9) )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     The request time should precede the coverage of segment 3 in
!     SPK 7.
!
      BODY = 2
      T    = 7 * SCALE  - 1.D0

      CALL T_SSFS ( BODY, T, HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND',  FOUND, .FALSE., OK )



      CALL TCASE ( 'Search w/o buffering, ET within segment, ' // &
                   're-use interval, left endpoint > segment ' // &
                   'begin.'                                     )



!
!     The request time should precede the coverage of segment 3 in
!     SPK 7.
!
      BODY  = 3
      SEGNO = 5
      SPKNO = 7

      TBEGS(6) = SPKNO * SCALE
      TENDS(6) = TBEGS(6) + 3.D0

      TBEGS(5) = TENDS(6) - 1.D0
      TENDS(5) = TBEGS(5) + 3.D0

      T        = SPKNO * SCALE  + 4.D0

      CALL T_SSFS ( BODY, T, HANDLE, DESCR, SEGID, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )

      XSEGID(SEGNO) = 'File: # Segno: #'
      CALL REPMC ( XSEGID(SEGNO), '#', SPKS(SPKNO), XSEGID(SEGNO) )
      CALL REPMI ( XSEGID(SEGNO), '#', SEGNO,       XSEGID(SEGNO) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(SEGNO),    OK )

!
!     Check the descriptor as well.  However, don't check the
!     segment addresses.
!
      CALL T_CRDESC ( 'SPK',        SEGNO,        BODY, &
                      TBEGS(SEGNO), TENDS(SEGNO), XDESCR(1,SEGNO) )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,SEGNO), DSCSIZ-1, &
                     0.D0,   OK                                   )





      CALL TCASE ( 'Search w/o buffering, ET < segment begin, ' // &
                   're-use interval right endpoint = segment '  // &
                   'begin.'                                     )
      BODY     = 4
      SEGNO    = 7
      SPKNO    = 7

      IDS(7)   = BODY
      IDS(8)   = BODY
      IDS(9)   = BODY

      TBEGS(9) = SPKNO * SCALE
      TENDS(9) = TBEGS(9) + 3.D0

      TBEGS(8) = TBEGS(9)
      TENDS(8) = TENDS(9)

      TBEGS(7) = TBEGS(9) - 2.D0
      TENDS(7) = TBEGS(9) + 3.D0

      T        = TBEGS(8) - 1.D0

      CALL T_SSFS ( BODY, T, HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )

      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(SPKNO), 0, OK )

      XSEGID(SEGNO) = 'File: # Segno: #'
      CALL REPMC ( XSEGID(SEGNO), '#', SPKS(SPKNO), XSEGID(SEGNO) )
      CALL REPMI ( XSEGID(SEGNO), '#', SEGNO,       XSEGID(SEGNO) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(SEGNO),    OK )

!
!     Check the descriptor as well.  However, don't check the
!     segment addresses.
!
      CALL T_CRDESC ( 'SPK',        SEGNO,        BODY, &
                      TBEGS(SEGNO), TENDS(SEGNO), XDESCR(1,SEGNO) )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,SEGNO), DSCSIZ-1, &
                     0.D0,   OK                                    )

!
!     Some cases where a partial list must be dumped:
!
      CALL TCASE ( 'Dump segment list from SPK 10.  While searching '// &
                   'list for segment for body 4, make upper bound '  // &
                   'of re-use interval < upper bound of '            // &
                   'segment descriptor.'                              )


!
!     Unload SPK 9; load SPK 10.
!
      CALL T_SUEF ( HNDLES(9) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_SLEF ( SPKS(10), HNDLES(10) )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Our request time should be in the interior of segment 15 in
!     SPK 7.
!
      SPKNO    = 7
      SEGNO    = 15

      IDS(15)   =  4
      IDS(16)   =  4
      IDS(17)   =  4

      TBEGS(15) =  SPKNO * SCALE + 10.D0
      TENDS(15) =  TBEGS(15)     +  3.D0

      TBEGS(16) =  TBEGS(15) + 1.D0
      TENDS(16) =  TENDS(15) - 1.D0

      TBEGS(17) =  TBEGS(16)
      TENDS(17) =  TBEGS(17)

      T         =  TBEGS(15) + 0.5D0

      CALL T_SSFS ( BODY, T, HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )

      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(SPKNO), 0, OK )

      XSEGID(SEGNO) = 'File: # Segno: #'
      CALL REPMC ( XSEGID(SEGNO), '#', SPKS(SPKNO), XSEGID(SEGNO) )
      CALL REPMI ( XSEGID(SEGNO), '#', SEGNO,       XSEGID(SEGNO) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(SEGNO),    OK )

!
!     Check the descriptor as well.  However, don't check the
!     segment addresses.
!
      CALL T_CRDESC ( 'SPK',        SEGNO,        BODY, &
                      TBEGS(SEGNO), TENDS(SEGNO), XDESCR(1,SEGNO) )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,SEGNO), DSCSIZ-1, &
                     0.D0,   OK                                    )



      CALL TCASE ( 'Dump segment list from SPK 10.  While searching '// &
                   'list for segment for body 4, make lower bound '  // &
                   'of re-use interval = upper bound of '            // &
                   'segment descriptor.'                              )


      SPKNO    = 7
      BODY     = 4
      TBEGS(9) = SPKNO * SCALE
      TENDS(9) = TBEGS(9) + 3.D0
      T        = TENDS(9) + 0.5D0

      CALL T_SSFS ( BODY, T, HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND',  FOUND, .FALSE., OK )



      CALL TCASE ( 'Dump segment list from SPK 10.  While searching '// &
                   'list for segment for body 5, make lower bound '  // &
                   'of re-use interval > lower bound of '            // &
                   'segment descriptor.'                              )


      SPKNO     =  7
      BODY      =  5
      IDS(18)   =  BODY
      IDS(19)   =  BODY
      IDS(20)   =  BODY

      TBEGS(20) =  SPKNO * SCALE + 10.D0
      TENDS(20) =  TBEGS(20)     +  3.D0

      TBEGS(19) =  TBEGS(20) - 2.D0
      TENDS(19) =  TBEGS(19) + 3.D0

      TBEGS(18) =  TBEGS(19) - 2.D0
      TENDS(18) =  TENDS(20) + 1.D0

      T         =  TENDS(18) - 0.5D0


      CALL T_SSFS ( BODY, T, HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )

      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(SPKNO), 0, OK )

      SEGNO = 18

      XSEGID(SEGNO) = 'File: # Segno: #'
      CALL REPMC ( XSEGID(SEGNO), '#', SPKS(SPKNO), XSEGID(SEGNO) )
      CALL REPMI ( XSEGID(SEGNO), '#', SEGNO,       XSEGID(SEGNO) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(SEGNO),    OK )

!
!     Check the descriptor as well.  However, don't check the
!     segment addresses.
!
      CALL T_CRDESC ( 'SPK',        SEGNO,        BODY, &
                      TBEGS(SEGNO), TENDS(SEGNO), XDESCR(1,SEGNO) )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,SEGNO), DSCSIZ-1, &
                     0.D0,   OK                                    )



      CALL TCASE ( 'Create a situation where room is needed in the '// &
                   'body table, and the second body list has '      // &
                   'expense greater than the first.'                  )

!
!     Unload SPKs 7 and 10.
!
      CALL T_SUEF ( HNDLES(7)   )
      CALL T_SUEF ( HNDLES(10)  )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Fill up (nearly) the segment table with a cheap list for body 2
!     and an expensive list for body 1.
!
      SPKNO = 7
      CALL T_SLEF ( SPKS(SPKNO), HNDLES(SPKNO) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      BODY      = 2
      IDS(2)    = BODY
      IDS(3)    = BODY


      TBEGS(3) = SPKNO * SCALE
      TENDS(3) = TBEGS(3) + 1.D0

      BODY     = 2
      SEGNO    = 3

      CALL T_SSFS ( BODY, TBEGS(SEGNO)+0.5D0, HANDLE, DESCR, SEGID, &
                    FOUND                                        )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     In this case, the segment should be found.  Make sure we get
!     back the right handle and segment identifier.
!
      CALL CHCKSL ( 'FOUND',  FOUND,      .TRUE.,            OK )
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(SPKNO), 0, OK )


      CALL T_SLEF ( SPKS(10), HNDLES(10) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      BODY     = 1
      SPKNO    = 10
      SEGNO    = 1
      I        = 1
      TBEGS(I) = SPKNO * SCALE + I - 1
      TENDS(I) = TBEGS(I)          + 1

      CALL T_SSFS ( BODY, TBEGS(1)+0.5D0, HANDLE, DESCR, SEGID, &
                    FOUND                                             )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND',  FOUND,      .TRUE.,            OK )
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(SPKNO), 0, OK )

!
!     Now do a look up for body 3.  This should cause the segment
!     lists for bodies 2 and 1 to get dumped.
!
      BODY      = 3
      SPKNO     = 10

      I         = 1
      J         = (BODY-1)*(STSIZE-3) + I

      TBEGS(J)  = SPKNO * SCALE + I - 1
      TENDS(J)  = TBEGS(I)          + 1

      XSEGID(J) = 'File: # Segno: #  Body:  #'

      CALL REPMC ( XSEGID(I), '#', SPKS(SPKNO), XSEGID(I) )
      CALL REPMI ( XSEGID(I), '#', J,           XSEGID(I) )
      CALL REPMI ( XSEGID(I), '#', BODY,        XSEGID(I) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SEGNO    = J

      CALL T_SSFS ( BODY, TBEGS(J)+0.5D0, HANDLE, DESCR, SEGID, &
                    FOUND                                     )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND',  FOUND,      .TRUE.,            OK )
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(SPKNO), 0, OK )



!
!     Return on entry in RETURN mode, if the error status is set.
!
      CALL TCASE ( 'Make sure all T_SBSR entry points return on '// &
                   'entry when RETURN() is .TRUE.'                )

!
!     Depending on whether we're calling a version of T_SBSR that does
!     coverage checking, the error status may be reset.

      SMSG = 'Return on entry'

      CALL SIGERR ( SMSG )

      CALL T_SBSR( ' ', 1, 1, 0.0D0, DESCR, SEGID, FOUND )

      IF ( RETURN() ) THEN
         CALL CHCKXC ( .TRUE., SMSG, OK )
      ELSE
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF


      CALL SIGERR ( SMSG )

      CALL T_SLEF ( ' ', HANDLE )

      IF ( RETURN() ) THEN
         CALL CHCKXC ( .TRUE., SMSG, OK )
      ELSE
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF



      CALL SIGERR ( SMSG )

      CALL T_SUEF ( HANDLE )

      IF ( RETURN() ) THEN
         CALL CHCKXC ( .TRUE., SMSG, OK )
      ELSE
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF


      CALL SIGERR ( SMSG )

      CALL T_SSFS  ( 1, 0.D0, HANDLE, DESCR, SEGID, FOUND )

      IF ( RETURN() ) THEN
         CALL CHCKXC ( .TRUE., SMSG, OK )
      ELSE
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF



      CALL TCASE  ( 'Make sure an error is signaled if T_SBSR '// &
                    'is called directly and RETURN() is .FALSE.' )


      CALL T_SBSR( ' ', 1, 1, 0.0D0, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(BOGUSENTRY)', OK )




      CALL TCASE ( 'Try DAFOPR error handling.'  )

      CALL T_SLEF ( 'ThisFileDoesNotExist', HANDLE )

      IF ( RETURN() ) THEN
         CALL CHCKXC ( .TRUE., 'SPICE(FILENOTFOUND)', OK )
      ELSE
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF




      CALL TCASE  ( 'Test partial deletion of a segment list when '// &
                    'a file is unloaded.'                            )

!
!     Unload the SPK files.  The load files 1 and 2.
!
      DO I = 1, NSPK

         CALL T_SUEF ( HNDLES(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      DO I = 1, 2
         CALL T_SLEF ( SPKS(I), HNDLES(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END DO

!
!     Do lookups for body 1 that hit both files.
!
      BODY     = 1
      TBEGS(1) = SCALE + 0.D0

      CALL T_SSFS ( BODY, TBEGS(1)+0.5D0, HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )


      BODY         = 1
      SPKNO        = 2

      SEGNO        = NSEG(SPKNO)/2  +  1

      TBEGS(SEGNO) = SPKNO * SCALE + SEGNO - 1

      CALL T_SSFS ( BODY, TBEGS(SEGNO)+0.5D0, HANDLE, DESCR, SEGID, &
                    FOUND                                             )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )

!
!     Do a lookup for body 2 to create a segment list for that
!     body.
!
      BODY         = 2
      SPKNO        = 2

      SEGNO        = NSEG(SPKNO)/2

      TBEGS(SEGNO) = SPKNO * SCALE + SEGNO - 1

      CALL T_SSFS ( BODY, TBEGS(SEGNO)+0.5D0, HANDLE, DESCR, SEGID, &
                    FOUND                                         )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )


!
!     Reload file 1, removing the portion of body 1's segment list
!     that came from file 1, as part of the unload process that
!     precedes re-loading file 1.
!
      CALL T_SLEF ( SPKS(1), HNDLES(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
!     Create FTSIZE copies of SPK 1 and load FTSIZE-1 of them.  We
!     should get a file table overflow error.
!
      CALL TCASE ( 'File table overflow error.' )


      DO I = 1, FTSIZE

         SPKCPY(I) = 'copy#.bsp'
         CALL REPMI ( SPKCPY(I), '#', I, SPKCPY(I) )

         BODY     = 1
         TBEGS(1) = SCALE + 0.D0
         TENDS(1) = SCALE + 1.D0
         SPKNO    = 1

         XSEGID(1) = 'File: # Segno: #'
         CALL REPMC ( XSEGID(1), '#', SPKCPY(I), XSEGID(1) )
         CALL REPMI ( XSEGID(1), '#', 1,         XSEGID(1) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL T_CRDAF ( 'SPK',  SPKCPY(I),  NSEG(1), &
                         BODY,  TBEGS,      TENDS,   XSEGID )

      END DO


      DO I = 1, FTSIZE-2
         CALL T_SLEF ( SPKCPY(I), CPYHAN(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END DO

      CALL T_SLEF ( SPKCPY(FTSIZE-1), CPYHAN(FTSIZE-1) )

!
!     Note:  if FTSIZE were >= the handle manager file table's size,
!     we would expect the short error message
!
!        SPICE(FTFULL)
!
      CALL CHCKXC ( .TRUE., 'SPICE(SPKFILETABLEFULL)', OK )


!
!     Loading, unloading, and priority checks:
!
      CALL TCASE ( 'Load all copies of SPK 1, looking up the same ' // &
                   'state from each.  Unload the files in reverse ' // &
                   'order.  Repeat 3 times.'                         )

!
!     First, make sure all files are unloaded.
!
      DO I = 1, NSPK

         CALL T_SUEF ( HNDLES(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      DO I = 1, FTSIZE-1

         CALL T_SUEF ( CPYHAN(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      BODY     = 1

      DO I = 1, 3

         DO J = 1, FTSIZE

            TBEGS(J) = SCALE + 0.D0
            TENDS(J) = SCALE + 1.D0

            CALL T_SLEF ( SPKCPY(J), CPYHAN(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            XSEGID(J) = 'File: # Segno: #'
            CALL REPMC ( XSEGID(J), '#', SPKCPY(J), XSEGID(J) )
            CALL REPMI ( XSEGID(J), '#', 1,         XSEGID(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL T_SSFS ( BODY, TBEGS(J)+0.5D0, HANDLE, DESCR, SEGID, &
                          FOUND                                      )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           In this case, the segment should be found.  Make sure
!           we get back the right handle and segment identifier.
!
            CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )
            CALL CHCKSI ( 'HANDLE', HANDLE, '=', CPYHAN(J), 0, OK )
            CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(J),    OK )

!
!           Check the descriptor as well.  However, don't check the
!           segment addresses.
!
            CALL T_CRDESC ( 'SPK', 1, BODY, TBEGS(J), TENDS(J), &
                            XDESCR(1,J)                         )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,J), DSCSIZ-1, &
                           0.D0,   OK                               )

         END DO

!
!        Now unload files, looking up states as we go.
!
         DO J = FTSIZE-1, 1, -1

            CALL T_SUEF ( CPYHAN(J+1) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            TBEGS(J) = SCALE + 0.D0
            TENDS(J) = SCALE + 1.D0

            XSEGID(J) = 'File: # Segno: #'
            CALL REPMC ( XSEGID(J), '#', SPKCPY(J), XSEGID(J) )
            CALL REPMI ( XSEGID(J), '#', 1,         XSEGID(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL T_SSFS ( BODY, TBEGS(J)+0.5D0, HANDLE, DESCR, SEGID, &
                          FOUND                                      )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           In this case, the segment should be found.  Make sure
!           we get back the right handle and segment identifier.
!
            CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )
            CALL CHCKSI ( 'HANDLE', HANDLE, '=', CPYHAN(J), 0, OK )
            CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(J),    OK )

!
!           Check the descriptor as well.  However, don't check the
!           segment addresses.
!
            CALL T_CRDESC ( 'SPK', 1, BODY, TBEGS(J), TENDS(J), &
                            XDESCR(1,J)                         )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,J), DSCSIZ-1, &
                           0.D0,   OK                                )

         END DO

      END DO

!
!     Make sure we don't accumulate DAF links by re-loading a file.
!
      CALL TCASE ( 'Load the first SPK file 2*FTSIZE times.' )

      DO I = 1, FTSIZE*2

         TBEGS(1) = SCALE + 0.D0
         TENDS(1) = SCALE + 1.D0

         CALL T_SLEF ( SPKS(1), HNDLES(1) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XSEGID(1) = 'File: # Segno: #'
         CALL REPMC ( XSEGID(1), '#', SPKS(1), XSEGID(1) )
         CALL REPMI ( XSEGID(1), '#', 1,       XSEGID(1) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL T_SSFS ( BODY, TBEGS(1)+0.5D0, HANDLE, DESCR, SEGID, &
                       FOUND                                      )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        In this case, the segment should be found.  Make sure
!        we get back the right handle and segment identifier.
!
         CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(1), 0, OK )
         CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(1),    OK )

!
!        Check the descriptor as well.  However, don't check the
!        segment addresses.
!
         CALL T_CRDESC ( 'SPK', 1, BODY, TBEGS(1), TENDS(1), &
                          XDESCR(1,1) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,1), DSCSIZ-1, &
                        0.D0,   OK                                )

      END DO

!
!     Last step:  delete all of the SPK files we created.
!
      DO I = 1, NSPK

         CALL T_SUEF ( HNDLES(I) )
         CALL DELFIL ( SPKS  (I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      DO I = 1, FTSIZE

         CALL T_SUEF ( CPYHAN(I) )
         CALL DELFIL ( SPKCPY(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


      CALL T_SUCCESS ( OK )
      RETURN
      END
