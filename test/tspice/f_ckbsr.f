C$Procedure F_CKBSR ( Family of tests for T_CBR )

      SUBROUTINE F_CKBSR ( OK )

C$ Abstract
C
C     This routine tests the CK segment selection and buffering system,
C     which is implemented by T_CBR.
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
C     OK       is a logical that indicates the test status to the
C              caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  This routine does not generate any errors. Routines in its
C         call tree may generate errors that are either intentional and
C         trapped or unintentional and need reporting. The test family
C         utilities manage this.
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
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C     B.V. Semenov       (JPL)
C     E.D. Wright        (JPL)
C
C$ Version
C
C-    TSPICE Version 2.1.0, 05-JUN-2021 (BVS) (JDR)
C
C        Added missing 'D' to literal DP constants.
C
C        Edited the header to comply with NAIF standard.
C
C-    TSPICE Version 2.0.0, 17-MAR-2014 (NJB)
C
C        Table size parameters
C
C           FTSIZE
C           ITSIZE
C           STSIZE
C
C        were modified for compatibility with
C
C           T_CKBSR
C
C-    TSPICE Version 1.1.0, 13-DEC-2011 (EDW)
C
C        All variables to SAVE for f2c'd version compiled with Borland.
C        Implemented a proper SPICE header.
C
C-    TSPICE Version 1.0.0, 28-NOV-2001 (NJB)
C
C-&


C$ Index_Entries
C
C     Test CK segment buffering system
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               EVEN
      LOGICAL               RETURN

C
C     Local parameters
C
      DOUBLE PRECISION      TOL0
      PARAMETER           ( TOL0   = 0.00D0 )

      INTEGER               FTSIZE
      PARAMETER           ( FTSIZE = 500 )

      INTEGER               STSIZE
      PARAMETER           ( STSIZE = 1000 )

      INTEGER               ITSIZE
      PARAMETER           ( ITSIZE = 100 )

      INTEGER               DSCSIZ
      PARAMETER           ( DSCSIZ = 5 )

      INTEGER               ND
      PARAMETER           ( ND     = 2 )

      INTEGER               NI
      PARAMETER           ( NI     = 6 )

      INTEGER               SIDLEN
      PARAMETER           ( SIDLEN = 40 )

      INTEGER               NINST
      PARAMETER           ( NINST  = 4 )

C
C     MAXSEG should be set to MAX ( NINST * STSIZE, FTSIZE ).
C
      INTEGER               MAXSEG
      PARAMETER           ( MAXSEG = NINST * STSIZE )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               NCK
      PARAMETER           ( NCK    = 15 )

C
C     The number of segments in the respective CK files:
C
      INTEGER               NSEG1
      PARAMETER           ( NSEG1  = 1 )

      INTEGER               NSEG2
      PARAMETER           ( NSEG2  = STSIZE/2 )

      INTEGER               NSEG3
      PARAMETER           ( NSEG3  = STSIZE/2 )

      INTEGER               NSEG4
      PARAMETER           ( NSEG4  = 10 )

      INTEGER               NSEG5
      PARAMETER           ( NSEG5  = STSIZE )

      INTEGER               NSEG6
      PARAMETER           ( NSEG6  = STSIZE+10 )

      INTEGER               NSEG7
      PARAMETER           ( NSEG7  = ITSIZE )

      INTEGER               NSEG8
      PARAMETER           ( NSEG8  = 23 )

      INTEGER               NSEG9
      PARAMETER           ( NSEG9  = NSEG8 )

      INTEGER               NSEG10
      PARAMETER           ( NSEG10 = NINST * STSIZE )

      INTEGER               NSEG11
      PARAMETER           ( NSEG11 = NINST * (STSIZE-3) )

      INTEGER               NSEG12
      PARAMETER           ( NSEG12 = STSIZE )

      INTEGER               NSEG13
      PARAMETER           ( NSEG13 = 10 )

      INTEGER               NSEG14
      PARAMETER           ( NSEG14 = STSIZE/2 + 10 )

      INTEGER               NSEG15
      PARAMETER           ( NSEG15 = STSIZE/2 )

C
C     Other parameters:
C
      INTEGER               FILSIZ
      PARAMETER           ( FILSIZ = 255 )

      INTEGER               SCALE
      PARAMETER           ( SCALE  = 10000 )

      INTEGER               SMSLEN
      PARAMETER           ( SMSLEN = 25 )

      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 50 )

C
C     Local variables
C
      CHARACTER*(SIDLEN)    SEGID
      CHARACTER*(SMSLEN)    SMSG
      CHARACTER*(FILSIZ)    CKCPY  ( FTSIZE )
      CHARACTER*(FILSIZ)    CKS    ( NCK )
      CHARACTER*(SIDLEN)    XSEGID ( MAXSEG )

      DOUBLE PRECISION      DESCR  ( DSCSIZ )
      DOUBLE PRECISION      T
      DOUBLE PRECISION      TBEGS  ( MAXSEG )
      DOUBLE PRECISION      TENDS  ( MAXSEG )
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      XDESCR ( DSCSIZ, MAXSEG )

      INTEGER               CKNO
      INTEGER               CPYHAN ( FTSIZE )
      INTEGER               HANDLE
      INTEGER               HNDLES ( NCK )
      INTEGER               I
      INTEGER               IDS    ( MAXSEG )
      INTEGER               INST
      INTEGER               J
      INTEGER               NSEG   ( NCK )
      INTEGER               SEGNO

      LOGICAL               AVFLAG
      LOGICAL               FOUND

C
C     Saved variables
C
      SAVE

C
C     Initial values
C
      DATA                  CKS   /  'bsr1.bc',  'bsr2.bc',
     .                               'bsr3.bc',  'bsr4.bc',
     .                               'bsr5.bc',  'bsr6.bc',
     .                               'bsr7.bc',  'bsr8.bc',
     .                               'bsr9.bc',  'bsr10.bc',
     .                               'bsr11.bc', 'bsr12.bc',
     .                               'bsr13.bc', 'bsr14.bc',
     .                               'bsr15.bc'              /

      DATA                  NSEG   / NSEG1,   NSEG2,  NSEG3,  NSEG4,
     .                               NSEG5,   NSEG6,  NSEG7,  NSEG8,
     .                               NSEG9,   NSEG10, NSEG11, NSEG12,
     .                               NSEG13,  NSEG14, NSEG15         /


C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_CKBSR' )


      CALL TCASE ( 'The first CK file contains 1 segment for '    //
     .             'instrument 1. Make sure we can look up data ' //
     .             'from this file.'                               )

C
C     Create the first CK file.
C
      INST     = 1
      TBEGS(1) = SCALE + 0.D0
      TENDS(1) = SCALE + 1.D0
      CKNO     = 1

      XSEGID(1) = 'File: # Segno: #'
      CALL REPMC  ( XSEGID(1), '#', CKS(1), XSEGID(1) )
      CALL REPMI  ( XSEGID(1), '#', 1,      XSEGID(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CRDAF  ( 'CK',   CKS(1),  NSEG(1),
     .                 INST,  TBEGS,   TENDS,   XSEGID )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CLF  ( CKS(1), HNDLES(1) )
      CALL CHCKXC ( .FALSE., ' ', OK  )

      CALL T_CBS  ( INST, TBEGS(1)+0.5, TOL0, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     In this case, the segment should be found.  Make sure we get
C     back the right handle and segment identifier.
C
      CALL CHCKSL ( 'FOUND',  FOUND,      .TRUE.,        OK )
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(1), 0, OK )
      CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(1),    OK )

C
C     Check the descriptor as well.  However, don't check the
C     segment addresses.
C
      CALL T_CRCKDS ( 1, INST, TBEGS(1), TENDS(1), XDESCR(1,1), .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,1), DSCSIZ-1, 0.D0,
     .               OK                                              )



      CALL TCASE ( 'Try to look up data for a different instrument'  //
     .             'in CK 1.  Also look up data for instrument 1 '   //
     .             'for a time which is not covered.'                 )

      CALL T_CBS  ( 2, TBEGS(1)+0.5, TOL0, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     In this case, the segment should not be found.
C
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,    OK )


      CALL T_CBS  ( 1, TBEGS(1)+10.D0, TOL0, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     In this case, the segment should not be found.
C
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,    OK )


      CALL TCASE ( 'Create a second CK containing data for '        //
     .             'instrument 1 and instrument 2.  Load this CK, ' //
     .             'then look up a state covered by the new file.'    )

      INST  = 1
      CKNO  = 2

      DO I = 1, NSEG(CKNO)

         IF ( I .LE. NSEG(CKNO)/2 ) THEN
            IDS(I)   = 2
         ELSE
            IDS(I)   = 1
         END IF

         TBEGS(I) = CKNO * SCALE + I - 1
         TENDS(I) = TBEGS(I)          + 1

         XSEGID(I) = 'File: # Segno: #'

         CALL REPMC  ( XSEGID(I), '#', CKS(CKNO), XSEGID(I) )
         CALL REPMI  ( XSEGID(I), '#', I,         XSEGID(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


      CALL T_CRDAF ( 'CK',   CKS(CKNO),  NSEG(CKNO),
     .                IDS,   TBEGS,      TENDS,       XSEGID )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CLF  ( CKS(CKNO), HNDLES(CKNO) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SEGNO = NSEG(CKNO)


      CALL T_CBS  ( INST, TBEGS(SEGNO)+0.5D0, TOL0, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     In this case, the segment should be found.  Make sure we get
C     back the right handle and segment identifier.
C
      CALL CHCKSL ( 'FOUND',  FOUND,      .TRUE.,            OK )
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(CKNO),  0, OK )
      CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(SEGNO),    OK )

C
C     Check the descriptor as well.  However, don't check the
C     segment addresses.
C
      CALL T_CRCKDS ( SEGNO, INST,     TBEGS(SEGNO), TENDS(SEGNO),
     .                XDESCR(1,SEGNO), .TRUE.                        )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,SEGNO), DSCSIZ-1,
     .               0.D0,   OK                                    )




      CALL TCASE ( 'Look up data for instrument 2.  This should '//
     .             'cause an OLD FILES search.'                    )

      INST  = 2
      CKNO  = 2
      SEGNO = 1



      CALL T_CBS  ( INST, TBEGS(SEGNO)+0.5D0, TOL0, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     In this case, the segment should be found.  Make sure we get
C     back the right handle and segment identifier.
C
      CALL CHCKSL ( 'FOUND',  FOUND,      .TRUE.,            OK )
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(CKNO),  0, OK )
      CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(SEGNO),    OK )

C
C     Check the descriptor as well.  However, don't check the
C     segment addresses.
C
      CALL T_CRCKDS ( SEGNO, INST,     TBEGS(SEGNO), TENDS(SEGNO),
     .                XDESCR(1,SEGNO), .TRUE.                      )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,SEGNO), DSCSIZ-1,
     .               0.D0,   OK                                    )



      CALL TCASE ( 'Create a third CK containing data for instrument '//
     .             '3. Load this CK, then look up a state covered '   //
     .             'by the new file. This should cause the segment '  //
     .             'list for instrument 1 to get dumped.'             )

      INST  = 3
      CKNO  = 3

      DO I = 1, NSEG(CKNO)

         IDS(I) = INST

         TBEGS(I) = CKNO * SCALE + I - 1
         TENDS(I) = TBEGS(I)          + 1

         XSEGID(I) = 'File: # Segno: #'

         CALL REPMC  ( XSEGID(I), '#', CKS(CKNO), XSEGID(I) )
         CALL REPMI  ( XSEGID(I), '#', I,         XSEGID(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


      CALL T_CRDAF ( 'CK',  CKS(CKNO),  NSEG(CKNO),
     .                IDS,  TBEGS,      TENDS,      XSEGID )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CLF  ( CKS(CKNO), HNDLES(CKNO) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SEGNO = NSEG(CKNO)


      CALL T_CBS  ( INST, TBEGS(SEGNO)+0.5D0, TOL0, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     In this case, the segment should be found.  Make sure we get
C     back the right handle and segment identifier.
C
      CALL CHCKSL ( 'FOUND',  FOUND,      .TRUE.,           OK )
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(CKNO), 0, OK )
      CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(SEGNO),   OK )

C
C     Check the descriptor as well.  However, don't check the
C     segment addresses.
C
      CALL T_CRCKDS ( SEGNO, INST, TBEGS(SEGNO), TENDS(SEGNO),
     .                XDESCR(1,SEGNO), .TRUE.                 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,SEGNO), DSCSIZ-1,
     .               0.D0,   OK                                    )






      CALL TCASE ( 'Create another CK for instrument 2 and load it. ' //
     .             'Make another read request for instrument 2 that ' //
     .             'will be satisfied by a segment in file 3. This '  //
     .             'should result in another OLD FILES search.'       )


      INST = 2
      CKNO = 4

      DO I = 1, NSEG(CKNO)

         IDS(I)   = INST

         TBEGS(I) = CKNO * SCALE + I - 1
         TENDS(I) = TBEGS(I)         + 1

         XSEGID(I) = 'File: # Segno: #'

         CALL REPMC  ( XSEGID(I), '#', CKS(CKNO), XSEGID(I) )
         CALL REPMI  ( XSEGID(I), '#', I,         XSEGID(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL T_CRDAF ( 'CK',   CKS(CKNO),  NSEG(CKNO),
     .                IDS,   TBEGS,      TENDS,       XSEGID )

      CALL CHCKXC ( .FALSE., ' ', OK )





      CALL TCASE ( 'Create another CK for instrument 1 and load it. ' //
     .             'The segment count in this file is such that all ' //
     .             'other instrument lists must be dumped to make '   //
     .             'room. Then make a request that is satisfied by '  //
     .             'CK 1.  The segment in CK 1 cannot be added to '   //
     .             'the segment table.'                                )

      INST = 1
      CKNO = 5

      DO I = 1, NSEG(CKNO)

         IDS(I)   = INST

         TBEGS(I) = CKNO * SCALE + I - 1
         TENDS(I) = TBEGS(I)         + 1

         XSEGID(I) = 'File: # Segno: #'

         CALL REPMC  ( XSEGID(I), '#', CKS(CKNO), XSEGID(I) )
         CALL REPMI  ( XSEGID(I), '#', I,         XSEGID(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL T_CRDAF ( 'CK',  CKS(CKNO),  NSEG(CKNO),
     .                IDS,  TBEGS,      TENDS,       XSEGID )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CLF  ( CKS(CKNO), HNDLES(CKNO) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CKNO  = 1
      SEGNO = 1

      TBEGS(SEGNO) = CKNO * SCALE + SEGNO - 1
      TENDS(SEGNO) = CKNO * SCALE + SEGNO

      CALL T_CBS  ( INST, TBEGS(SEGNO)+0.5D0, TOL0, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     In this case, the segment should be found.  Make sure we get
C     back the right handle and segment identifier.
C
      XSEGID(SEGNO) = 'File: # Segno: #'
      CALL REPMC ( XSEGID(SEGNO), '#', CKS(CKNO), XSEGID(SEGNO) )
      CALL REPMI ( XSEGID(SEGNO), '#', 1,         XSEGID(SEGNO) )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND',  FOUND,      .TRUE.,           OK )
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(CKNO), 0, OK )
      CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(SEGNO),   OK )

C
C     Check the descriptor as well.  However, don't check the
C     segment addresses.
C
      CALL T_CRCKDS ( SEGNO, INST, TBEGS(SEGNO), TENDS(SEGNO),
     .                XDESCR(1,SEGNO), .TRUE.                     )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,SEGNO), DSCSIZ-1,
     .               0.D0,   OK                                    )


      CALL TCASE ( 'Start a segment list for instrument 1 by '        //
     .             'making a request that is satisfied by CK 1.  '    //
     .             'Then build a file (CK 6) with too many segments ' //
     .             'for instrument 1 to be buffered.  Make a '        //
     .             'request that is satisfied by CK 6. This tests '   //
     .             'the logic for searching the subset of a segment ' //
     .             'list that must be dumped due to lack of room.'     )


C
C     Set up by making a request that will be satisfied by the segment
C     in CK 1.  This builds up the segment list for instrument 1.
C
      INST     = 1
      TBEGS(1) = SCALE + 0.D0
      TENDS(1) = SCALE + 1.D0
      CKNO     = 1
      SEGNO    = 1

      XSEGID(1) = 'File: # Segno: #'
      CALL REPMC  ( XSEGID(1), '#', CKS(1), XSEGID(1) )
      CALL REPMI  ( XSEGID(1), '#', SEGNO,  XSEGID(1) )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CBS  ( INST, TBEGS(SEGNO)+0.5D0, TOL0, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Go ahead and make the new file.
C
      INST  = 1
      CKNO  = 6

      DO I = 1, NSEG(CKNO)

         IDS(I)   = INST

         IF ( ( I .EQ. 10 ) .OR. ( I .EQ. STSIZE+1 ) ) THEN
C
C           We want the lower bound of the re-use interval to
C           match the right endpoint of the segment's coverage
C           interval.
C
            TBEGS(I-1) = CKNO * SCALE + I
            TENDS(I-1) = TBEGS(I-1)   + 1.0D0

            TBEGS(I)   = CKNO * SCALE + I - 1
            TENDS(I)   = TBEGS(I)         + 1

            TBEGS(I+1) = TBEGS(I)
            TENDS(I+1) = TENDS(I)

            TBEGS(I+2) = TENDS(I)   + 1
            TENDS(I+2) = TBEGS(I+2) + 1


         ELSE IF ( I .EQ. STSIZE+6 ) THEN
C
C           Create a singleton segment.
C
            TBEGS(I)   =  CKNO * SCALE + I - 1
            TENDS(I)   =  TBEGS(I)

         ELSE IF ( I .EQ. STSIZE+7 ) THEN
C
C           Create an invisible segment.
C
            TBEGS(I)   =  CKNO * SCALE + I - 1
            TENDS(I)   =  TBEGS(I) - 1

         ELSE IF (     (   I .LT. 9   )
     .            .OR. ( ( I .GT. 12  ) .AND. (I .LT. STSIZE) )
     .            .OR. (   I .GT. STSIZE+3 )                      ) THEN

            TBEGS(I) = CKNO * SCALE + I - 1
            TENDS(I) = TBEGS(I)         + 1

         END IF

         XSEGID(I) = 'File: # Segno: #'

         CALL REPMC  ( XSEGID(I), '#', CKS(CKNO), XSEGID(I) )
         CALL REPMI  ( XSEGID(I), '#', I,         XSEGID(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


      CALL T_CRDAF ( 'CK',  CKS(CKNO),  NSEG(CKNO),
     .                IDS,  TBEGS,      TENDS,       XSEGID )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CLF  ( CKS(CKNO), HNDLES(CKNO) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SEGNO = 1

      CALL T_CBS  ( INST, TBEGS(SEGNO)+0.5D0, TOL0, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     In this case, the segment should be found.  Make sure we get
C     back the right handle and segment identifier.
C

      XSEGID(SEGNO) = 'File: # Segno: #'
      CALL REPMC ( XSEGID(SEGNO), '#', CKS(CKNO), XSEGID(SEGNO) )
      CALL REPMI ( XSEGID(SEGNO), '#', 1,         XSEGID(SEGNO) )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND',  FOUND,      .TRUE.,           OK )
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(CKNO), 0, OK )
      CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(SEGNO),   OK )

C
C     Check the descriptor as well.  However, don't check the
C     segment addresses.
C
      CALL T_CRCKDS ( SEGNO, INST, TBEGS(SEGNO), TENDS(SEGNO),
     .                XDESCR(1,SEGNO), .TRUE.                 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,SEGNO), DSCSIZ-1,
     .               0.D0,   OK                                    )






      CALL TCASE ( 'Create a CK containing data for ITSIZE new '//
     .             'instruments. Look up data for each.'               )


C
C     Unload all CKs.
C
      DO I = 1, NCK
         CALL  T_CUF( HNDLES(I) )
      END DO

      CKNO  = 7

      DO I = 1, NSEG(CKNO)

         IDS(I)    = ITSIZE+I

         TBEGS(I)  = CKNO * SCALE + I - 1
         TENDS(I)  = TBEGS(I)         + 1

         XSEGID(I) = 'File: # Segno: #'

         CALL REPMC  ( XSEGID(I), '#', CKS(CKNO), XSEGID(I) )
         CALL REPMI  ( XSEGID(I), '#', I,           XSEGID(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL T_CRDAF ( 'CK',  CKS(CKNO),  NSEG(CKNO),
     .                IDS,  TBEGS,      TENDS,       XSEGID )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CLF  ( CKS(CKNO), HNDLES(CKNO) )
      CALL CHCKXC ( .FALSE., ' ', OK )


      DO I = 1, NSEG(CKNO)

         INST  = IDS(I)
         SEGNO = I

         CALL T_CBS  ( INST, TBEGS(SEGNO)+0.5D0, TOL0, .TRUE. )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        In this case, the segment should be found.  Make sure we get
C        back the right handle and segment identifier.
C
         XSEGID(SEGNO) = 'File: # Segno: #'
         CALL REPMC ( XSEGID(SEGNO), '#', CKS(CKNO), XSEGID(SEGNO) )
         CALL REPMI ( XSEGID(SEGNO), '#', SEGNO,     XSEGID(SEGNO) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSL ( 'FOUND',  FOUND,      .TRUE.,           OK )
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(CKNO), 0, OK )
         CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(SEGNO),   OK )

C
C        Check the descriptor as well.  However, don't check the
C        segment addresses.
C
         CALL T_CRCKDS ( SEGNO, INST, TBEGS(SEGNO), TENDS(SEGNO),
     .                   XDESCR(1,SEGNO), .TRUE.                  )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,SEGNO), DSCSIZ-1,
     .                  0.D0,   OK                                    )

      END DO





      CALL TCASE ( 'The instrument table should be full now; the '    //
     .             'segment table should have room.  Cause '          //
     .             'an instrument list to be dumped to make room in ' //
     .             'the instrument table.'                             )

C
C     Create a list for instrument 1 more expensive than those for the
C     instruments in CK 7.  Instrument 1's list will be placed at the
C     head of the instrument table.
C
      INST     = 1
      CKNO     = 2
      SEGNO    = NSEG(CKNO)
      I        = SEGNO
      TBEGS(I) = CKNO * SCALE + I - 1
      TENDS(I) = TBEGS(I)         + 1

      XSEGID(1) = 'File: # Segno: #'
      CALL REPMC  ( XSEGID(1), '#', CKS(CKNO), XSEGID(1) )
      CALL REPMI  ( XSEGID(1), '#', SEGNO,     XSEGID(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CLF  ( CKS(2), HNDLES(2) )
      CALL CHCKXC ( .FALSE., ' ', OK  )

      CALL T_CBS  ( INST, TBEGS(I)+0.5D0, TOL0, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     In this case, the segment should be found.  Make sure we get
C     back the right handle and segment identifier.
C
      CALL CHCKSL ( 'FOUND',  FOUND,      .TRUE.,           OK )
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(CKNO), 0, OK )
      CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(1),       OK )

C
C     Check the descriptor as well.  However, don't check the
C     segment addresses.
C
      CALL T_CRCKDS( SEGNO, INST, TBEGS(I), TENDS(I), XDESCR(1,1),
     .               .TRUE.                                       )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,1), DSCSIZ-1,
     .               0.D0,   OK                                   )

C
C     Now do a look up for instrument 2.  This will require dumping
C     lists from CK 7.
C
      INST     = 2
      CKNO     = 2
      SEGNO    = 1
      I        = SEGNO
      TBEGS(I) = CKNO * SCALE + I - 1
      TENDS(I) = TBEGS(I)         + 1


      XSEGID(SEGNO) = 'File: # Segno: #'
      CALL REPMC ( XSEGID(SEGNO), '#', CKS(CKNO), XSEGID(SEGNO) )
      CALL REPMI ( XSEGID(SEGNO), '#', SEGNO,     XSEGID(SEGNO) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CBS  ( INST, TBEGS(SEGNO)+0.5D0, TOL0, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     In this case, the segment should be found.  Make sure we get
C     back the right handle and segment identifier.
C
      CALL CHCKSL ( 'FOUND',  FOUND,      .TRUE.,            OK )
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(CKNO), 0, OK )
      CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(SEGNO),    OK )

C
C     Check the descriptor as well.  However, don't check the
C     segment addresses.
C
      CALL T_CRCKDS ( 1, INST, TBEGS(1), TENDS(1), XDESCR(1,1), .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,1), DSCSIZ-1,
     .               0.D0,   OK                                 )



      CALL TCASE ( 'Look up data from a representative subset ' //
     .             'of the segments in CK 6.'                     )

      CKNO = 6

      CALL T_CLF ( CKS(CKNO), HNDLES(CKNO) )
      CALL CHCKXC ( .FALSE., ' ', OK )


      DO I = 1, NSEG(CKNO)

         IDS(I)   = INST

         IF ( ( I .EQ. 10 ) .OR. ( I .EQ. STSIZE+1 ) ) THEN
C
C           We want the lower bound of the re-use interval to
C           match the right endpoint of the segment's coverage
C           interval.
C
            TBEGS(I-1) = CKNO * SCALE + I
            TENDS(I-1) = TBEGS(I-1)        + 1.0D0

            TBEGS(I)   = CKNO * SCALE + I - 1
            TENDS(I)   = TBEGS(I)          + 1

            TBEGS(I+1) = TBEGS(I)
            TENDS(I+1) = TENDS(I)

            TBEGS(I+2) = TENDS(I)          + 1
            TENDS(I+2) = TBEGS(I+2)        + 1

         ELSE IF ( I .EQ. STSIZE+6 ) THEN
C
C           Create a singleton segment.
C
            TBEGS(I)   =  CKNO * SCALE + I - 1
            TENDS(I)   =  TBEGS(I)

         ELSE IF ( I .EQ. STSIZE+7 ) THEN
C
C           Create an invisible segment.
C
            TBEGS(I)   =  CKNO * SCALE + I - 1
            TENDS(I)   =  TBEGS(I) - 1

         ELSE IF (     ( I .LT. 10   )
     .            .OR. ( ( I .GT. 12 ) .AND. ( I .LT. STSIZE ) )
     .            .OR. ( I .GT. STSIZE+3 )                        ) THEN

            TBEGS(I) = CKNO * SCALE + I - 1
            TENDS(I) = TBEGS(I)          + 1

         END IF

         XSEGID(I) = 'File: # Segno: #'

         CALL REPMC ( XSEGID(I), '#', CKS(CKNO), XSEGID(I) )
         CALL REPMI ( XSEGID(I), '#', I,           XSEGID(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


      I = 1

      DO WHILE ( I .LE. NSEG(CKNO) )

         INST  = 1
         SEGNO = I

         CALL T_CBS  ( INST, TBEGS(SEGNO)+0.5D0, TOL0, .TRUE. )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        In this case, the segment should be found.  Make sure we get
C        back the right handle and segment identifier.
C
         XSEGID(SEGNO) = 'File: # Segno: #'
         CALL REPMC ( XSEGID(SEGNO), '#', CKS(CKNO), XSEGID(SEGNO) )
         CALL REPMI ( XSEGID(SEGNO), '#', SEGNO,     XSEGID(SEGNO) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSL ( 'FOUND',  FOUND,      .TRUE.,           OK )
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(CKNO), 0, OK )
         CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(SEGNO),   OK )

C
C        Check the descriptor as well.  However, don't check the
C        segment addresses.
C
         CALL T_CRCKDS ( SEGNO, INST, TBEGS(SEGNO), TENDS(SEGNO),
     .                   XDESCR(1,SEGNO),  .TRUE.                 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,SEGNO), DSCSIZ-1,
     .                  0.D0,   OK                                    )

C
C        Skip some tests that are unlikely to reveal bugs, as well as
C        those which would give anomalous results due to the structure
C        of CK 6.
C
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


C
C     Try a search w/o buffering case where no segment is found.
C
      CALL TCASE ( 'Search w/o buffering, no segment should be found.' )
      CKNO  = 6
      INST  = 1
      T     = 2 * TENDS( NSEG(CKNO) )


      CALL T_CBS  ( INST, T, TOL0, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND',  FOUND, .FALSE., OK )


C
C     Return on entry in RETURN mode, if the error status is set.
C
      CALL TCASE ( 'Make sure all T_CBR entry points return on entry '//
     .             'when RETURN() is .TRUE.'                           )

C
C     Depending on whether we're calling a version of T_CBR that does
C     coverage checking, the error status may be reset.

      SMSG = 'Return on entry'

      CALL SIGERR ( SMSG )

      CALL T_CBR(' ', 1, 1, 0.0D0, 0.0D0, .FALSE., DESCR, SEGID, FOUND )

      IF ( RETURN() ) THEN
         CALL CHCKXC ( .TRUE., SMSG, OK )
      ELSE
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF


      CALL SIGERR ( SMSG )

      CALL T_CLF ( ' ', HANDLE )

      IF ( RETURN() ) THEN
         CALL CHCKXC ( .TRUE., SMSG, OK )
      ELSE
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF


      CALL SIGERR ( SMSG )


      CALL T_CUF ( HANDLE )

      IF ( RETURN() ) THEN
         CALL CHCKXC ( .TRUE., SMSG, OK )
      ELSE
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF


      CALL SIGERR ( SMSG )

      CALL T_CBS  ( 1, 0.D0, TOL0, .TRUE. )

      IF ( RETURN() ) THEN
         CALL CHCKXC ( .TRUE., SMSG, OK )
      ELSE
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF


      CALL SIGERR ( SMSG )

      CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )

      IF ( RETURN() ) THEN
         CALL CHCKXC ( .TRUE., SMSG, OK )
      ELSE
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF



      CALL TCASE  ( 'Make sure an error is signaled if T_CBR '//
     .              'is called directly and RETURN() is .FALSE.' )


      CALL T_CBR(' ', 1, 1, 0.0D0, 0.0D0, .FALSE., DESCR, SEGID, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(CKBOGUSENTRY)', OK )


C
C     Unload the CK files.
C
      DO I = 1, NCK
         CALL T_CUF ( HNDLES(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END DO

C
C     Make sure an error is signaled if no CKs are loaded.
C
      CALL TCASE  ( 'Make sure an error is signaled if no CKs '//
     .              'are loaded.'                               )
      CALL T_CBS  ( 1, 0.D0, TOL0, .TRUE. )

      CALL CHCKXC ( .TRUE., 'SPICE(NOLOADEDFILES)', OK )

C
C     Load CK1 and look up a state from it to create a cheap list.
C     Make the cheap list the second list by looking up data from
C     it after looking up data for instrument ITSIZE+1.
C
      CALL TCASE ( 'Test removal of cheap list when adding a new ' //
     .             'instrument; cheap list is 2nd.'                  )

      CALL T_CLF  ( CKS(1), HNDLES(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Now load the CK containing 100 instruments.  Look up data for
C     each one.  The last one will cause the list for instrument 1 to
C     be dumped.
C
      CKNO = 7
      CALL T_CLF ( CKS(CKNO), HNDLES(CKNO) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, NSEG(CKNO)

         IDS(I)   = ITSIZE+I

         TBEGS(I) = CKNO * SCALE + I - 1
         TENDS(I) = TBEGS(I)          + 1

         XSEGID(I) = 'File: # Segno: #'

         CALL REPMC ( XSEGID(I), '#', CKS(CKNO), XSEGID(I) )
         CALL REPMI ( XSEGID(I), '#', I,           XSEGID(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


      DO I = 1, NSEG(CKNO)

         INST  = IDS(I)
         SEGNO = I

         CALL T_CBS  ( INST, TBEGS(SEGNO)+0.5D0, TOL0, .TRUE. )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        In this case, the segment should be found.  Make sure we get
C        back the right handle and segment identifier.
C
         XSEGID(SEGNO) = 'File: # Segno: #'
         CALL REPMC ( XSEGID(SEGNO), '#', CKS(CKNO), XSEGID(SEGNO) )
         CALL REPMI ( XSEGID(SEGNO), '#', SEGNO,     XSEGID(SEGNO) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSL ( 'FOUND',  FOUND,      .TRUE.,           OK )
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(CKNO), 0, OK )
         CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(SEGNO),   OK )

C
C        Check the descriptor as well.  However, don't check the
C        segment addresses.
C
         CALL T_CRCKDS ( SEGNO, INST, TBEGS(SEGNO), TENDS(SEGNO),
     .                   XDESCR(1,SEGNO), .TRUE.                    )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,SEGNO), DSCSIZ-1,
     .                  0.D0,   OK                                    )

         IF ( I .EQ. 1 ) THEN
C
C           Create a cheap list for instrument 1.
C
           INST = 1

           CALL T_CLF  ( CKS(CKNO), HNDLES(CKNO) )
           CALL CHCKXC ( .FALSE., ' ', OK )
           TBEGS(1) = SCALE

           CALL T_CBS  ( INST, TBEGS(1)+0.5D0, TOL0, .TRUE. )
           CALL CHCKXC ( .FALSE., ' ', OK )
           CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
           CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

      END DO


      CALL TCASE ( 'Test ability to make room by deleting an ' //
     .             'instrument table entry with an empty list.' )

C
C     Create an example of the list in question by forcing a search
C     without buffering on instrument 1, where the highest priority file
C     contains too many segments to buffer.  However, we want this
C     list to have a high expense, so load a CK with many segments
C     for this instrument and search it first.
C
      CKNO = 6
      CALL T_CLF ( CKS(CKNO), HNDLES(CKNO) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      INST = 1
      T    = SCALE * CKNO + (STSIZE+1) - 1 + 0.5D0

      CALL T_CBS  ( INST, T, TOL0, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Now look up data for the first NSEG-1 instruments in CK 7.  This
C     should fill up the instrument table.
C
      CKNO = 7

      DO I = 1, NSEG(CKNO)-1

         IDS(I)   = ITSIZE+I

         TBEGS(I) = CKNO * SCALE + I - 1
         TENDS(I) = TBEGS(I)          + 1

         INST     = IDS(I)
         SEGNO    = I

         CALL T_CBS  ( INST, TBEGS(SEGNO)+0.5D0, TOL0, .TRUE. )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        In this case, the segment should be found.  Make sure we get
C        back the right handle and segment identifier.
C
         XSEGID(SEGNO) = 'File: # Segno: #'
         CALL REPMC ( XSEGID(SEGNO), '#', CKS(CKNO), XSEGID(SEGNO) )
         CALL REPMI ( XSEGID(SEGNO), '#', SEGNO,     XSEGID(SEGNO) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSL ( 'FOUND',  FOUND,      .TRUE.,           OK )
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(CKNO), 0, OK )
         CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(SEGNO),   OK )

C
C        Check the descriptor as well.  However, don't check the
C        segment addresses.
C
         CALL T_CRCKDS ( SEGNO, INST, TBEGS(SEGNO), TENDS(SEGNO),
     .                   XDESCR(1,SEGNO), .TRUE.                      )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,SEGNO), DSCSIZ-1,
     .                  0.D0,   OK                                    )

      END DO


C
C     Try some cases where the re-use interval matches the selected
C     segment's coverage interval.
C
      CALL TCASE ( 'Search w/o buffering case, selected segment is ' //
     .             'in dumped list, coverage interval matches '      //
     .             're-use interval, request time is in center of '  //
     .             're-use interval.'                                )

C
C     Set up the case by unloading the currently loaded CKs.  Load
C     CK 1 and look up a state from it.  Then load CK 6.
C

C
C     Unload the CK files.
C
      DO I = 1, NCK-1
         CALL T_CUF ( HNDLES(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END DO

C
C     Load CK 1 and look up a state from this file.
C
      CALL T_CLF ( CKS(1), HNDLES(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      INST     = 1
      TBEGS(1) = SCALE + 0.D0
      TENDS(1) = SCALE + 1.D0
      CKNO     = 1
      SEGNO    = 1

      XSEGID(1) = 'File: # Segno: #'
      CALL REPMC  ( XSEGID(1), '#', CKS(1), XSEGID(1) )
      CALL REPMI  ( XSEGID(1), '#', 1,       XSEGID(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CBS  ( INST, TBEGS(SEGNO)+0.5D0, TOL0, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )


C     Now load CK 6.  Look up a state from segment 9, where the
C     request time is to the right of a segment whose right endpoint
C     is at the left endpoint of the re-use interval.
C
      CALL T_CLF  ( CKS(6), HNDLES(6) )
      CALL CHCKXC ( .FALSE., ' ', OK  )

      CKNO   =  6
      INST   =  1
      SEGNO  =  9


      TBEGS(SEGNO) = CKNO * SCALE + SEGNO + 1
      TENDS(SEGNO) = TBEGS(SEGNO)  + 1

      T            = TBEGS(SEGNO) + 0.25D0

      CALL T_CBS  ( INST, T, TOL0, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     In this case, segment 9 should match.
C
      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )

      SEGNO = 9

      XSEGID(1) = 'File: # Segno: #'
      CALL REPMC  ( XSEGID(1), '#', CKS(6), XSEGID(1) )
      CALL REPMI  ( XSEGID(1), '#', SEGNO,  XSEGID(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(CKNO), 0, OK )
      CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(1),       OK )

C
C     Check the descriptor as well.  However, don't check the
C     segment addresses.
C
      CALL T_CRCKDS ( SEGNO, INST, TBEGS(SEGNO), TENDS(SEGNO),
     .                XDESCR(1,SEGNO), .TRUE.                       )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,SEGNO), DSCSIZ-1,
     .               0.D0,   OK                                     )





C
C     Create a situation where the segment list for instrument 1
C     contributed by CK 6 gets dumped, and where the request is
C     satisfied by a segment in CK 1.
C
      CALL TCASE ( 'Dump segment list from CK 6; find segment '//
     .             'for instrument 1 in CK 1.'                    )

      CALL T_CLF  ( CKS(1), HNDLES(1) )
      CALL CHCKXC ( .FALSE., ' ', OK  )

      CALL T_CLF  ( CKS(6), HNDLES(6) )
      CALL CHCKXC ( .FALSE., ' ', OK  )

      INST     = 1
      TBEGS(1) = SCALE
      TENDS(1) = SCALE + 1.D0
      T        =  0.5D0 * (  TBEGS(1) + TENDS(1)  )

      CALL T_CBS  ( INST, T, TOL0, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )

C
C     Check handle, segment descriptor and ID.
C
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(1), 0, OK )

      CALL T_CRCKDS ( 1, INST, TBEGS(1), TENDS(1), XDESCR(1,1), .TRUE. )
      CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,1), DSCSIZ-1,
     .               0.D0,  OK                                        )


      XSEGID(1) = 'File: # Segno: #'
      CALL REPMC  ( XSEGID(1), '#', CKS(1), XSEGID(1) )
      CALL REPMI  ( XSEGID(1), '#', 1,       XSEGID(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(1), OK )



      CALL TCASE ( 'Dump segment list from CK 6.  While searching '//
     .             'list for segment for instrument 1, make lower '//
     .             'bound of re-use interval match lower bound of '//
     .             'segment descriptor.'                              )


C
C     Make CK 1 higher priority than CK 6.
C
      CALL T_CLF  ( CKS(1), HNDLES(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Place request time in the "hole" between segments STSIZE+1 and
C     STSIZE+3.
C
      I          = STSIZE+1

      TBEGS(I-1) = CKNO * SCALE + I
      TENDS(I-1) = TBEGS(I-1)   + 1.0D0

      TBEGS(I)   = CKNO * SCALE + I - 1
      TENDS(I)   = TBEGS(I)         + 1

      TBEGS(I+1) = TBEGS(I)
      TENDS(I+1) = TENDS(I)

      TBEGS(I+2) = TENDS(I)          + 1
      TENDS(I+2) = TBEGS(I+2)        + 1

      T          = TBEGS(I-1) + 0.5D0

      CALL T_CBS  ( INST, T, TOL0, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     In this case, segment STSIZE should match.
C
      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )

      SEGNO = STSIZE

      XSEGID(1) = 'File: # Segno: #'
      CALL REPMC  ( XSEGID(1), '#', CKS(6), XSEGID(1) )
      CALL REPMI  ( XSEGID(1), '#', SEGNO,  XSEGID(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(CKNO), 0, OK )
      CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(1),       OK )

C
C     Check the descriptor as well.  However, don't check the
C     segment addresses.
C
      I          = SEGNO + 1
      TBEGS(I-1) = CKNO * SCALE + I
      TENDS(I-1) = TBEGS(I-1)   + 1.0D0

      CALL T_CRCKDS ( SEGNO, INST, TBEGS(SEGNO), TENDS(SEGNO),
     .                XDESCR(1,SEGNO), .TRUE.                  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,SEGNO), DSCSIZ-1,
     .               0.D0,  OK                                        )




C
C     Check correct handling of re-use intervals.  Create a new
C     CK file that contains coverage that exemplifies the various
C     masking possibilities that may occur.
C
      CALL TCASE ( 'Check re-use for a 1-instrument segment list.' )

      CKNO = 8

C
C     Segment 1:
C
      INST      = 1
      IDS(1)    = INST

      TBEGS(1) = CKNO * SCALE
      TENDS(1) = TBEGS(1) + 1.D0


C
C     Segments 2-3:
C
      INST      = 2
      IDS(2)    = INST
      IDS(3)    = INST


      TBEGS(3) = CKNO * SCALE
      TENDS(3) = TBEGS(3) + 1.D0

      TBEGS(2) = TENDS(3) + 1.D0
      TENDS(2) = TBEGS(2) + 1.D0


C
C     Segments 4-6:
C
      INST      = 3
      IDS(4)    = INST
      IDS(5)    = INST
      IDS(6)    = INST

      TBEGS(6) = CKNO * SCALE
      TENDS(6) = TBEGS(6) + 3.D0

      TBEGS(5) = TENDS(6) - 1.D0
      TENDS(5) = TBEGS(5) + 3.D0

      TBEGS(4) = TBEGS(5) + 1.D0
      TENDS(4) = TENDS(5) - 1.D0


C
C     Segments 7-9:
C
      INST     = 4
      IDS(7)   = INST
      IDS(8)   = INST
      IDS(9)   = INST

      TBEGS(9) = CKNO * SCALE
      TENDS(9) = TBEGS(9) + 3.D0

      TBEGS(8) = TBEGS(9)
      TENDS(8) = TENDS(9)

      TBEGS(7) = TBEGS(9) - 2.D0
      TENDS(7) = TBEGS(9) + 3.D0

C
C     Segments 10-12:
C
      INST      = 5
      IDS(10)   = INST
      IDS(11)   = INST
      IDS(12)   = INST

      TBEGS(12) = CKNO * SCALE
      TENDS(12) = TBEGS(12) + 3.D0

      TBEGS(11) = TBEGS(12) - 2.D0
      TENDS(11) = TBEGS(11) + 3.D0

      TBEGS(10) = TBEGS(11) - 2.D0
      TENDS(10) = TENDS(12) + 1.D0

C
C     Segments 13-14:
C
      INST      = 6
      IDS(13)   = INST
      IDS(14)   = INST

C
C     Singleton segment:
C
      TBEGS(13) = CKNO * SCALE
      TENDS(13) = TBEGS(13)

C
C     Invisible segment:
C
      TBEGS(14) = TENDS(13) + 3.D0
      TENDS(14) = TBEGS(14) - 1.D0

C
C     Three more segments for instrument 4:
C
      IDS(15)   =  4
      IDS(16)   =  4
      IDS(17)   =  4

      TBEGS(15) =  CKNO * SCALE + 10.D0
      TENDS(15) =  TBEGS(15)    +  3.D0

      TBEGS(16) =  TBEGS(15) + 1.D0
      TENDS(16) =  TENDS(15) - 1.D0

      TBEGS(17) =  TBEGS(16)
      TENDS(17) =  TENDS(16)

C
C     Three more segments for instrument 5:
C
      INST      = 5
      IDS(18)   =  INST
      IDS(19)   =  INST
      IDS(20)   =  INST

      TBEGS(20) =  CKNO * SCALE + 10.D0
      TENDS(20) =  TBEGS(20)    +  3.D0

      TBEGS(19) = TBEGS(20) - 2.D0
      TENDS(19) = TBEGS(19) + 3.D0

      TBEGS(18) = TBEGS(19) - 2.D0
      TENDS(18) = TENDS(20) + 1.D0


C
C     Create a sequence of segments for instrument 6 with the
C     following topology:
C
C
C              +++++++           segment 21
C                    +++++++             22
C        +++++++                         23
C
C
      INST      =  6
      IDS(21)   =  INST
      IDS(22)   =  INST
      IDS(23)   =  INST

      TBEGS(21) =  CKNO * SCALE + 10.D0
      TENDS(21) =  TBEGS(21)     +  3.D0

      TBEGS(22) =  TENDS(21)
      TENDS(22) =  TBEGS(21) + 3.D0

      TBEGS(23) =  TBEGS(21) - 3.D0
      TENDS(23) =  TBEGS(21)


C
C     Create the ninth CK, which is just a copy of the 8th, except
C     for descriptors and segment IDs.
C
      CKNO = 9

      DO SEGNO = 1, NSEG(9)

         CALL T_CRCKDS (  SEGNO,           IDS(SEGNO),
     .                    TBEGS(SEGNO),    TENDS(SEGNO),
     .                    XDESCR(1,SEGNO), .TRUE.        )

         XSEGID(SEGNO) = 'File: # Segno: #'
         CALL REPMC  ( XSEGID(SEGNO), '#', CKS(CKNO), XSEGID(SEGNO) )
         CALL REPMI  ( XSEGID(SEGNO), '#', SEGNO,     XSEGID(SEGNO) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL T_CRDAF ( 'CK',  CKS(9),  NSEG(9),
     .                IDS,   TBEGS,  TENDS,   XSEGID )



C
C     Create the segment descriptors and segment identifiers for
C     this CK file.
C
      CKNO = 8

      DO SEGNO = 1, NSEG(8)

         CALL T_CRCKDS ( SEGNO,           IDS(SEGNO),
     .                   TBEGS(SEGNO),    TENDS(SEGNO),
     .                   XDESCR(1,SEGNO), .TRUE.        )

         XSEGID(SEGNO) = 'File: # Segno: #'
         CALL REPMC ( XSEGID(SEGNO), '#', CKS(CKNO), XSEGID(SEGNO) )
         CALL REPMI ( XSEGID(SEGNO), '#', SEGNO,     XSEGID(SEGNO) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

C
C     Unload the other CK files.  Create and load the CK file.
C
C
C     Unload the CK files.  Again.
C
      DO I = 1, CKNO-1
         CALL T_CUF ( HNDLES(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END DO

      CALL T_CRDAF ( 'CK',  CKS(8),  NSEG(8),
     .                IDS,   TBEGS,  TENDS,   XSEGID )



C      CALL BYEBYE ( 'SUCCESS' )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CLF  ( CKS(8), HNDLES(8) )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Time for tests.
C
C
C     Make sure we can re-use data from the first segment for
C     instrument 1.
C
      CKNO  = 8
      INST  = IDS(1)
      T     = 0.5D0 * (  TBEGS(1) + TENDS(1)  )

      DO I = 1, 3


         CALL T_CBS  ( INST, T, TOL0, .TRUE. )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )

C
C        Check handle, segment descriptor and ID.
C
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(8), 0, OK )

         CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,1), DSCSIZ-1,
     .                  0.D0,  OK                                     )

         CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(1), OK )

      END DO


      T = TBEGS(1) - 1.D0


      CALL T_CBS  ( INST, T, TOL0, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND',  FOUND, .FALSE., OK )


      T = TENDS(1) + 1.D0

      CALL T_CBS  ( INST, T, TOL0, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND',  FOUND, .FALSE., OK )


      T = TBEGS(1)

      CALL T_CBS  ( INST, T, TOL0, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )


      T = TENDS(1)

      CALL T_CBS  ( INST, T, TOL0, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

C
C     Check out behavior for coverage consisting of two non-overlapping
C     segments.  The coverage topology is as follows:
C
C
C                      ++++++++++    segment 2
C        +++++++++++                         3
C
C
C
      CALL TCASE ( 'Coverage is union of two disjoint intervals. '//
     .             'Test re-use of each.'                           )



      INST  = IDS(2)
      T     = 0.5D0 * (  TBEGS(2) + TENDS(2)  )

      DO I = 1, 3

         CALL T_CBS  ( INST, T, TOL0, .TRUE. )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )

C
C        Check handle, segment descriptor and ID.
C
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(8), 0, OK )

         CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,2), DSCSIZ-1,
     .                  0.D0,  OK                                    )

         CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(2), OK )

      END DO


      T = 0.5D0 * (  TBEGS(3) + TENDS(3)  )

      DO I = 1, 3

         CALL T_CBS  ( INST, T, TOL0, .TRUE. )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )

C
C        Check handle, segment descriptor and ID.
C
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(8), 0, OK )

         CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,3), DSCSIZ-1,
     .                  0.D0,  OK                                    )

         CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(3), OK )

      END DO

C
C     Hit the endpoints of the left interval.
C
      T  = TBEGS(3)

      CALL T_CBS  ( INST, T, TOL0, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )

C
C     Check handle, segment descriptor and ID.
C
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(8), 0, OK )

      CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,3), DSCSIZ-1,
     .               0.D0,  OK                                    )

      CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(3), OK )


      T  = TENDS(3)


      CALL T_CBS  ( INST, T, TOL0, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )

C
C     Check handle, segment descriptor and ID.
C
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(8), 0, OK )

      CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,3), DSCSIZ-1,
     .               0.D0,  OK                                    )

      CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(3), OK )




C
C     Segments 4-6:
C
C
C     Check out behavior for coverage consisting of three segments
C     whose coverage is as shown:
C
C
C                 +++++++          segment 4
C              +++++++++++++               5
C        +++++++++++                       6
C
C
      CALL TCASE ( 'Segments 4-6:  three-segment overlapping case #1.' )



      INST  = IDS(5)
      T     = TENDS(6) + 0.25D0

      DO I = 1, 3

         CALL T_CBS  ( INST, T, TOL0, .TRUE. )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )

C
C        Check handle, segment descriptor and ID.
C
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(8), 0, OK )

         CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,5), DSCSIZ-1,
     .                  0.D0,  OK                                   )

         CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(5), OK )

      END DO


      INST  = IDS(4)
      T     = TBEGS(6) + 0.25D0

      DO I = 1, 3

         CALL T_CBS  ( INST, T, TOL0, .TRUE. )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )

C
C        Check handle, segment descriptor and ID.
C
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(8), 0, OK )

         CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,6), DSCSIZ-1,
     .                  0.D0,  OK                                   )

         CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(6), OK )

      END DO




      T = TBEGS(5) + 0.25D0

      CALL T_CBS  ( INST, T, TOL0, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )


      T = TBEGS(6) - 0.25D0

      CALL T_CBS  ( INST, T, TOL0, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND',  FOUND, .FALSE., OK )



C
C     Segments 7-9:
C
C
C     Check out behavior for coverage consisting of three segments
C     whose coverage is as shown:
C
C        +++++++++++           segment 7
C             +++++++++++              8
C             +++++++++++              9
C
      CALL TCASE ( 'Segments 7-9:  three-segment overlapping case #2.' )

C
C     Get the right side of the re-use interval to coincide with
C     the left endpoint of a descriptor, where T lies to the left
C     of the segment, in the CHECK LIST state:
C
      INST  = IDS(7)
      T     = TBEGS(7) + 0.25D0

      CALL T_CBS  ( INST, T, TOL0, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )

      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(8), 0, OK )

      CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,7), DSCSIZ-1,
     .               0.D0,  OK                                         )

      CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(7), OK )





C
C     Check out behavior for coverage consisting of three segments
C     whose coverage is as shown:
C
C
C       ++++++++++++++++++        segment 10
C           +++++++                       11
C               ++++++++                  12
C
C
      CALL TCASE ( 'Three-segment overlapping case #2.' )



      INST  = IDS(10)
      T     = TENDS(12) + 0.25D0

      DO I = 1, 3

         CALL T_CBS  ( INST, T, TOL0, .TRUE. )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )

C
C        Check handle, segment descriptor and ID.
C
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(8), 0, OK )

         CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,10), DSCSIZ-1,
     .                  0.D0,  OK                                   )

         CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(10), OK )

      END DO


      T = TENDS(10) + 1.D0

      CALL T_CBS  ( INST, T, TOL0, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND',  FOUND, .FALSE., OK )



      T = TBEGS(10) + 0.25D0

      DO I = 1, 3

         CALL T_CBS  ( INST, T, TOL0, .TRUE. )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )

C
C        Check handle, segment descriptor and ID.
C
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(8), 0, OK )

         CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,10), DSCSIZ-1,
     .                  0.D0,  OK                                  )

         CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(10), OK )

      END DO


      T = TBEGS(11) - 0.25D0

      CALL T_CBS  ( INST, T, TOL0, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )

      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(8), 0, OK )

      CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,10), DSCSIZ-1,
     .               0.D0,  OK                                         )

      CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(10), OK )



C
C     Check out behavior for coverage consisting of three segments
C     whose coverage is as shown:
C
C
C       ++++++++++++++++++        segment 15
C            +++++++                      16
C            +++++++                      17
C
C
      CALL TCASE ( 'T > segment upper bound.  Lower bound of '//
     .             're-use interval = upper bound of segment.'   )



      INST  = IDS(15)
      T     = TENDS(17) + 0.5D0

      CALL T_CBS  ( INST, T, TOL0, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )

C
C     Check handle, segment descriptor and ID.
C
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(8), 0, OK )

      CALL T_CHDS ( 'DESCR',  DESCR,  '=', XDESCR(1,15), DSCSIZ-1,
     .               0.D0,  OK                                       )

      CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(15), OK )


C
C     Check out behavior for coverage consisting of three segments
C     whose coverage is as shown:
C
C
C              +++++++           segment 21
C                    +++++++             22
C        +++++++                         23
C
C

      CALL TCASE ( 'T is in segment.  Lower bound of '        //
     .             're-use interval = lower bound of segment.'   )

      INST      =  6
      IDS(21)   =  INST
      IDS(22)   =  INST
      IDS(23)   =  INST

      TBEGS(21) =  CKNO * SCALE + 10.D0
      TENDS(21) =  TBEGS(21)     +  3.D0

      TBEGS(22) =  TENDS(21)
      TENDS(22) =  TBEGS(21) + 3.D0

      TBEGS(23) =  TBEGS(21) - 3.D0
      TENDS(23) =  TBEGS(21)



      INST  = IDS(21)
      T     = TBEGS(21) + 0.5D0

      CALL T_CBS  ( INST, T, TOL0, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )

C
C     Check handle, segment descriptor and ID.
C
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(8), 0, OK )

      CALL T_CHDS ( 'DESCR',  DESCR,  '=', XDESCR(1,21), DSCSIZ-1,
     .               0.D0,  OK                                        )

      CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(21), OK )


C
C     Check out behavior for coverage consisting singleton and
C     invisible segments.
C
C
      CALL TCASE ( 'Look up data from a singleton segment.' )

      T    = TBEGS(13)
      INST = IDS(13)

      CALL T_CBS  ( INST, T, TOL0, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )

      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(8), 0, OK )

      CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,13), DSCSIZ-1,
     .               0.D0,  OK                                         )

      CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(13), OK )


C
C     Exercise the logic for handling singleton and invisible
C     segments during a NEW INST search.
C
      CALL TCASE ( 'Look up data from a singleton segment, this time '//
     .             'in a NEW SEGMENTS search.'                       )

      CALL T_CLF ( CKS(9), HNDLES(9) )
      CALL CHCKXC ( .FALSE., ' ', OK )


      SEGNO = 13
      CKNO =  9

      CALL T_CRCKDS ( SEGNO,             IDS(SEGNO),
     .                TBEGS(SEGNO),      TENDS(SEGNO),
     .                XDESCR(1,SEGNO),  .TRUE.         )

      XSEGID(SEGNO) = 'File: # Segno: #'
      CALL REPMC ( XSEGID(SEGNO), '#', CKS(CKNO), XSEGID(SEGNO) )
      CALL REPMI ( XSEGID(SEGNO), '#', SEGNO,       XSEGID(SEGNO) )
      CALL CHCKXC ( .FALSE., ' ', OK )


      T    = TBEGS(13)
      INST = IDS(13)

      CALL T_CBS  ( INST, T, TOL0, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )

      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(9), 0, OK )

      CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,13), DSCSIZ-1,
     .               0.D0,  OK                                         )

      CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(13), OK )





      CALL TCASE ( 'Prepare for search w/o buffering tests: '//
     .             'create a CK with STSIZE segments for '   //
     .             'instruments 1-NINST.'                     )

C
C     Create a CK file with STSIZE segments for instruments 1-NINST.
C
      CKNO = 10

      DO INST = 1, NINST

         DO I = 1, STSIZE

            J         = (INST-1)*STSIZE + I
            IDS(J)    = INST

            IF ( INST .EQ. NINST ) THEN
               TBEGS(J)  = CKNO * SCALE - I - 1
            ELSE
               TBEGS(J)  = CKNO * SCALE + I - 1
            END IF

            TENDS(J)  = TBEGS(J) + 1

            XSEGID(J) = 'File: # Segno: #  Inst:  #'

            CALL REPMC ( XSEGID(J), '#', CKS(CKNO),   XSEGID(J) )
            CALL REPMI ( XSEGID(J), '#', J,           XSEGID(J) )
            CALL REPMI ( XSEGID(J), '#', INST,        XSEGID(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END DO

      CALL T_CRDAF ( 'CK',  CKS(CKNO),  NSEG(CKNO),
     .                IDS,   TBEGS,     TENDS,       XSEGID )

      CALL CHCKXC ( .FALSE., ' ', OK )



      CALL TCASE ( 'Prepare for search w/o buffering tests: '//
     .             'create a CK with STSIZE-3 segments for ' //
     .             'instruments 1-NINST.'                     )

C
C     Create a CK file with STSIZE-3 segments for instruments
C     1-NINST.
C

      CKNO = 11

      DO INST = 1, NINST

         DO I = 1, STSIZE-3

            J         = (INST-1)*(STSIZE-3) + I
            IDS(J)    = INST

            IF ( INST .EQ. NINST ) THEN
               TBEGS(J)  = CKNO * SCALE - I - 1
            ELSE
               TBEGS(J)  = CKNO * SCALE + I - 1
            END IF

            TENDS(J)  = TBEGS(J)          + 1

            XSEGID(J) = 'File: # Segno: #  Inst:  #'

            CALL REPMC ( XSEGID(J), '#', CKS(CKNO),   XSEGID(J) )
            CALL REPMI ( XSEGID(J), '#', J,           XSEGID(J) )
            CALL REPMI ( XSEGID(J), '#', INST,        XSEGID(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END DO

      CALL T_CRDAF ( 'CK',  CKS(CKNO),  NSEG(CKNO),
     .                IDS,  TBEGS,      TENDS,       XSEGID )

      CALL CHCKXC ( .FALSE., ' ', OK )




      CALL TCASE ( 'Search w/o buffering, T < segment begin, ' //
     .             're-use interval right endpoint < segment '  //
     .             'begin.'                                     )


C
C     Unload the CK files.  Again.
C
      DO I = 1, NCK
         CALL T_CUF ( HNDLES(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END DO

C
C     Load CKs 8 and 10.
C
      CALL T_CLF ( CKS(8),  HNDLES(8)  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CLF ( CKS(10), HNDLES(10) )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     The request time should precede the coverage of segment 3 in
C     CK 8.
C
      INST = 2
      T    = 8 * SCALE  - 1.D0

      CALL T_CBS  ( INST, T, TOL0, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND',  FOUND, .FALSE., OK )



      CALL TCASE ( 'Search w/o buffering, T within segment, ' //
     .             're-use interval, left endpoint > segment ' //
     .             'begin.'                                     )



C
C     The request time should precede the coverage of segment 3 in
C     CK 8.
C
      INST  = 3
      SEGNO = 5
      CKNO  = 8

      TBEGS(6) = CKNO * SCALE
      TENDS(6) = TBEGS(6) + 3.D0

      TBEGS(5) = TENDS(6) - 1.D0
      TENDS(5) = TBEGS(5) + 3.D0

      T        = CKNO * SCALE  + 4.D0

      CALL T_CBS  ( INST, T, TOL0, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )

      XSEGID(SEGNO) = 'File: # Segno: #'
      CALL REPMC  ( XSEGID(SEGNO), '#', CKS(CKNO), XSEGID(SEGNO) )
      CALL REPMI  ( XSEGID(SEGNO), '#', SEGNO,     XSEGID(SEGNO) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(SEGNO),    OK )

C
C     Check the descriptor as well.  However, don't check the
C     segment addresses.
C
      CALL T_CRCKDS ( SEGNO,           INST,
     .                TBEGS(SEGNO),    TENDS(SEGNO),
     .                XDESCR(1,SEGNO), .TRUE.        )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,SEGNO), DSCSIZ-1,
     .               0.D0,   OK                                    )



      CALL TCASE ( 'Search w/o buffering, T < segment begin, ' //
     .             're-use interval right endpoint = segment '  //
     .             'begin.'                                     )
      INST     = 4
      SEGNO    = 7

      IDS(7)   = INST
      IDS(8)   = INST
      IDS(9)   = INST

      TBEGS(9) = CKNO * SCALE
      TENDS(9) = TBEGS(9) + 3.D0

      TBEGS(8) = TBEGS(9)
      TENDS(8) = TENDS(9)

      TBEGS(7) = TBEGS(9) - 2.D0
      TENDS(7) = TBEGS(9) + 3.D0

      T        = TBEGS(8) - 1.D0


      CALL T_CBS  ( INST, T, TOL0, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )

      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(8), 0, OK )

      XSEGID(SEGNO) = 'File: # Segno: #'
      CALL REPMC ( XSEGID(SEGNO), '#', CKS(CKNO), XSEGID(SEGNO) )
      CALL REPMI ( XSEGID(SEGNO), '#', SEGNO,     XSEGID(SEGNO) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(SEGNO),    OK )

C
C     Check the descriptor as well.  However, don't check the
C     segment addresses.
C
      CALL T_CRCKDS ( SEGNO,           INST,
     .                TBEGS(SEGNO),    TENDS(SEGNO),
     .                XDESCR(1,SEGNO), .TRUE.        )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,SEGNO), DSCSIZ-1,
     .               0.D0,   OK                                    )

C
C     Some cases where a partial list must be dumped:
C
      CALL TCASE ( 'Dump segment list from CK 11.  While searching '  //
     .             'list for segment for instrument 4, make '         //
     .             'upper bound of re-use interval < upper bound of ' //
     .             'segment descriptor.'                               )


C
C     Unload CK 10; load CK 11.
C
      CALL T_CUF ( HNDLES(10) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CLF ( CKS(11), HNDLES(11) )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Our request time should be in the interior of segment 15 in
C     CK 8.
C
      CKNO      = 8
      SEGNO     = 15

      IDS(15)   =  4
      IDS(16)   =  4
      IDS(17)   =  4

      TBEGS(15) =  CKNO * SCALE + 10.D0
      TENDS(15) =  TBEGS(15)     +  3.D0

      TBEGS(16) =  TBEGS(15) + 1.D0
      TENDS(16) =  TENDS(15) - 1.D0

      TBEGS(17) =  TBEGS(16)
      TENDS(17) =  TBEGS(17)

      T         =  TBEGS(15) + 0.5D0

      CALL T_CBS  ( INST, T, TOL0, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )

      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(8), 0, OK )

      XSEGID(SEGNO) = 'File: # Segno: #'
      CALL REPMC ( XSEGID(SEGNO), '#', CKS(CKNO), XSEGID(SEGNO) )
      CALL REPMI ( XSEGID(SEGNO), '#', SEGNO,       XSEGID(SEGNO) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(SEGNO),    OK )

C
C     Check the descriptor as well.  However, don't check the
C     segment addresses.
C
      CALL T_CRCKDS ( SEGNO,           INST,
     .                TBEGS(SEGNO),    TENDS(SEGNO),
     .                XDESCR(1,SEGNO), .TRUE.         )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,SEGNO), DSCSIZ-1,
     .               0.D0,   OK                                    )



      CALL TCASE ( 'Dump segment list from CK 11.  While searching ' //
     .             'list for segment for instrument 4, make lower '  //
     .             'bound of re-use interval = upper bound of '      //
     .             'segment descriptor.'                              )


      CKNO     = 8
      INST     = 4
      TBEGS(9) = CKNO * SCALE
      TENDS(9) = TBEGS(9) + 3.D0
      T        = TENDS(9) + 0.5D0

      CALL T_CBS  ( INST, T, TOL0, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND',  FOUND, .FALSE., OK )


      CALL TCASE ( 'Dump segment list from CK 11.  While searching ' //
     .             'list for segment for instrument 5, make lower '  //
     .             'bound of re-use interval > lower bound of '      //
     .             'segment descriptor.'                              )


      INST      =  5
      IDS(18)   =  INST
      IDS(19)   =  INST
      IDS(20)   =  INST

      TBEGS(20) =  CKNO * SCALE + 10.D0
      TENDS(20) =  TBEGS(20)     +  3.D0

      TBEGS(19) =  TBEGS(20) - 2.D0
      TENDS(19) =  TBEGS(19) + 3.D0

      TBEGS(18) =  TBEGS(19) - 2.D0
      TENDS(18) =  TENDS(20) + 1.D0

      T         =  TENDS(18) - 0.5D0



      CALL T_CBS  ( INST, T, TOL0, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )

      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(8), 0, OK )

      SEGNO = 18

      XSEGID(SEGNO) = 'File: # Segno: #'
      CALL REPMC  ( XSEGID(SEGNO), '#', CKS(CKNO), XSEGID(SEGNO) )
      CALL REPMI  ( XSEGID(SEGNO), '#', SEGNO,     XSEGID(SEGNO) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(SEGNO), OK )

C
C     Check the descriptor as well.  However, don't check the
C     segment addresses.
C
      CALL T_CRCKDS ( SEGNO,            INST,
     .                TBEGS(SEGNO),     TENDS(SEGNO),
     .                XDESCR(1,SEGNO),  .TRUE.       )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,SEGNO), DSCSIZ-1,
     .               0.D0,   OK                                     )



      CALL TCASE ( 'Create a situation where room is needed in the '//
     .             'instrument table, and the second instrument '   //
     .             'list has expense greater than the first.'        )

C
C     Unload CKs 8 and 11.
C
      CALL T_CUF  ( HNDLES(8)   )
      CALL T_CUF  ( HNDLES(11)  )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Fill up (nearly) the segment table with a cheap list for
C     instrument 2 and an expensive list for instrument 1.
C
      CALL T_CLF  ( CKS(8), HNDLES(8) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      INST      = 2
      IDS(2)    = INST
      IDS(3)    = INST


      TBEGS(3) = CKNO * SCALE
      TENDS(3) = TBEGS(3) + 1.D0

      INST     = 2
      CKNO     = 8
      SEGNO    = 3

      CALL T_CBS  ( INST, TBEGS(SEGNO)+0.5D0, TOL0, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     In this case, the segment should be found.  Make sure we get
C     back the right handle and segment identifier.
C
      CALL CHCKSL ( 'FOUND',  FOUND,      .TRUE.,           OK )
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(CKNO), 0, OK )


      CALL T_CLF  ( CKS(11), HNDLES(11) )
      CALL CHCKXC ( .FALSE., ' ', OK    )

      INST     = 1
      CKNO     = 11
      SEGNO    = 1
      I        = 1
      TBEGS(I) = CKNO * SCALE + I - 1
      TENDS(I) = TBEGS(I)         + 1

      CALL T_CBS  ( INST, TBEGS(SEGNO)+0.5D0, TOL0, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND',  FOUND,      .TRUE.,           OK )
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(CKNO), 0, OK )

C
C     Now do a look up for instrument 3.  This should cause the segment
C     lists for instruments 2 and 1 to get dumped.
C
      INST      = 3
      CKNO      = 11

      I         = 1
      J         = (INST-1)*(STSIZE-3) + I

      TBEGS(J)  = CKNO * SCALE + I - 1
      TENDS(J)  = TBEGS(I)          + 1

      XSEGID(J) = 'File: # Segno: #  Instrument:  #'

      CALL REPMC  ( XSEGID(I), '#', CKS(CKNO), XSEGID(I) )
      CALL REPMI  ( XSEGID(I), '#', J,         XSEGID(I) )
      CALL REPMI  ( XSEGID(I), '#', INST,      XSEGID(I) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SEGNO = J

      CALL T_CBS  ( INST, TBEGS(SEGNO)+0.5D0, TOL0, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND',  FOUND,      .TRUE.,           OK )
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(CKNO), 0, OK )




      CALL TCASE ( 'Try DAFOPR error handling.'  )

      CALL T_CLF ( 'ThisFileDoesNotExist', HANDLE )

      IF ( RETURN() ) THEN
         CALL CHCKXC ( .TRUE., 'SPICE(FILENOTFOUND)', OK )
      ELSE
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF




      CALL TCASE  ( 'Test partial deletion of a segment list when '//
     .              'a file is unloaded.'                            )

C
C     Unload the CK files.  The load files 1 and 2.
C
      DO I = 1, NCK

         CALL T_CUF  ( HNDLES(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      DO I = 1, 2
         CALL T_CLF  ( CKS(I), HNDLES(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END DO

C
C     Do lookups for instrument 1 that hit both files.
C
      INST     = 1
      TBEGS(1) = SCALE + 0.D0

      CALL T_CBS  ( INST, TBEGS(1)+0.5D0, TOL0, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )


      INST         = 1
      CKNO         = 2

      SEGNO        = NSEG(CKNO)/2  +  1

      TBEGS(SEGNO) = CKNO * SCALE + SEGNO - 1

      CALL T_CBS  ( INST, TBEGS(SEGNO)+0.5D0, TOL0, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )

C
C     Do a lookup for instrument 2 to create a segment list for that
C     instrument.
C
      INST         = 2
      CKNO         = 2

      SEGNO        = NSEG(CKNO)/2

      TBEGS(SEGNO) = CKNO * SCALE + SEGNO - 1


      CALL T_CBS  ( INST, TBEGS(SEGNO)+0.5D0, TOL0, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )


C
C     Reload file 1, removing the portion of instrument 1's segment list
C     that came from file 1, as part of the unload process that
C     precedes re-loading file 1.
C
      CALL T_CLF  ( CKS(1), HNDLES(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create FTSIZE copies of CK 1 and load FTSIZE-1 of them.  We
C     should get a file table overflow error.
C
      CALL TCASE ( 'File table overflow error.' )


      DO I = 1, FTSIZE

         CKCPY(I) = 'copy#.bc'
         CALL REPMI ( CKCPY(I), '#', I, CKCPY(I) )

         INST     = 1
         TBEGS(1) = SCALE + 0.D0
         TENDS(1) = SCALE + 1.D0
         CKNO     = 1
         SEGNO    = 1

         XSEGID(1) = 'File: # Segno: #'
         CALL REPMC ( XSEGID(1), '#', CKCPY(I), XSEGID(1) )
         CALL REPMI ( XSEGID(1), '#', SEGNO,    XSEGID(1) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL T_CRDAF ( 'CK',   CKCPY(I),   NSEG(1),
     .                   INST,  TBEGS,      TENDS,   XSEGID )
      END DO


      DO I = 1, FTSIZE-2
         CALL T_CLF  ( CKCPY(I), CPYHAN(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END DO

      CALL T_CLF  ( CKCPY(FTSIZE-1), CPYHAN(FTSIZE-1) )
C
C     If the error is caught by the handle manager, the short message
C     check should be:
C
C        CALL CHCKXC ( .TRUE., 'SPICE(FTFULL)', OK )
C
C     Since the BSR file table is smaller than the handle manager
C     table, the error check is as shown below:
C
      CALL CHCKXC ( .TRUE., 'SPICE(CKTOOMANYFILES)', OK )


C
C     Loading, unloading, and priority checks:
C
      CALL TCASE ( 'Load all copies of CK 1, looking up the same ' //
     .             'state from each.  Unload the files in reverse ' //
     .             'order.  Repeat 3 times.'                         )

C
C     First, make sure all files are unloaded.
C
      DO I = 1, NCK

         CALL T_CUF  ( HNDLES(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      DO I = 1, FTSIZE-1

         CALL T_CUF  ( CPYHAN(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


      INST = 1

      DO I = 1, 3

         DO J = 1, FTSIZE

            TBEGS(J) = SCALE + 0.D0
            TENDS(J) = SCALE + 1.D0

            CALL T_CLF  ( CKCPY(J), CPYHAN(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            XSEGID(J) = 'File: # Segno: #'
            CALL REPMC ( XSEGID(J), '#', CKCPY(J), XSEGID(J) )
            CALL REPMI ( XSEGID(J), '#', 1,        XSEGID(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL T_CBS  ( INST, TBEGS(J)+0.5D0, TOL0, .TRUE. )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           In this case, the segment should be found.  Make sure
C           we get back the right handle and segment identifier.
C
            CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )
            CALL CHCKSI ( 'HANDLE', HANDLE, '=', CPYHAN(J), 0, OK )
            CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(J),    OK )

C
C           Check the descriptor as well.  However, don't check the
C           segment addresses.
C
            CALL T_CRCKDS ( SEGNO,        INST,
     .                      TBEGS(J),     TENDS(J),
     .                      XDESCR(1,J),  .TRUE.    )

            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,J), DSCSIZ-1,
     .                     0.D0,   OK                                )

         END DO

C
C        Now unload files, looking up states as we go.
C
         DO J = FTSIZE-1, 1, -1

            CALL T_CUF  ( CPYHAN(J+1) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            TBEGS(J) = SCALE + 0.D0
            TENDS(J) = SCALE + 1.D0

            XSEGID(J) = 'File: # Segno: #'
            CALL REPMC ( XSEGID(J), '#', CKCPY(J), XSEGID(J) )
            CALL REPMI ( XSEGID(J), '#', 1,        XSEGID(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL T_CBS  ( INST, TBEGS(J)+0.5D0, TOL0, .TRUE. )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           In this case, the segment should be found.  Make sure
C           we get back the right handle and segment identifier.
C
            CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )
            CALL CHCKSI ( 'HANDLE', HANDLE, '=', CPYHAN(J), 0, OK )
            CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(J),    OK )

C
C           Check the descriptor as well.  However, don't check the
C           segment addresses.
C
            CALL T_CRCKDS ( SEGNO,        INST,
     .                      TBEGS(J),     TENDS(J),
     .                      XDESCR(1,J),  .TRUE.    )

            CALL CHCKXC ( .FALSE., ' ', OK )


            CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,J), DSCSIZ-1,
     .                     0.D0,   OK                                )

         END DO

      END DO

C
C     Make sure we don't accumulate DAF links by re-loading a file.
C
      CALL TCASE ( 'Load the first CK file 2*FTSIZE times.' )

      DO I = 1, FTSIZE*2

         SEGNO    = 1
         TBEGS(1) = SCALE + 0.D0
         TENDS(1) = SCALE + 1.D0

         CALL T_CLF ( CKS(1), HNDLES(1) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XSEGID(1) = 'File: # Segno: #'
         CALL REPMC ( XSEGID(1), '#', CKS(1), XSEGID(1) )
         CALL REPMI ( XSEGID(1), '#', SEGNO,  XSEGID(1) )
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL T_CBS  ( INST, TBEGS(1)+0.5D0, TOL0, .TRUE. )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        In this case, the segment should be found.  Make sure
C        we get back the right handle and segment identifier.
C
         CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(1), 0, OK )
         CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(1),    OK )

C
C        Check the descriptor as well.  However, don't check the
C        segment addresses.
C

         CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,1), DSCSIZ-1,
     .                  0.D0,   OK                                )

      END DO


C
C     Tests using non-zero tolerance follow...
C     We'll make use of CK8.
C
      CALL T_CLF ( CKS(8), HNDLES(8) )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     The topology of the coverage for segments 10-12 is as shown.
C
C       ++++++++++++++++++        segment 10
C           +++++++                       11
C               ++++++++                  12
C

      CALL TCASE ( 'Request time is covered by segment 11, but '   //
     .             'tolerance allows segment 12 to cover request.' //
     .             'Repeat the lookup to test the re-use interval.' )


      CKNO  = 8
      INST  = 5
      SEGNO = 12
      TOL   = 1.5D0

      TBEGS(12) = CKNO * SCALE
      TENDS(12) = TBEGS(12) + 3.D0

      TBEGS(11) = TBEGS(12) - 2.D0
      TENDS(11) = TBEGS(11) + 3.D0

      TBEGS(10) = TBEGS(11) - 2.D0
      TENDS(10) = TENDS(12) + 1.D0

      TBEGS(SEGNO) = CKNO * SCALE
      TENDS(SEGNO) = TBEGS(SEGNO) + 3.D0

      XSEGID(SEGNO) = 'File: # Segno: #'
      CALL REPMC  ( XSEGID(SEGNO), '#', CKS(CKNO), XSEGID(SEGNO) )
      CALL REPMI  ( XSEGID(SEGNO), '#', SEGNO,     XSEGID(SEGNO) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CRCKDS ( SEGNO,         INST,
     .                TBEGS(SEGNO),  TENDS(SEGNO),
     .                XDESCR(1,1),  .TRUE.        )

      DO I = 1, 3

         CALL T_CBS  ( INST, TBEGS(SEGNO) - 1.0D0, TOL, .TRUE. )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        In this case, the segment should be found.  Make sure
C        we get back the right handle and segment identifier.
C
         CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(CKNO),  0, OK )
         CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(SEGNO),    OK )

C
C        Check the descriptor as well.  However, don't check the
C        segment addresses.
C
         CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,1), DSCSIZ-1,
     .                  0.D0,   OK                                )

      END DO


      CALL TCASE ( 'Repeat the test with tolerance too small to '  //
     .             'catch segment 12.  We should hit segment 11.'  )

      TOL   = 0.5D0
      SEGNO = 11

      XSEGID(SEGNO) = 'File: # Segno: #'
      CALL REPMC  ( XSEGID(SEGNO), '#', CKS(CKNO), XSEGID(SEGNO) )
      CALL REPMI  ( XSEGID(SEGNO), '#', SEGNO,     XSEGID(SEGNO) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CRCKDS ( SEGNO,         INST,
     .                TBEGS(SEGNO),  TENDS(SEGNO),
     .                XDESCR(1,1),  .TRUE.        )

      DO I = 1, 3

         CALL T_CBS  ( INST, TBEGS(12) - 1.0D0, TOL, .TRUE. )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )
C
C        In this case, the segment should be found.  Make sure
C        we get back the right handle and segment identifier.
C
         CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(CKNO),  0, OK )
         CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(SEGNO),    OK )

C
C        Check the descriptor as well.  However, don't check the
C        segment addresses.
C
         CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,1), DSCSIZ-1,
     .                  0.D0,   OK                                )

      END DO


      CALL TCASE ( 'Request time is covered by segment 10, but '   //
     .             'tolerance allows segment 10 to cover request.' //
     .             'Repeat the lookup to test the re-use interval.' )

      SEGNO = 11
      TOL   = 1.1D0

      DO I = 1, 3

         CALL T_CBS  ( INST, TBEGS(11) - 1.0D0, TOL, .TRUE. )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )
C
C        In this case, the segment should be found.  Make sure
C        we get back the right handle and segment identifier.
C
         CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(CKNO),  0, OK )
         CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(SEGNO),    OK )

C
C        Check the descriptor as well.  However, don't check the
C        segment addresses.
C
         CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,1), DSCSIZ-1,
     .                  0.D0,   OK                                )

      END DO

C
C     Now check handling of tolerance of the left side of the
C     request time.
C
      CALL TCASE ( 'Request time is covered by segment 10, but '   //
     .             'tolerance allows segment 12 to cover request.' //
     .             'Repeat the lookup to test the re-use interval.' )

      SEGNO = 12
      TOL   = 0.5D0

      XSEGID(SEGNO) = 'File: # Segno: #'
      CALL REPMC  ( XSEGID(SEGNO), '#', CKS(CKNO), XSEGID(SEGNO) )
      CALL REPMI  ( XSEGID(SEGNO), '#', SEGNO,     XSEGID(SEGNO) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CRCKDS ( SEGNO,         INST,
     .                TBEGS(SEGNO),  TENDS(SEGNO),
     .                XDESCR(1,1),  .TRUE.        )

      DO I = 1, 3

         CALL T_CBS  ( INST, TENDS(SEGNO) + 0.01D0, TOL, .TRUE. )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        In this case, the segment should be found.  Make sure
C        we get back the right handle and segment identifier.
C
         CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(CKNO),  0, OK )
         CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(SEGNO),    OK )

C
C        Check the descriptor as well.  However, don't check the
C        segment addresses.
C
         CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,1), DSCSIZ-1,
     .                  0.D0,   OK                                )

      END DO


      CALL TCASE ( 'Repeat the test with tolerance too small to '  //
     .             'catch segment 12.  We should hit segment 10.'  )

      SEGNO = 10
      TOL   = 0.001D0

      XSEGID(SEGNO) = 'File: # Segno: #'
      CALL REPMC  ( XSEGID(SEGNO), '#', CKS(CKNO), XSEGID(SEGNO) )
      CALL REPMI  ( XSEGID(SEGNO), '#', SEGNO,     XSEGID(SEGNO) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CRCKDS ( SEGNO,         INST,
     .                TBEGS(SEGNO),  TENDS(SEGNO),
     .                XDESCR(1,1),  .TRUE.        )

C
C     Repeat lookup to test the re-use interval.
C
      DO I = 1, 3

         CALL T_CBS  ( INST, TENDS(12) + 0.01D0, TOL, .TRUE. )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        In this case, the segment should be found.  Make sure
C        we get back the right handle and segment identifier.
C
         CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(CKNO),  0, OK )
         CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(SEGNO),    OK )

C
C        Check the descriptor as well.  However, don't check the
C        segment addresses.
C
         CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,1), DSCSIZ-1,
     .                  0.D0,   OK                                )

      END DO


C
C     Load CKs 11 and 10, in that order.  Most segments from
C     CK 10 can't be buffered.
C
      CALL T_CLF ( CKS(11), HNDLES(11) )
      CALL T_CLF ( CKS(10), HNDLES(10) )

C
C     Do a look up that will hit the last segment for inst NINST
C     in CK 10, using non-zero tolerance.
C
      CALL TCASE ( 'Do a look up that will hit the last segment for '//
     .             'instrument NINST in CK 10, relying on tolerance '//
     .             'to catch the segment.' )

      CKNO  = 10
      INST  = NINST

      I         = STSIZE
      J         = (INST-1)*STSIZE + I
      IDS(J)    = INST

      TBEGS(J)  = CKNO * SCALE - I - 1

      TENDS(J)  = TBEGS(J) + 1

      XSEGID(J) = 'File: # Segno: #  Inst:  #'

      CALL REPMC ( XSEGID(J), '#', CKS(CKNO),   XSEGID(J) )
      CALL REPMI ( XSEGID(J), '#', J,           XSEGID(J) )
      CALL REPMI ( XSEGID(J), '#', INST,        XSEGID(J) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SEGNO = J

      CALL T_CRCKDS ( SEGNO,         INST,
     .                TBEGS(SEGNO),  TENDS(SEGNO),
     .                XDESCR(1,1),  .TRUE.        )

      TOL = 0.1D0

      DO I = 1, 3

         CALL T_CBS  ( INST, TBEGS(SEGNO) - 0.01D0, TOL, .TRUE. )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        In this case, the segment should be found.  Make sure
C        we get back the right handle and segment identifier.
C
         CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(CKNO),  0, OK )
         CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(SEGNO),    OK )

C
C        Check the descriptor as well.  However, don't check the
C        segment addresses.
C
         CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,1), DSCSIZ-1,
     .                  0.D0,   OK                                )

      END DO

C
C     Do a look up that will hit the last segment for inst NINST-1
C     in CK 10, using non-zero tolerance.
C
      CALL TCASE ( 'Do a look up that will hit the last segment for '//
     .             'instrument NINST-1 in CK 10, relying on '        //
     .             'tolerance to catch the segment.' )

      CKNO  = 10
      INST  = NINST-1

      I         = STSIZE
      J         = (INST-1)*STSIZE + I
      IDS(J)    = INST

      TBEGS(J)  = CKNO * SCALE + I - 1

      TENDS(J)  = TBEGS(J) + 1

      XSEGID(J) = 'File: # Segno: #  Inst:  #'

      CALL REPMC ( XSEGID(J), '#', CKS(CKNO),   XSEGID(J) )
      CALL REPMI ( XSEGID(J), '#', J,           XSEGID(J) )
      CALL REPMI ( XSEGID(J), '#', INST,        XSEGID(J) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SEGNO = J

      CALL T_CRCKDS ( SEGNO,         INST,
     .                TBEGS(SEGNO),  TENDS(SEGNO),
     .                XDESCR(1,1),  .TRUE.        )

      TOL = 0.1D0

      DO I = 1, 3

         CALL T_CBS  ( INST, TENDS(SEGNO) + 0.01D0, TOL, .TRUE. )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        In this case, the segment should be found.  Make sure
C        we get back the right handle and segment identifier.
C
         CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(CKNO),  0, OK )
         CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(SEGNO),    OK )

C
C        Check the descriptor as well.  However, don't check the
C        segment addresses.
C
         CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,1), DSCSIZ-1,
     .                  0.D0,   OK                                )

      END DO




      CALL TCASE ( 'Make sure presence of a.v. data is not '    //
     .             'considered for lookups for which T_CBS '    //
     .             'was supplied a "need a.v." flag of .FALSE.' )

C
C     Now check that segments w/o angular velocity are handled
C     correctly.  Create a CK file with STSIZE segments for
C     instrument 1.  For N = 1, ... STSIZE/2, the segments
C     indexed 2N-1 have no angular velocity, while the segments
C     indexed 2N do have angular velocity.
C
C
C     Create a CK file with STSIZE segments for instruments 1-NINST.
C
      CKNO = 12
      INST = 1

      DO SEGNO = 1, NSEG(CKNO)

         IDS(SEGNO) = INST

         TBEGS(SEGNO)  = CKNO * SCALE + SEGNO - 1
         TENDS(SEGNO)  = TBEGS(SEGNO) + 1

         XSEGID(SEGNO) = 'File: # Segno: #  Inst:  #'

         CALL REPMC ( XSEGID(SEGNO), '#', CKS(CKNO),   XSEGID(SEGNO) )
         CALL REPMI ( XSEGID(SEGNO), '#', SEGNO,       XSEGID(SEGNO) )
         CALL REPMI ( XSEGID(SEGNO), '#', INST,        XSEGID(SEGNO) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         AVFLAG = EVEN(SEGNO)

         CALL T_CRCKDS ( SEGNO,             IDS(SEGNO),
     .                   TBEGS(SEGNO),      TENDS(SEGNO),
     .                   XDESCR(1,SEGNO),   AVFLAG         )

         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

C
C     Create the CK directly, since we want to have control over the
C     value of the a.v. flag.
C
      CALL CKOPN  ( CKS(CKNO), ' ', 0, HNDLES(CKNO) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO SEGNO = 1, NSEG(CKNO)

         CALL DAFBNA ( HNDLES(CKNO), XDESCR(1,SEGNO), XSEGID(SEGNO) )
         CALL DAFENA
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL CKCLS ( HNDLES(CKNO) )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Unload all files.
C
      DO I = 1, NCK

         CALL T_CUF  ( HNDLES(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      DO I = 1, FTSIZE

         CALL T_CUF  ( CPYHAN(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


C     Let's look up data without requiring a.v.
C
      CALL T_CLF ( CKS(CKNO), HNDLES(CKNO) )

      DO SEGNO = 1, NSEG(CKNO)

         CALL T_CBS  ( INST, TBEGS(SEGNO) + 0.5D0, TOL0, .FALSE. )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        In this case, the segment should be found.  Make sure
C        we get back the right handle and segment identifier.
C
         CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(CKNO),  0, OK )
         CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(SEGNO),    OK )

C
C        Check the descriptor as well.  However, don't check the
C        segment addresses.
C
         CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,SEGNO), DSCSIZ-1,
     .                  0.D0,   OK                                   )

      END DO



      CALL TCASE ( 'Now make sure that segments without a.v. are not '//
     .             'seen when a.v. is requested.'                     )

C
C     Repeat, now requiring a.v.
C
      DO SEGNO = 1, NSEG(CKNO)

         CALL T_CBS  ( INST, TBEGS(SEGNO) + 0.5D0, TOL0, .TRUE. )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        In this case, the segment should be found.  Make sure
C        we get back the right handle and segment identifier.
C
         CALL CHCKSL ( 'FOUND',  FOUND,  EVEN(SEGNO), OK )

         IF ( FOUND ) THEN

            CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(CKNO),  0, OK )
            CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(SEGNO),    OK )
C
C           Check the descriptor as well.  However, don't check the
C           segment addresses.
C
            CALL T_CHDS( 'DESCR', DESCR, '=', XDESCR(1,SEGNO), DSCSIZ-1,
     .                    0.D0,   OK                                   )
         END IF

      END DO



      CALL TCASE ( 'Repeat the previous tests after loading CK2. ' //
     .             'This requires the search to be performed on '  //
     .             'a partial segment list.'                       )


      CALL T_CLF ( CKS(2), HNDLES(2) )
      CALL CHCKXC ( .FALSE., ' ', OK )


      DO SEGNO = 1, NSEG(CKNO)

         CALL T_CBS  ( INST, TBEGS(SEGNO) + 0.5D0, TOL0, .FALSE. )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        In this case, the segment should be found.  Make sure
C        we get back the right handle and segment identifier.
C
         CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(CKNO),  0, OK )
         CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(SEGNO),    OK )

C
C        Check the descriptor as well.  However, don't check the
C        segment addresses.
C
         CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,SEGNO), DSCSIZ-1,
     .                  0.D0,   OK                                   )

      END DO



C
C     Repeat, now requiring a.v.
C
      DO SEGNO = 1, NSEG(CKNO)

         CALL T_CBS  ( INST, TBEGS(SEGNO) + 0.5D0, TOL0, .TRUE. )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        In this case, the segment should be found.  Make sure
C        we get back the right handle and segment identifier.
C
         CALL CHCKSL ( 'FOUND',  FOUND,  EVEN(SEGNO), OK )

         IF ( FOUND ) THEN

            CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(CKNO),  0, OK )
            CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(SEGNO),    OK )
C
C           Check the descriptor as well.  However, don't check the
C           segment addresses.
C
            CALL T_CHDS( 'DESCR', DESCR, '=', XDESCR(1,SEGNO), DSCSIZ-1,
     .                    0.D0,   OK                                   )
         END IF

      END DO


      CALL TCASE ( 'Repeat the previous tests after loading CK10. ' //
     .             'This requires the search to be performed on '   //
     .             'an unbuffered file (CK12).'                      )


      CALL T_CLF ( CKS(10), HNDLES(10) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO SEGNO = 1, NSEG(CKNO)

         CALL T_CBS  ( INST, TBEGS(SEGNO) + 0.5D0, TOL0, .FALSE. )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        In this case, the segment should be found.  Make sure
C        we get back the right handle and segment identifier.
C
         CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(CKNO),  0, OK )
         CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(SEGNO),    OK )

C
C        Check the descriptor as well.  However, don't check the
C        segment addresses.
C
         CALL T_CHDS ( 'DESCR', DESCR, '=', XDESCR(1,SEGNO), DSCSIZ-1,
     .                  0.D0,   OK                                   )

      END DO


C
C     Repeat, now requiring a.v.
C
      DO SEGNO = 1, NSEG(CKNO)

         CALL T_CBS  ( INST, TBEGS(SEGNO) + 0.5D0, TOL0, .TRUE. )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        In this case, the segment should be found.  Make sure
C        we get back the right handle and segment identifier.
C
         CALL CHCKSL ( 'FOUND',  FOUND,  EVEN(SEGNO), OK )

         IF ( FOUND ) THEN

            CALL CHCKSI ( 'HANDLE', HANDLE, '=', HNDLES(CKNO),  0, OK )
            CALL CHCKSC ( 'SEGID',  SEGID,  '=', XSEGID(SEGNO),    OK )
C
C           Check the descriptor as well.  However, don't check the
C           segment addresses.
C
            CALL T_CHDS( 'DESCR', DESCR, '=', XDESCR(1,SEGNO), DSCSIZ-1,
     .                    0.D0,   OK                                   )
         END IF

      END DO



      CALL TCASE ( 'Test our ability to resume searches after a ' //
     .             'segment is found.  Resume searches of the '   //
     .             'normal segment list, partial list, and '      //
     .             'of an unbuffered file.'                       )

C
C     Create files to be used in continued searches.  All segments
C     in these files have identical coverage.
C
      DO CKNO = 13, 15

         INST = 1

         DO SEGNO = 1, NSEG(CKNO)

            IDS(SEGNO) = INST

            TBEGS(SEGNO)  = 13 * SCALE - 1
            TENDS(SEGNO)  = TBEGS(SEGNO) + 1

            XSEGID(SEGNO) = 'File: # Segno: #  Inst:  #'

            CALL REPMC ( XSEGID(SEGNO), '#', CKS(CKNO),  XSEGID(SEGNO) )
            CALL REPMI ( XSEGID(SEGNO), '#', SEGNO,      XSEGID(SEGNO) )
            CALL REPMI ( XSEGID(SEGNO), '#', INST,       XSEGID(SEGNO) )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

         CALL T_CRDAF ( 'CK',  CKS(CKNO),  NSEG(CKNO),
     .                   IDS,  TBEGS,      TENDS,       XSEGID )

         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


C
C     Load the files.
C
      DO CKNO = 13, 15
         CALL T_CLF ( CKS(CKNO), HNDLES(CKNO) )
      END DO

C
C     Start the search.
C
      CALL T_CBS  ( INST, TBEGS(1) + 0.5D0, TOL0, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO CKNO = 15, 13, -1

         DO SEGNO = NSEG(CKNO), 1, -1

            XSEGID(SEGNO) = 'File: # Segno: #  Inst:  #'
            CALL REPMC ( XSEGID(SEGNO), '#', CKS(CKNO),  XSEGID(SEGNO) )
            CALL REPMI ( XSEGID(SEGNO), '#', SEGNO,      XSEGID(SEGNO) )
            CALL REPMI ( XSEGID(SEGNO), '#', INST,       XSEGID(SEGNO) )
            CALL CHCKXC ( .FALSE., ' ', OK )


            CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )
C
C           The segment should be found.  Make sure
C           we get back the right handle and segment identifier.
C
            CALL CHCKSL ( 'FOUND',  FOUND,  .TRUE., OK )

            IF ( FOUND ) THEN

               CALL CHCKSI( 'HANDLE', HANDLE, '=', HNDLES(CKNO),  0, OK)
               CALL CHCKSC( 'SEGID',  SEGID,  '=', XSEGID(SEGNO),    OK)

C
C              Check the descriptor as well.  However, don't check the
C              segment addresses.
C
               CALL T_CRCKDS ( SEGNO,            IDS(SEGNO),
     .                         TBEGS(SEGNO),     TENDS(SEGNO),
     .                         XDESCR(1,SEGNO), .TRUE.         )

               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL T_CHDS( 'DESCR',   DESCR, '=', XDESCR(1,SEGNO),
     .                       DSCSIZ-1, 0.D0,   OK                   )
            END IF

         END DO

      END DO

C
C     Finally, we should end up with FOUND = .FALSE.
C

      CALL T_CSN  ( HANDLE, DESCR, SEGID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND',  FOUND,  .FALSE., OK )



C
C     Last step:  delete all of the CK files we created.
C
      DO I = 1, NCK

         CALL T_CUF  ( HNDLES(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL DELFIL ( CKS  (I)  )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      DO I = 1, FTSIZE

         CALL T_CUF  ( CPYHAN(I) )
         CALL DELFIL ( CKCPY(I)  )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL T_SUCCESS ( OK )
      RETURN
      END
