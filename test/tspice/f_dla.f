C$Procedure F_DLA ( Test DLA routines )

      SUBROUTINE F_DLA ( OK )

C$ Abstract
C
C     Test the SPICELIB DLA routines.
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
C     TEST FAMILY
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'dla.inc'
      INCLUDE 'das.inc'

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
C     This routine tests the DLA routines
C
C        DLABBS
C        DLABFS
C        DLABNS
C        DLAENS
C        DLAFNS
C        DLAFPS
C        DLAOPN
C        DLASSG
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
C
C$ Version
C
C-    TSPICE Version 1.0.1, 15-JUN-2021 (JDR)
C
C        Edited the header and test cases comments to comply with NAIF
C        standard. Added entry to $Index_Entries.
C
C        Fixed small typos in test case titles.
C
C-    TSPICE Version 1.0.0, 06-JAN-2017 (NJB)
C
C-&


C$ Index_Entries
C
C     test DLA routines
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               DLASSG
      LOGICAL               EXISTS

C
C     Local parameters
C
      CHARACTER*(*)         DLA0
      PARAMETER           ( DLA0   = 'dlatest0.dla' )

      CHARACTER*(*)         DLA1
      PARAMETER           ( DLA1   = 'dlatest1.dla' )


      INTEGER               CBUFLN
      PARAMETER           ( CBUFLN = 80 )

      INTEGER               CBUFSZ
      PARAMETER           ( CBUFSZ = 100 )

      INTEGER               IBUFSZ
      PARAMETER           ( IBUFSZ = 100 )

      INTEGER               DBUFSZ
      PARAMETER           ( DBUFSZ = 100 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

C
C     Local Variables
C
      CHARACTER*(CBUFLN)    CDATA  ( CBUFSZ )
      CHARACTER*(LNSIZE)    LABEL
      CHARACTER*(CBUFLN)    XDATAC ( CBUFSZ )

      DOUBLE PRECISION      DDATA  ( DBUFSZ )
      DOUBLE PRECISION      XDATAD ( DBUFSZ )

      INTEGER               DLADSC ( DLADSZ )
      INTEGER               HAN0
      INTEGER               HAN1
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               IDATA  ( IBUFSZ )
      INTEGER               J
      INTEGER               NCADD
      INTEGER               NCOMCH
      INTEGER               NFOUND
      INTEGER               NSEG
      INTEGER               NXTDSC ( DLADSZ )
      INTEGER               PRVDSC ( DLADSZ )
      INTEGER               XBASEC
      INTEGER               XBASED
      INTEGER               XBASEI
      INTEGER               XDATAI ( IBUFSZ )
      INTEGER               XBWD
      INTEGER               XDSC   ( DLADSZ )
      INTEGER               XFWD
      INTEGER               XSIZEC
      INTEGER               XSIZED
      INTEGER               XSIZEI

      LOGICAL               FOUND
      LOGICAL               ISSAME

C
C     Save all variables to avoid stack problems on some
C     platforms.
C
      SAVE


C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_DLA' )


C     *****************************************************************
C
C     DLAOPN tests
C
C     *****************************************************************
C
C     DLAOPN error cases:
C
C     --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Blank DLA file name' )

      IF ( EXISTS(DLA0) ) THEN
         CALL DELFIL ( DLA0 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      NCOMCH = 0

      CALL DLAOPN ( ' ', 'DSK', DLA0, NCOMCH, HANDLE )
      CALL CHCKXC ( .TRUE., 'SPICE(BLANKFILENAME)', OK )


C
C     --- Case --------------------------------------------------------
C
      CALL TCASE ( 'File exists' )

      NCOMCH = 0

      CALL DLAOPN ( DLA0, 'DSK', DLA0, NCOMCH, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DLAOPN ( DLA0, 'DSK', DLA0, NCOMCH, HAN1 )
      CALL CHCKXC ( .TRUE., 'SPICE(FILEOPENFAIL)', OK )

      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( DLA0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Blank file type' )

      NCOMCH = 0

      CALL DLAOPN ( DLA0, ' ', DLA0, NCOMCH, HANDLE )
      CALL CHCKXC ( .TRUE., 'SPICE(BLANKFILETYPE)', OK )

C
C     --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Non-printing character in file type' )

      NCOMCH = 0

      CALL DLAOPN ( DLA0, CHAR(0), DLA0, NCOMCH, HANDLE )
      CALL CHCKXC ( .TRUE., 'SPICE(ILLEGALCHARACTER)', OK )


C
C     --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Bad comment record count' )


      NCOMCH = -1

      CALL DLAOPN ( DLA0, 'DSK', DLA0, NCOMCH, HANDLE )
      CALL CHCKXC ( .TRUE., 'SPICE(BADRECORDCOUNT)', OK )


C
C     DLAOPN normal cases:
C
C     --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Create DLA file without data' )


      NCOMCH = 0

      CALL DLAOPN ( DLA0, 'DSK', DLA0, NCOMCH, HAN0 )

C
C     Close file and re-open for read access.
C
      CALL DASCLS ( HAN0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASOPR ( DLA0, HAN0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check DLA version.
C
      CALL DASRDI ( HAN0, VERIDX, VERIDX, IDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'DLA version', IDATA(1), '=', FMTVER, 0, OK )

C
C     Check forward and backward pointers.
C
      CALL DASRDI ( HAN0, LLBIDX, LLBIDX, IDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'Begin ptr', IDATA(1), '=', NULPTR, 0, OK )

      CALL DASRDI ( HAN0, LLEIDX, LLEIDX, IDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'End ptr',   IDATA(1), '=', NULPTR, 0, OK )

C
C     Leave file open for read access for further tests.
C
C     --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Create DLA file with 10 segments.' )

C
C     Note that we can't use a DSK for these tests, since DSKs don't
C     contain character data.
C
      IF ( EXISTS(DLA1) ) THEN
         CALL DELFIL ( DLA1 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

C
C     Add space for comment characters, just to make sure this
C     doesn't cause problems.
C
      NCOMCH = 4096

      CALL DLAOPN ( DLA1, 'DSK', DLA1, NCOMCH, HAN1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Populate data buffers.
C
      NSEG = 10

      DO I = 1, NSEG

         CALL DLABNS ( HAN1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         DO J = 1, IBUFSZ-I

            IDATA(J) = 1000 * I + J

         END DO

         DO J = 1, DBUFSZ-I

            DDATA(J) = DBLE( 1000 * I + J )

         END DO

         DO J = 1, CBUFSZ-I

            CDATA(J) = 'Segment # line #.'

            CALL REPMI ( CDATA(J), '#', I, CDATA(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPMI ( CDATA(J), '#', J, CDATA(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CDATA(J)(CBUFLN-2:) = '-->'

         END DO



         CALL DASADI ( HAN1, IBUFSZ-I, IDATA )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL DASADD ( HAN1, DBUFSZ-I, DDATA )
         CALL CHCKXC ( .FALSE., ' ', OK )

         NCADD = (CBUFSZ-I)*CBUFLN

         CALL DASADC ( HAN1, NCADD, 1, CBUFLN, CDATA )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL DLAENS ( HAN1 )
         CALL CHCKXC ( .FALSE., ' ', OK )


      END DO

C
C     Close file and re-open for read access.
C
      CALL DASCLS ( HAN1 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL DASOPR ( DLA1, HAN1 )
      CALL CHCKXC ( .FALSE., ' ', OK )


C     *****************************************************************
C
C     DLABFS/DLAFNS tests
C
C     *****************************************************************
C
C     --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DLABFS: bad handle' )

      CALL DLABFS ( -999, DLADSC, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDHANDLE)', OK )



C
C     --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DLAFNS: bad handle' )

      CALL DLAFNS ( -999, DLADSC, NXTDSC, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDHANDLE)', OK )


C
C     --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DLABFS: Start forward search on empty list' )

C
C     DLA0 is assumed to be open at this point.
C


      CALL CLEARI ( DLADSZ, DLADSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CLEARI ( DLADSZ, XDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DLABFS ( HAN0, DLADSC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

      CALL CHCKAI ( 'DLADSC', DLADSC, '=', XDSC, DLADSZ, OK )

C
C     --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DLABFS/DLAFNS: perform forward search on '
     .//           '10-segment DLA.'                    )

C
C     DLA1 is assumed to be open at this point.
C
      CALL CLEARI ( DLADSZ, PRVDSC )

      NFOUND = 0

      CALL DLABFS ( HAN1, DLADSC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      XBASEI = 3
      XBASEC = 0
      XBASED = 0

      DO WHILE ( FOUND )

         NFOUND = NFOUND + 1
C
C        Check out the descriptor components.
C
C        Base addresses precede the first address in use by
C        a DLA segment.
C
C        The integer base is shifted by the amount of integer
C        data in the previous segment, plus the size of a DLA
C        descriptor, since DLA descriptors reside in DAS integer
C        address space.
C
         XBASEI = XBASEI + DLADSZ + PRVDSC(ISZIDX)

         LABEL = 'IBASE segment #'
         CALL REPMI ( LABEL, '#', NFOUND, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( LABEL, DLADSC(IBSIDX), '=',
     .                        XBASEI,         0,   OK )

C
C        Set the expected size based on the formula used
C        for constructing the file.
C
         XSIZEI = IBUFSZ - NFOUND

         LABEL = 'ISIZE segment #'
         CALL REPMI ( LABEL, '#', NFOUND, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( LABEL, DLADSC(ISZIDX), '=',
     .                        XSIZEI,         0,   OK )


C
C        Check d.p. base and size.
C
         XBASED = XBASED + PRVDSC(DSZIDX)

         LABEL = 'DBASE segment #'
         CALL REPMI ( LABEL, '#', NFOUND, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( LABEL, DLADSC(DBSIDX), '=',
     .                        XBASED,         0,   OK )

C
C        Set the expected size based on the formula used
C        for constructing the file.
C
         XSIZED = DBUFSZ - NFOUND

         LABEL = 'DSIZE segment #'
         CALL REPMI ( LABEL, '#', NFOUND, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( LABEL, DLADSC(DSZIDX), '=',
     .                        XSIZED,         0,   OK )


C
C        Check character base and size.
C
         XBASEC = XBASEC + PRVDSC(CSZIDX)

         LABEL = 'CBASE segment #'
         CALL REPMI ( LABEL, '#', NFOUND, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( LABEL, DLADSC(CBSIDX), '=',
     .                        XBASEC,         0,   OK )

C
C        Set the expected size based on the formula used
C        for constructing the file.
C
         XSIZEC = (CBUFSZ - NFOUND) * CBUFLN

         LABEL = 'CSIZE segment #'
         CALL REPMI ( LABEL, '#', NFOUND, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( LABEL, DLADSC(CSZIDX), '=',
     .                        XSIZEC,         0,   OK )

C
C        Check the forward pointer. The pointer should contain
C        the DAS integer address of the first element of
C        the next descriptor, or NULPTR if the current segment
C        is the last.
C
         IF ( NFOUND .LT. NSEG ) THEN

            XFWD = DLADSC(IBSIDX) + DLADSC(ISZIDX) + 1
         ELSE
            XFWD = NULPTR
         END IF


         LABEL = 'FWD PTR segment #'
         CALL REPMI ( LABEL, '#', NFOUND, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( LABEL, DLADSC(FWDIDX), '=',
     .                        XFWD,           0,   OK )

C
C        Check the backward pointer. The pointer should contain
C        the DAS integer address of the first element of
C        the previous descriptor, or NULPTR if the current segment
C        is the first.
C
         IF ( NFOUND .GT. 1 ) THEN

            XBWD = PRVDSC(IBSIDX) + 1 - DLADSZ
         ELSE
            XBWD = NULPTR
         END IF


         LABEL = 'BWD PTR segment #'
         CALL REPMI ( LABEL, '#', NFOUND, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( LABEL, DLADSC(BWDIDX), '=',
     .                        XBWD,           0,   OK )

C
C        Prepare to check data. Fill buffers with expected
C        values.
C
         DO J = 1, IBUFSZ-NFOUND

            XDATAI(J) = 1000 * NFOUND + J

         END DO

         DO J = 1, DBUFSZ-NFOUND

            XDATAD(J) = DBLE( 1000 * NFOUND + J )

         END DO

         DO J = 1, CBUFSZ-NFOUND

            XDATAC(J) = 'Segment # line #.'

            CALL REPMI ( XDATAC(J), '#', NFOUND, XDATAC(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPMI ( XDATAC(J), '#', J, XDATAC(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            XDATAC(J)(CBUFLN-2:) = '-->'

         END DO

C
C        Check integer data.
C
         CALL DASRDI ( HAN1, XBASEI+1, XBASEI+XSIZEI, IDATA )
         CALL CHCKXC ( .FALSE., ' ', OK )

         LABEL = 'IDATA segment #'
         CALL REPMI ( LABEL, '#', NFOUND, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAI ( LABEL, IDATA, '=', XDATAI, XSIZEI, OK )

C
C        Check d.p. data.
C
         CALL DASRDD ( HAN1, XBASED+1, XBASED+XSIZED, DDATA )
         CALL CHCKXC ( .FALSE., ' ', OK )

         LABEL = 'DDATA segment #'
         CALL REPMI ( LABEL, '#', NFOUND, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( LABEL, DDATA, '=', XDATAD, XSIZED, 0.D0, OK )


C
C        Check character data.
C
         CALL DASRDC ( HAN1, XBASEC+1, XBASEC+XSIZEC, 1, CBUFLN, CDATA )
         CALL CHCKXC ( .FALSE., ' ', OK )

         LABEL = 'CDATA segment #'
         CALL REPMI ( LABEL, '#', NFOUND, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        For this check we must convert the character count to
C        an array element count.
C
         CALL CHCKAC ( LABEL, CDATA, '=', XDATAC, XSIZEC/CBUFLN, OK )

C
C        Fetch next segment.
C
         CALL MOVEI ( DLADSC, DLADSZ, PRVDSC )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL DLAFNS ( HAN1, PRVDSC, DLADSC, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )


         IF ( NFOUND .GT. NSEG ) THEN
C
C           Escape from loop if necessary.
C
            FOUND = .FALSE.

         END IF

      END DO

      CALL CHCKSI ( 'NFOUND', NFOUND, '=', NSEG, 0, OK )





C     *****************************************************************
C
C     DLABBS/DLAFPS tests
C
C     *****************************************************************
C
C     --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DLABBS: bad handle' )

      CALL DLABBS ( -999, DLADSC, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDHANDLE)', OK )



C
C     --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DLAFPS: bad handle' )

      CALL DLAFPS ( -999, DLADSC, PRVDSC, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDHANDLE)', OK )

C
C     --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DLABBS: Start backward search on empty list' )

      CALL CLEARI ( DLADSZ, DLADSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CLEARI ( DLADSZ, XDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DLABBS ( HAN0, DLADSC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

      CALL CHCKAI ( 'DLADSC', DLADSC, '=', XDSC, DLADSZ, OK )

C
C     --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DLABBS/DLAFPS: perform backward search on '
     .//           '10-segment DLA.'                    )

C
C     DLA1 is assumed to be open at this point.
C
      NFOUND = 0

      CALL DLABBS ( HAN1, DLADSC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )


C
C     We're going to assume at this point that the forward
C     search routines are working. We'll use them to produce
C     the expected descriptors.
C
      DO I = 1, NSEG
C
C        Produce the expected descriptor for the Ith segment.
C
         CALL DLABFS ( HAN1, XDSC, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         DO J = 1, I-1

            CALL MOVEI  ( XDSC, DLADSZ, DLADSC )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL DLAFNS ( HAN1, DLADSC, XDSC, FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

C
C        Do a backward search to locate the Ith segment.
C
         CALL DLABBS ( HAN1, DLADSC, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

         DO J = 1, NSEG-I

            CALL DLAFPS ( HAN1, DLADSC, PRVDSC, FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL MOVEI ( PRVDSC, DLADSZ, DLADSC )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

C
C        Compare descriptors.
C
         LABEL = 'DLADSC segment #'
         CALL REPMI ( LABEL, '#', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAI ( LABEL, DLADSC, '=', XDSC, DLADSZ, OK )

      END DO



C     *****************************************************************
C
C     DLASSG tests
C
C     *****************************************************************
C
C     --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DLASSG: compare DLA descriptors in 10-segment '
     .//           'DLA file.'                                     )

      CALL DLABFS ( HAN1, DLADSC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      DO I = 2, 10

         CALL DLAFNS ( HAN1, DLADSC, NXTDSC, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

         ISSAME = DLASSG ( HAN1, HAN1, DLADSC, NXTDSC )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'ISSAME', ISSAME, .FALSE., OK )

         CALL MOVEI ( NXTDSC, DLADSZ, DLADSC )

      END DO

C
C     --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DLASSG: compare matching descriptors and '
     .//           'handles.'                                  )


      ISSAME = DLASSG ( HAN1, HAN1, DLADSC, DLADSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISSAME', ISSAME, .TRUE., OK )

C
C     --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DLASSG: compare matching descriptors and '
     .//           'different handles.'                      )


      ISSAME = DLASSG ( HAN1, HAN1+1, DLADSC, DLADSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISSAME', ISSAME, .FALSE., OK )

C
C     --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DLASSG: compare matching handles and '
     .//           'different descriptors.'               )

C
C     Make sure that a difference in any descriptor element is
C     detected.
C
      CALL MOVEI ( DLADSC, DLADSZ, NXTDSC )

      DO I = 1, DLADSZ
C
C        Modify the Ith element of NXTDSC.
C
         NXTDSC(I) = -NXTDSC(I)

         ISSAME = DLASSG ( HAN1, HAN1, DLADSC, NXTDSC )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'ISSAME', ISSAME, .FALSE., OK )
C
C        Restore original element value.
C
         NXTDSC(I) = -NXTDSC(I)

      END DO


C
C     --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Clean up DLA files' )

      CALL DASCLS ( HAN0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( EXISTS(DLA0) ) THEN
         CALL DELFIL ( DLA0 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF


      CALL DASCLS ( HAN1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( EXISTS(DLA1) ) THEN
         CALL DELFIL ( DLA1 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL T_SUCCESS ( OK )
      RETURN
      END
