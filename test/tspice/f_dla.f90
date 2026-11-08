!$Procedure F_DLA ( Test DLA routines )

      SUBROUTINE F_DLA ( OK )

! $ Abstract
!
!     Test the SPICELIB DLA routines.
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
!     TEST FAMILY
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'dla.inc'
      INCLUDE 'das.inc'

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
!     This routine tests the DLA routines
!
!        DLABBS
!        DLABFS
!        DLABNS
!        DLAENS
!        DLAFNS
!        DLAFPS
!        DLAOPN
!        DLASSG
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
!
! $ Version
!
!-    TSPICE Version 1.0.1, 15-JUN-2021 (JDR)
!
!        Edited the header and test cases comments to comply with NAIF
!        standard. Added entry to $Index_Entries.
!
!        Fixed small typos in test case titles.
!
!-    TSPICE Version 1.0.0, 06-JAN-2017 (NJB)
!
!-&


! $ Index_Entries
!
!     test DLA routines
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               DLASSG
      LOGICAL               EXISTS

!
!     Local parameters
!
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

!
!     Local Variables
!
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

!
!     Save all variables to avoid stack problems on some
!     platforms.
!
      SAVE


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_DLA' )


!     *****************************************************************
!
!     DLAOPN tests
!
!     *****************************************************************
!
!     DLAOPN error cases:
!
!     --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Blank DLA file name' )

      IF ( EXISTS(DLA0) ) THEN
         CALL DELFIL ( DLA0 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      NCOMCH = 0

      CALL DLAOPN ( ' ', 'DSK', DLA0, NCOMCH, HANDLE )
      CALL CHCKXC ( .TRUE., 'SPICE(BLANKFILENAME)', OK )


!
!     --- Case --------------------------------------------------------
!
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

!
!     --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Blank file type' )

      NCOMCH = 0

      CALL DLAOPN ( DLA0, ' ', DLA0, NCOMCH, HANDLE )
      CALL CHCKXC ( .TRUE., 'SPICE(BLANKFILETYPE)', OK )

!
!     --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Non-printing character in file type' )

      NCOMCH = 0

      CALL DLAOPN ( DLA0, CHAR(0), DLA0, NCOMCH, HANDLE )
      CALL CHCKXC ( .TRUE., 'SPICE(ILLEGALCHARACTER)', OK )


!
!     --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Bad comment record count' )


      NCOMCH = -1

      CALL DLAOPN ( DLA0, 'DSK', DLA0, NCOMCH, HANDLE )
      CALL CHCKXC ( .TRUE., 'SPICE(BADRECORDCOUNT)', OK )


!
!     DLAOPN normal cases:
!
!     --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Create DLA file without data' )


      NCOMCH = 0

      CALL DLAOPN ( DLA0, 'DSK', DLA0, NCOMCH, HAN0 )

!
!     Close file and re-open for read access.
!
      CALL DASCLS ( HAN0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASOPR ( DLA0, HAN0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check DLA version.
!
      CALL DASRDI ( HAN0, VERIDX, VERIDX, IDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'DLA version', IDATA(1), '=', FMTVER, 0, OK )

!
!     Check forward and backward pointers.
!
      CALL DASRDI ( HAN0, LLBIDX, LLBIDX, IDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'Begin ptr', IDATA(1), '=', NULPTR, 0, OK )

      CALL DASRDI ( HAN0, LLEIDX, LLEIDX, IDATA )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'End ptr',   IDATA(1), '=', NULPTR, 0, OK )

!
!     Leave file open for read access for further tests.
!
!     --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Create DLA file with 10 segments.' )

!
!     Note that we can't use a DSK for these tests, since DSKs don't
!     contain character data.
!
      IF ( EXISTS(DLA1) ) THEN
         CALL DELFIL ( DLA1 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

!
!     Add space for comment characters, just to make sure this
!     doesn't cause problems.
!
      NCOMCH = 4096

      CALL DLAOPN ( DLA1, 'DSK', DLA1, NCOMCH, HAN1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Populate data buffers.
!
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

!
!     Close file and re-open for read access.
!
      CALL DASCLS ( HAN1 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL DASOPR ( DLA1, HAN1 )
      CALL CHCKXC ( .FALSE., ' ', OK )


!     *****************************************************************
!
!     DLABFS/DLAFNS tests
!
!     *****************************************************************
!
!     --- Case --------------------------------------------------------
!
      CALL TCASE ( 'DLABFS: bad handle' )

      CALL DLABFS ( -999, DLADSC, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDHANDLE)', OK )



!
!     --- Case --------------------------------------------------------
!
      CALL TCASE ( 'DLAFNS: bad handle' )

      CALL DLAFNS ( -999, DLADSC, NXTDSC, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDHANDLE)', OK )


!
!     --- Case --------------------------------------------------------
!
      CALL TCASE ( 'DLABFS: Start forward search on empty list' )

!
!     DLA0 is assumed to be open at this point.
!


      CALL CLEARI ( DLADSZ, DLADSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CLEARI ( DLADSZ, XDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DLABFS ( HAN0, DLADSC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

      CALL CHCKAI ( 'DLADSC', DLADSC, '=', XDSC, DLADSZ, OK )

!
!     --- Case --------------------------------------------------------
!
      CALL TCASE ( 'DLABFS/DLAFNS: perform forward search on ' &
      //           '10-segment DLA.'                    )

!
!     DLA1 is assumed to be open at this point.
!
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
!
!        Check out the descriptor components.
!
!        Base addresses precede the first address in use by
!        a DLA segment.
!
!        The integer base is shifted by the amount of integer
!        data in the previous segment, plus the size of a DLA
!        descriptor, since DLA descriptors reside in DAS integer
!        address space.
!
         XBASEI = XBASEI + DLADSZ + PRVDSC(ISZIDX)

         LABEL = 'IBASE segment #'
         CALL REPMI ( LABEL, '#', NFOUND, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( LABEL, DLADSC(IBSIDX), '=', &
                              XBASEI,         0,   OK )

!
!        Set the expected size based on the formula used
!        for constructing the file.
!
         XSIZEI = IBUFSZ - NFOUND

         LABEL = 'ISIZE segment #'
         CALL REPMI ( LABEL, '#', NFOUND, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( LABEL, DLADSC(ISZIDX), '=', &
                              XSIZEI,         0,   OK )


!
!        Check d.p. base and size.
!
         XBASED = XBASED + PRVDSC(DSZIDX)

         LABEL = 'DBASE segment #'
         CALL REPMI ( LABEL, '#', NFOUND, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( LABEL, DLADSC(DBSIDX), '=', &
                              XBASED,         0,   OK )

!
!        Set the expected size based on the formula used
!        for constructing the file.
!
         XSIZED = DBUFSZ - NFOUND

         LABEL = 'DSIZE segment #'
         CALL REPMI ( LABEL, '#', NFOUND, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( LABEL, DLADSC(DSZIDX), '=', &
                              XSIZED,         0,   OK )


!
!        Check character base and size.
!
         XBASEC = XBASEC + PRVDSC(CSZIDX)

         LABEL = 'CBASE segment #'
         CALL REPMI ( LABEL, '#', NFOUND, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( LABEL, DLADSC(CBSIDX), '=', &
                              XBASEC,         0,   OK )

!
!        Set the expected size based on the formula used
!        for constructing the file.
!
         XSIZEC = (CBUFSZ - NFOUND) * CBUFLN

         LABEL = 'CSIZE segment #'
         CALL REPMI ( LABEL, '#', NFOUND, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( LABEL, DLADSC(CSZIDX), '=', &
                              XSIZEC,         0,   OK )

!
!        Check the forward pointer. The pointer should contain
!        the DAS integer address of the first element of
!        the next descriptor, or NULPTR if the current segment
!        is the last.
!
         IF ( NFOUND .LT. NSEG ) THEN

            XFWD = DLADSC(IBSIDX) + DLADSC(ISZIDX) + 1
         ELSE
            XFWD = NULPTR
         END IF


         LABEL = 'FWD PTR segment #'
         CALL REPMI ( LABEL, '#', NFOUND, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( LABEL, DLADSC(FWDIDX), '=', &
                              XFWD,           0,   OK )

!
!        Check the backward pointer. The pointer should contain
!        the DAS integer address of the first element of
!        the previous descriptor, or NULPTR if the current segment
!        is the first.
!
         IF ( NFOUND .GT. 1 ) THEN

            XBWD = PRVDSC(IBSIDX) + 1 - DLADSZ
         ELSE
            XBWD = NULPTR
         END IF


         LABEL = 'BWD PTR segment #'
         CALL REPMI ( LABEL, '#', NFOUND, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( LABEL, DLADSC(BWDIDX), '=', &
                              XBWD,           0,   OK )

!
!        Prepare to check data. Fill buffers with expected
!        values.
!
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

!
!        Check integer data.
!
         CALL DASRDI ( HAN1, XBASEI+1, XBASEI+XSIZEI, IDATA )
         CALL CHCKXC ( .FALSE., ' ', OK )

         LABEL = 'IDATA segment #'
         CALL REPMI ( LABEL, '#', NFOUND, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAI ( LABEL, IDATA, '=', XDATAI, XSIZEI, OK )

!
!        Check d.p. data.
!
         CALL DASRDD ( HAN1, XBASED+1, XBASED+XSIZED, DDATA )
         CALL CHCKXC ( .FALSE., ' ', OK )

         LABEL = 'DDATA segment #'
         CALL REPMI ( LABEL, '#', NFOUND, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( LABEL, DDATA, '=', XDATAD, XSIZED, 0.D0, OK )


!
!        Check character data.
!
         CALL DASRDC ( HAN1, XBASEC+1, XBASEC+XSIZEC, 1, CBUFLN, CDATA )
         CALL CHCKXC ( .FALSE., ' ', OK )

         LABEL = 'CDATA segment #'
         CALL REPMI ( LABEL, '#', NFOUND, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        For this check we must convert the character count to
!        an array element count.
!
         CALL CHCKAC ( LABEL, CDATA, '=', XDATAC, XSIZEC/CBUFLN, OK )

!
!        Fetch next segment.
!
         CALL MOVEI ( DLADSC, DLADSZ, PRVDSC )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL DLAFNS ( HAN1, PRVDSC, DLADSC, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )


         IF ( NFOUND .GT. NSEG ) THEN
!
!           Escape from loop if necessary.
!
            FOUND = .FALSE.

         END IF

      END DO

      CALL CHCKSI ( 'NFOUND', NFOUND, '=', NSEG, 0, OK )





!     *****************************************************************
!
!     DLABBS/DLAFPS tests
!
!     *****************************************************************
!
!     --- Case --------------------------------------------------------
!
      CALL TCASE ( 'DLABBS: bad handle' )

      CALL DLABBS ( -999, DLADSC, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDHANDLE)', OK )



!
!     --- Case --------------------------------------------------------
!
      CALL TCASE ( 'DLAFPS: bad handle' )

      CALL DLAFPS ( -999, DLADSC, PRVDSC, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDHANDLE)', OK )

!
!     --- Case --------------------------------------------------------
!
      CALL TCASE ( 'DLABBS: Start backward search on empty list' )

      CALL CLEARI ( DLADSZ, DLADSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CLEARI ( DLADSZ, XDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DLABBS ( HAN0, DLADSC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

      CALL CHCKAI ( 'DLADSC', DLADSC, '=', XDSC, DLADSZ, OK )

!
!     --- Case --------------------------------------------------------
!
      CALL TCASE ( 'DLABBS/DLAFPS: perform backward search on ' &
      //           '10-segment DLA.'                    )

!
!     DLA1 is assumed to be open at this point.
!
      NFOUND = 0

      CALL DLABBS ( HAN1, DLADSC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )


!
!     We're going to assume at this point that the forward
!     search routines are working. We'll use them to produce
!     the expected descriptors.
!
      DO I = 1, NSEG
!
!        Produce the expected descriptor for the Ith segment.
!
         CALL DLABFS ( HAN1, XDSC, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         DO J = 1, I-1

            CALL MOVEI  ( XDSC, DLADSZ, DLADSC )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL DLAFNS ( HAN1, DLADSC, XDSC, FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

!
!        Do a backward search to locate the Ith segment.
!
         CALL DLABBS ( HAN1, DLADSC, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

         DO J = 1, NSEG-I

            CALL DLAFPS ( HAN1, DLADSC, PRVDSC, FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL MOVEI ( PRVDSC, DLADSZ, DLADSC )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

!
!        Compare descriptors.
!
         LABEL = 'DLADSC segment #'
         CALL REPMI ( LABEL, '#', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAI ( LABEL, DLADSC, '=', XDSC, DLADSZ, OK )

      END DO



!     *****************************************************************
!
!     DLASSG tests
!
!     *****************************************************************
!
!     --- Case --------------------------------------------------------
!
      CALL TCASE ( 'DLASSG: compare DLA descriptors in 10-segment ' &
      //           'DLA file.'                                     )

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

!
!     --- Case --------------------------------------------------------
!
      CALL TCASE ( 'DLASSG: compare matching descriptors and ' &
      //           'handles.'                                  )


      ISSAME = DLASSG ( HAN1, HAN1, DLADSC, DLADSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISSAME', ISSAME, .TRUE., OK )

!
!     --- Case --------------------------------------------------------
!
      CALL TCASE ( 'DLASSG: compare matching descriptors and ' &
      //           'different handles.'                      )


      ISSAME = DLASSG ( HAN1, HAN1+1, DLADSC, DLADSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISSAME', ISSAME, .FALSE., OK )

!
!     --- Case --------------------------------------------------------
!
      CALL TCASE ( 'DLASSG: compare matching handles and ' &
      //           'different descriptors.'               )

!
!     Make sure that a difference in any descriptor element is
!     detected.
!
      CALL MOVEI ( DLADSC, DLADSZ, NXTDSC )

      DO I = 1, DLADSZ
!
!        Modify the Ith element of NXTDSC.
!
         NXTDSC(I) = -NXTDSC(I)

         ISSAME = DLASSG ( HAN1, HAN1, DLADSC, NXTDSC )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'ISSAME', ISSAME, .FALSE., OK )
!
!        Restore original element value.
!
         NXTDSC(I) = -NXTDSC(I)

      END DO


!
!     --- Case --------------------------------------------------------
!
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
