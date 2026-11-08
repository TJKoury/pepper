!$Procedure DAFFA ( DAF, find array )

      SUBROUTINE DAFFA ( HANDLE, SUM, NAME, FOUND )

! $ Abstract
!
!     Find arrays in a DAF.
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
!     DAF
!
! $ Keywords
!
!     FILES
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE              'zzddhman.inc'

      INTEGER               TBSIZE
      PARAMETER           ( TBSIZE = FTSIZE )

      INTEGER               HANDLE
      DOUBLE PRECISION      SUM    ( * )
      CHARACTER*(*)         NAME
      LOGICAL               FOUND

! $ Brief_I/O
!
!     VARIABLE  I/O  ENTRY
!     --------  ---  --------------------------------------------------
!     HANDLE    I-O  DAFBFS, DAFBBS, DAFGH, DAFCS
!     SUM       I-O  DAFGS,  DAFRS,  DAFWS
!     NAME      I-O  DAFGN,  DAFRN
!     FOUND      O   DAFFNA, DAFFPA
!
! $ Detailed_Input
!
!     HANDLE   on input is the handle of the DAF to be searched.
!
!     SUM      on input is an array summary that replaces the
!              summary of the current array in the DAF currently
!              being searched.
!
!     NAME     on input is an array name that replaces the name
!              of the current array in the DAF currently being
!              searched.
!
! $ Detailed_Output
!
!     HANDLE   on output is the handle of the DAF currently being
!              searched.
!
!     SUM      on output is the summary for the array found most
!              recently.
!
!     NAME     on output is the name for the array found
!              most recently.
!
!     FOUND    is .TRUE. whenever the search for the next or the
!              previous array is successful, and is .FALSE. otherwise.
!
! $ Parameters
!
!     TBSIZE   is the maximum number of files (DAS and DAF) that may be
!              simultaneously open. TBSIZE is set to FTSIZE which is
!              assigned and defined in zzdhman.inc.
!
! $ Exceptions
!
!     1)  If DAFFA is called directly, the error SPICE(BOGUSENTRY)
!         is signaled.
!
!     2)  See entry points DAFBFS, DAFFNA, DAFBBS, DAFFPA, DAFGS, DAFGN,
!         DAFGH, DAFRS, DAFWS, DAFRN, and DAFCS for exceptions specific
!         to those entry points.
!
! $ Files
!
!     DAFs read by DAFFA and its entry points are opened
!     elsewhere, and referred to only by their handles.
!
! $ Particulars
!
!     DAFFA serves as an umbrella, allowing data to be shared by its
!     entry points:
!
!        DAFBFS         Begin forward search.
!        DAFFNA         Find next array.
!
!        DAFBBS         Begin backward search.
!        DAFFPA         Find previous array.
!
!        DAFGS          Get summary.
!        DAFGN          Get name.
!        DAFGH          Get handle.
!
!        DAFRS          Replace summary.
!        DAFWS          Write summary.
!        DAFRN          Replace name.
!
!        DAFCS          Continue search.
!
!     The main function of these entry points is to allow the
!     contents of any DAF to be examined on an array-by-array
!     basis.
!
!     Conceptually, the arrays in a DAF form a doubly linked list,
!     which can be searched in either of two directions: forward or
!     backward. It is possible to search multiple DAFs simultaneously.
!
!     DAFBFS (begin forward search) and DAFFNA are used to search the
!     arrays in a DAF in forward order. In applications that search a
!     single DAF at a time, the normal usage is
!
!        CALL DAFBFS ( HANDLE )
!        CALL DAFFNA ( FOUND  )
!
!        DO WHILE ( FOUND )
!
!           CALL DAFGS ( SUM  )
!           CALL DAFGN ( NAME )
!            .
!            .
!
!           CALL DAFFNA ( FOUND )
!
!        END DO
!
!
!
!     DAFBBS (begin backward search) and DAFFPA are used to search the
!     arrays in a DAF in backward order. In applications that search
!     a single DAF at a time, the normal usage is
!
!        CALL DAFBBS ( HANDLE )
!        CALL DAFFPA ( FOUND  )
!
!        DO WHILE ( FOUND )
!
!           CALL DAFGS ( SUM  )
!           CALL DAFGN ( NAME )
!            .
!            .
!
!           CALL DAFFPA ( FOUND )
!
!        END DO
!
!
!     In applications that conduct multiple searches simultaneously,
!     the above usage must be modified to specify the handle of the
!     file to operate on, in any case where the file may not be the
!     last one specified by DAFBFS or DAFBBS. The routine DAFCS
!     (DAF, continue search) is used for this purpose. Below, we
!     give an example of an interleaved search of two files specified
!     by the handles HANDL1 and HANDL2. The directions of searches
!     in different DAFs are independent; here we conduct a forward
!     search on one file and a backward search on the other.
!     Throughout, we use DAFCS to specify which file to operate on,
!     before calling DAFFNA, DAFFPA, DAFGS, DAFRS, DAFWS, DAFGN, or
!     DAFRN.
!
!
!        CALL DAFBFS ( HANDL1 )
!        CALL DAFBBS ( HANDL2 )
!
!        CALL DAFCS  ( HANDL1 )
!        CALL DAFFNA ( FOUND1 )
!
!        CALL DAFCS  ( HANDL2 )
!        CALL DAFFPA ( FOUND2 )
!
!        DO WHILE ( FOUND1 .OR. FOUND2 )
!
!           IF ( FOUND1 ) THEN
!
!              CALL DAFCS ( HANDL1 )
!              CALL DAFGS ( SUM    )
!              CALL DAFGN ( NAME   )
!               .
!               .
!              CALL DAFCS  ( HANDL1 )
!              CALL DAFFNA ( FOUND1 )
!
!           END IF
!
!           IF ( FOUND2 ) THEN
!
!              CALL DAFCS ( HANDL2 )
!              CALL DAFGS ( SUM    )
!              CALL DAFGN ( NAME   )
!               .
!               .
!              CALL DAFCS  ( HANDL2 )
!              CALL DAFFPA ( FOUND2 )
!
!           END IF
!
!        END DO
!
!
!     At any time, the latest array found (whether by DAFFNA or DAFFPA)
!     is regarded as the `current' array for the file in which the
!     array was found. The last DAF in which a search was started,
!     executed, or continued by any of DAFBFS, DAFBBS, DAFFNA, DAFFPA
!     or DAFCS is regarded as the `current' DAF. The summary and name
!     for the current array in the current DAF can be returned
!     separately, as shown above, by calls to DAFGS (get summary) and
!     DAFGN (get name). The handle of the current DAF can also be
!     returned by calling DAFGH (get handle).
!
!     The summary and name of the current array in the current DAF can
!     be updated (again, separately) by providing new ones through DAFRS
!     (replace summary) and DAFRN (replace name). This feature
!     should not be used except to correct errors that occurred during
!     the creation of a file. Note that changes can only be made to
!     files opened for write access. Also, the addresses of an array
!     cannot be changed using these routines. (Another routine,
!     DAFWS, is provided for this purpose, but should be used only
!     to reorder the arrays in a file.)
!
!     Once a search has been begun, it may be continued in either
!     direction. That is, DAFFPA may be used to back up during a
!     forward search, and DAFFNA may be used to advance during a
!     backward search.
!
! $ Examples
!
!     The numerical results shown for these examples may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) The following program illustrates the way summaries and
!        names for the arrays contained in a DAF can be modified.
!
!        This example is provided for educational purpose only.
!
!        Replace the body ID code 301 (Moon) with a test body ID,
!        e.g. -999, in every descriptor of an SPK file; update the
!        segment identifier to indicate that such change has been
!        implemented.
!
!
!        Example code begins here.
!
!
!              PROGRAM DAFFA_EX1
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions
!        C
!              INTEGER               CARDI
!              INTEGER               RTRIM
!
!        C
!        C     Local parameters.
!        C
!              INTEGER               DSCSIZ
!              PARAMETER           ( DSCSIZ  = 5    )
!
!              INTEGER               FILSIZ
!              PARAMETER           ( FILSIZ  = 255  )
!
!              INTEGER               LBCELL
!              PARAMETER           ( LBCELL = -5 )
!
!              INTEGER               MAXOBJ
!              PARAMETER           ( MAXOBJ  = 1000 )
!
!              INTEGER               ND
!              PARAMETER           ( ND      = 2    )
!
!              INTEGER               NI
!              PARAMETER           ( NI      = 6    )
!
!              INTEGER               NEWCODE
!              PARAMETER           ( NEWCODE = -999 )
!
!              INTEGER               OLDCODE
!              PARAMETER           ( OLDCODE =  301 )
!
!              INTEGER               SGIDLN
!              PARAMETER           ( SGIDLN  = 1000 )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(FILSIZ)    FNAME
!              CHARACTER*(SGIDLN)    SEGID
!              CHARACTER*(SGIDLN)    UPDSID
!
!              DOUBLE PRECISION      DC     ( ND )
!              DOUBLE PRECISION      SUM    ( DSCSIZ )
!
!              INTEGER               HANDLE
!              INTEGER               I
!              INTEGER               IC     ( NI )
!              INTEGER               IDS    ( LBCELL : MAXOBJ )
!              INTEGER               OBJ
!
!              LOGICAL               FOUND
!              LOGICAL               UPDATE
!
!        C
!        C     Get the SPK file name.
!        C
!              CALL PROMPT ( 'Enter name of the SPK file > ', FNAME )
!
!        C
!        C     Initialize the set IDS.
!        C
!              CALL SSIZEI ( MAXOBJ, IDS )
!
!        C
!        C     Find the set of objects in the SPK file.
!        C
!              CALL SPKOBJ ( FNAME, IDS )
!
!              WRITE(*,'(A)') 'Objects in the original DAF file:'
!              WRITE(*,'(20I4)') ( IDS(I), I= 1, CARDI ( IDS ) )
!
!        C
!        C     Open for writing the SPK file.
!        C
!              CALL DAFOPW ( FNAME, HANDLE )
!
!        C
!        C     Search the file in forward order.
!        C
!              CALL DAFBFS ( HANDLE )
!              CALL DAFFNA ( FOUND  )
!
!              WRITE(*,'(A)') 'Original Segment IDs (forward order):'
!
!              DO WHILE ( FOUND )
!
!        C
!        C        Fetch and unpack the descriptor (aka summary)
!        C        of the current segment, and get its name.
!        C
!                 CALL DAFGN ( SEGID )
!                 CALL DAFGS ( SUM   )
!                 CALL DAFUS ( SUM, ND, NI, DC, IC )
!
!        C
!        C        Print the current segment name
!        C
!                 WRITE(*,'(2I6,2X,A)') IC(1), IC(2),
!             .                         SEGID(:RTRIM(SEGID))
!
!        C
!        C        Replace ID codes if necessary.
!        C
!                 UPDATE = .FALSE.
!                 IF ( IC(1) .EQ. OLDCODE ) THEN
!
!                    IC(1)  = NEWCODE
!                    UPDATE = .TRUE.
!
!                 END IF
!                 IF ( IC(2) .EQ. OLDCODE ) THEN
!
!                    IC(2) = NEWCODE
!                    UPDATE = .TRUE.
!
!                 END IF
!
!        C
!        C        Update segment ID if necessary.
!        C
!                 IF ( UPDATE ) THEN
!
!                    UPDSID = '# - Updated. Do not use.'
!                    CALL REPMC ( UPDSID, '#', SEGID(:RTRIM(SEGID)),
!             .                   UPDSID                            )
!                    CALL DAFRN ( UPDSID )
!
!                 END IF
!
!        C
!        C        Re-pack the descriptor; replace the descriptor
!        C        in the file.
!        C
!                 CALL DAFPS ( ND, NI, DC, IC, SUM )
!
!                 CALL DAFRS ( SUM )
!
!        C
!        C        Find the next segment.
!        C
!                 CALL DAFFNA ( FOUND )
!
!              END DO
!
!        C
!        C     Close the file.
!        C
!              CALL DAFCLS ( HANDLE )
!
!        C
!        C     Reset the set IDS.
!        C
!              CALL SCARDI ( 0, IDS )
!
!        C
!        C     Find the set of objects in the updated SPK file.
!        C
!              CALL SPKOBJ ( FNAME, IDS )
!
!              WRITE(*,*) ' '
!              WRITE(*,'(A)') 'Objects in the updated DAF file:'
!              WRITE(*,'(20I4)') ( IDS(I), I= 1, CARDI ( IDS ) )
!
!        C
!        C     Search the file in backwards order and output the
!        C     segment IDs.
!        C
!              CALL DAFOPR ( FNAME, HANDLE )
!
!              CALL DAFBBS ( HANDLE )
!              CALL DAFFPA ( FOUND  )
!
!              WRITE(*,'(A)') 'Updated Segment IDs (backwards order):'
!
!              DO WHILE ( FOUND )
!
!        C
!        C        Fetch and unpack the descriptor (aka summary)
!        C        of the current segment, and get its name.
!        C
!                 CALL DAFGN ( SEGID )
!                 CALL DAFGS ( SUM   )
!                 CALL DAFUS ( SUM, ND, NI, DC, IC )
!
!                 WRITE(*,'(2I6,2X,A)') IC(1), IC(2),
!             .                         SEGID(:RTRIM(SEGID))
!
!        C
!        C        Find the previous segment.
!        C
!                 CALL DAFFPA ( FOUND )
!
!              END DO
!
!        C
!        C     Close the file.
!        C
!              CALL DAFCLS ( HANDLE )
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, using the SPK file named de430.bsp, the output was:
!
!
!        Enter name of the SPK file > de430.bsp
!        Objects in the original DAF file:
!           1   2   3   4   5   6   7   8   9  10 199 299 301 399
!        Original Segment IDs (forward order):
!             1     0  DE-0430LE-0430
!             2     0  DE-0430LE-0430
!             3     0  DE-0430LE-0430
!             4     0  DE-0430LE-0430
!             5     0  DE-0430LE-0430
!             6     0  DE-0430LE-0430
!             7     0  DE-0430LE-0430
!             8     0  DE-0430LE-0430
!             9     0  DE-0430LE-0430
!            10     0  DE-0430LE-0430
!           301     3  DE-0430LE-0430
!           399     3  DE-0430LE-0430
!           199     1  DE-0430LE-0430
!           299     2  DE-0430LE-0430
!
!        Objects in the updated DAF file:
!        -999   1   2   3   4   5   6   7   8   9  10 199 299 399
!        Updated Segment IDs (backwards order):
!           299     2  DE-0430LE-0430
!           199     1  DE-0430LE-0430
!           399     3  DE-0430LE-0430
!          -999     3  DE-0430LE-0430 - Updated. Do not use.
!            10     0  DE-0430LE-0430
!             9     0  DE-0430LE-0430
!             8     0  DE-0430LE-0430
!             7     0  DE-0430LE-0430
!             6     0  DE-0430LE-0430
!             5     0  DE-0430LE-0430
!             4     0  DE-0430LE-0430
!             3     0  DE-0430LE-0430
!             2     0  DE-0430LE-0430
!             1     0  DE-0430LE-0430
!
!
!     2) The following program compares data in two DAFs. The DAFs are
!        expected to have the same number of arrays, the same number
!        of elements in each corresponding array, and the same summary
!        format.
!
!        Each difference whose magnitude exceeds a specified tolerance
!        is flagged. The difference information is written to the
!        screen.
!
!
!        Example code begins here.
!
!
!              PROGRAM DAFFA_EX2
!              IMPLICIT NONE
!
!        C
!        C     Local parameters
!        C
!              INTEGER               RTRIM
!
!        C
!        C     Local parameters
!        C
!              INTEGER               ARRYSZ
!              PARAMETER           ( ARRYSZ = 10000000 )
!
!              INTEGER               ERRLEN
!              PARAMETER           ( ERRLEN =  240 )
!
!              INTEGER               FILEN
!              PARAMETER           ( FILEN  =  128 )
!
!              INTEGER               LINLEN
!              PARAMETER           ( LINLEN =   80 )
!
!              INTEGER               MAXND
!              PARAMETER           ( MAXND  =  125 )
!
!              INTEGER               MAXNI
!              PARAMETER           ( MAXNI  =  250 )
!
!              INTEGER               MAXSUM
!              PARAMETER           ( MAXSUM =  128 )
!
!              INTEGER               RLEN
!              PARAMETER           ( RLEN   = 1000 )
!
!
!        C
!        C     Local variables
!        C
!              CHARACTER*(RLEN)      ANAME1
!              CHARACTER*(RLEN)      ANAME2
!              CHARACTER*(FILEN)     DAF1
!              CHARACTER*(FILEN)     DAF2
!              CHARACTER*(FILEN)     LOG
!              CHARACTER*(ERRLEN)    PRSERR
!              CHARACTER*(LINLEN)    STR
!              CHARACTER*(LINLEN)    TOLCH
!
!              DOUBLE PRECISION      ARRAY1 ( ARRYSZ )
!              DOUBLE PRECISION      ARRAY2 ( ARRYSZ )
!              DOUBLE PRECISION      DC1    ( MAXND )
!              DOUBLE PRECISION      DC2    ( MAXND )
!              DOUBLE PRECISION      TOL
!              DOUBLE PRECISION      DIFF
!              DOUBLE PRECISION      SUM1   ( MAXSUM )
!              DOUBLE PRECISION      SUM2   ( MAXSUM )
!
!              INTEGER               FA1
!              INTEGER               FA2
!              INTEGER               I
!              INTEGER               IA1
!              INTEGER               IA2
!              INTEGER               IC1    ( MAXNI )
!              INTEGER               IC2    ( MAXNI )
!              INTEGER               HANDL1
!              INTEGER               HANDL2
!              INTEGER               LEN1
!              INTEGER               LEN2
!              INTEGER               ND1
!              INTEGER               ND2
!              INTEGER               NI1
!              INTEGER               NI2
!              INTEGER               PTR
!
!              LOGICAL               FOUND
!
!
!        C
!        C     Start out by obtaining the names of the DAFs to be
!        C     compared.
!        C
!              CALL PROMPT ( 'Enter name of first DAF  > ', DAF1 )
!              CALL PROMPT ( 'Enter name of second DAF > ', DAF2 )
!              CALL PROMPT ( 'Enter tolerance for data comparison > ',
!             .              TOLCH                                   )
!
!              CALL NPARSD ( TOLCH, TOL, PRSERR, PTR )
!
!        C
!        C     Open both DAFs for reading.
!        C
!              CALL DAFOPR ( DAF1, HANDL1 )
!              CALL DAFOPR ( DAF2, HANDL2 )
!
!        C
!        C     Start forward searches in both DAFS.
!        C
!              CALL DAFBFS ( HANDL1 )
!              CALL DAFBFS ( HANDL2 )
!
!        C
!        C     Obtain the summary formats for each DAF. Stop now
!        C     if the summary formats don't match.
!        C
!              CALL DAFHSF ( HANDL1, ND1, NI1 )
!              CALL DAFHSF ( HANDL2, ND2, NI2 )
!
!              IF (  ( ND1 .NE. ND2 ) .OR. ( NI1 .NE. NI2 )  ) THEN
!
!                 STR = 'Summary formats do not match.  NI1 = #, '//
!             .                      'NI2 = #, ND1 = #, ND2 = #.'
!
!                 CALL REPMI  ( STR, '#', NI1, STR )
!                 CALL REPMI  ( STR, '#', NI2, STR )
!                 CALL REPMI  ( STR, '#', ND1, STR )
!                 CALL REPMI  ( STR, '#', ND2, STR )
!
!                 WRITE(*,*) STR
!
!                 CALL SIGERR ( 'Incompatible DAFs' )
!
!              END IF
!
!        C
!        C     Find the first array in each DAF. Use DAFCS
!        C     (DAF, continue search) to set the handle of the DAF
!        C     to search in before calling DAFFNA.
!        C
!              CALL DAFCS  ( HANDL1 )
!              CALL DAFFNA ( FOUND  )
!
!              IF ( FOUND ) THEN
!                 CALL DAFCS  ( HANDL2 )
!                 CALL DAFFNA ( FOUND  )
!              END IF
!
!              DO WHILE ( FOUND )
!
!        C
!        C        Get the summary and name of each array, using
!        C        DAFCS to select the DAF to get the information
!        C        from. Unpack the summaries and find the beginning
!        C        and ending addresses of the arrays. Read the
!        C        arrays into the variables ARRAY1 and ARRAY2.
!        C
!                 CALL DAFCS ( HANDL1 )
!                 CALL DAFGN ( ANAME1 )
!                 CALL DAFGS ( SUM1   )
!                 CALL DAFUS ( SUM1, ND1, NI1, DC1, IC1 )
!
!                 IA1  = IC1 ( NI1 - 1 )
!                 FA1  = IC1 ( NI1     )
!                 LEN1 = FA1 - IA1  + 1
!
!                 IF (  LEN1  .GT.  ARRYSZ  ) THEN
!                    CALL SETMSG ( 'Buffer too small; need # elts.')
!                    CALL ERRINT ( '#', LEN1                       )
!                    CALL SIGERR ( 'ARRAYTOOSMALL'                 )
!                 ELSE
!                    CALL DAFGDA ( HANDL1, IA1, FA1, ARRAY1 )
!                 END IF
!
!                 CALL DAFCS ( HANDL2 )
!                 CALL DAFGN ( ANAME2 )
!                 CALL DAFGS ( SUM2   )
!                 CALL DAFUS ( SUM2, ND2, NI2, DC2, IC2 )
!
!                 IA2 = IC2 ( NI2 - 1 )
!                 FA2 = IC2 ( NI2     )
!
!                 LEN2 = FA2 - IA2  + 1
!
!                 IF (  LEN2  .GT.  ARRYSZ  ) THEN
!
!                    CALL SETMSG ( 'Buffer too small; need # elts.')
!                    CALL ERRINT ( '#', LEN2                       )
!                    CALL SIGERR ( 'ARRAYTOOSMALL'                 )
!
!                 ELSE IF ( LEN1 .NE. LEN2 ) THEN
!
!                    CALL SETMSG ( 'DAF structures do not match. '//
!             .                    'LEN1 = #, LEN2 = #. ' )
!                    CALL ERRINT ( '#', LEN1              )
!                    CALL ERRINT ( '#', LEN2              )
!                    CALL SIGERR ( 'Incompatible DAFs' )
!
!                 ELSE
!                    CALL DAFGDA ( HANDL2, IA2, FA2, ARRAY2 )
!                 END IF
!        C
!        C
!        C        Compare the data in the two arrays. Log a message
!        C        for every instance of data that differs by more
!        C        than the allowed tolerance. Use the array names
!        C        to label the data sources.
!        C
!                 DO I = 1, LEN1
!
!                    DIFF  =  ABS( ARRAY1(I) - ARRAY2(I) )
!
!                    IF (  DIFF  .GT.  TOL  ) THEN
!        C
!        C              Get the array names.
!        C
!                       CALL DAFCS ( HANDL1 )
!                       CALL DAFGN ( ANAME1 )
!                       CALL DAFCS ( HANDL2 )
!                       CALL DAFGN ( ANAME2 )
!
!        C
!        C              Construct the report strings. The number 14
!        C              below is the number of significant digits to
!        C              show in the strings representing d.p.
!        C              numbers.
!        C
!
!                       WRITE(*,*) ' '
!                       WRITE(*,*) 'Difference of array ' //
!             .                    'elements exceeded '   //
!             .                    'tolerance.'
!                       WRITE(*,*) 'First array : ',
!             .                     ANAME1(:RTRIM(ANAME1))
!                       WRITE(*,*) 'Second array: ',
!             .                     ANAME2(:RTRIM(ANAME2))
!
!                       STR = 'First value : #'
!                       CALL REPMD  ( STR, '#', ARRAY1(I), 14, STR )
!                       WRITE(*,*) STR
!
!                       STR = 'Second value: #'
!                       CALL REPMD  ( STR, '#', ARRAY2(I), 14, STR )
!                       WRITE(*,*) STR
!
!                       STR = 'Difference  :  #'
!                       CALL REPMD  ( STR, '#', DIFF,      14, STR )
!                       WRITE(*,*) STR
!
!                    END IF
!
!                 END DO
!
!        C
!        C        Find the next pair of arrays.
!        C
!                 CALL DAFCS  ( HANDL1 )
!                 CALL DAFFNA ( FOUND  )
!
!                 IF ( FOUND ) THEN
!                    CALL DAFCS  ( HANDL2 )
!                    CALL DAFFNA ( FOUND  )
!                 END IF
!
!              END DO
!
!        C
!        C     Close the DAFs.
!        C
!              CALL DAFCLS ( HANDL1 )
!              CALL DAFCLS ( HANDL2 )
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, using the PCK (DAF) files named
!        earth_720101_031229.bpc and earth_720101_070527.bpc, and a
!        tolerance of '1.D0', the output was:
!
!
!        Enter name of first DAF  > earth_720101_031229.bpc
!        Enter name of second DAF > earth_720101_070527.bpc
!        Enter tolerance for data comparison > 1.D0
!
!         Difference of array elements exceeded tolerance.
!         First array : Earth PCK, ITRF93 Frame
!         Second array: Earth PCK, ITRF93 Frame
!         First value : -8.8352636890345E+08
!         Second value: -8.8352636783584E+08
!         Difference  :  1.0676109790802E+00
!
!         Difference of array elements exceeded tolerance.
!         First array : Earth PCK, ITRF93 Frame
!         Second array: Earth PCK, ITRF93 Frame
!         First value : -8.8343997629503E+08
!         Second value: -8.8343997451568E+08
!         Difference  :  1.7793515920639E+00
!
!         Difference of array elements exceeded tolerance.
!         First array : Earth PCK, ITRF93 Frame
!         Second array: Earth PCK, ITRF93 Frame
!         First value : -8.8335358368661E+08
!         Second value: -8.8335358119552E+08
!         Difference  :  2.4910920858383E+00
!
!         Difference of array elements exceeded tolerance.
!         First array : Earth PCK, ITRF93 Frame
!         Second array: Earth PCK, ITRF93 Frame
!         First value : -8.8326719107819E+08
!         Second value: -8.8326718787536E+08
!         Difference  :  3.2028328180313E+00
!
!         Difference of array elements exceeded tolerance.
!         First array : Earth PCK, ITRF93 Frame
!         Second array: Earth PCK, ITRF93 Frame
!         First value : -8.8318079846977E+08
!         Second value: -8.8318079455519E+08
!         Difference  :  3.9145733118057E+00
!
!         Difference of array elements exceeded tolerance.
!         First array : Earth PCK, ITRF93 Frame
!         Second array: Earth PCK, ITRF93 Frame
!         First value : -8.8309440586135E+08
!         Second value: -8.8309440123503E+08
!         Difference  :  4.6263140439987E+00
!
!         Difference of array elements exceeded tolerance.
!         First array : Earth PCK, ITRF93 Frame
!         Second array: Earth PCK, ITRF93 Frame
!         First value : -8.8300801325293E+08
!         Second value: -8.8300800791487E+08
!         Difference  :  5.3380546569824E+00
!
!         Difference of array elements exceeded tolerance.
!         First array : Earth PCK, ITRF93 Frame
!         Second array: Earth PCK, ITRF93 Frame
!         First value : -8.8292162064451E+08
!         Second value: -8.8292161459471E+08
!         Difference  :  6.0497951507568E+00
!
!         Difference of array elements exceeded tolerance.
!         First array : Earth PCK, ITRF93 Frame
!         Second array: Earth PCK, ITRF93 Frame
!         First value : -8.8283522803609E+08
!         Second value: -8.8283522127455E+08
!         Difference  :  6.7615358829498E+00
!
!         Difference of array elements exceeded tolerance.
!         First array : Earth PCK, ITRF93 Frame
!         Second array: Earth PCK, ITRF93 Frame
!         First value : -8.8274883542767E+08
!         Second value: -8.8274882795439E+08
!         Difference  :  7.4732763767242E+00
!
!         Difference of array elements exceeded tolerance.
!         First array : Earth PCK, ITRF93 Frame
!         Second array: Earth PCK, ITRF93 Frame
!         First value : -8.8266244281925E+08
!         Second value: -8.8266243463423E+08
!         Difference  :  8.1850171089172E+00
!
!         Difference of array elements exceeded tolerance.
!         First array : Earth PCK, ITRF93 Frame
!         Second array: Earth PCK, ITRF93 Frame
!         First value : -8.8257605021083E+08
!         Second value: -8.8257604131407E+08
!         Difference  :  8.8967577219009E+00
!
!         Difference of array elements exceeded tolerance.
!         First array : Earth PCK, ITRF93 Frame
!         Second array: Earth PCK, ITRF93 Frame
!         First value : -8.8248965760241E+08
!         Second value: -8.8248964799391E+08
!         Difference  :  9.6084982156754E+00
!
!         Difference of array elements exceeded tolerance.
!         First array : Earth PCK, ITRF93 Frame
!         Second array: Earth PCK, ITRF93 Frame
!         First value : -8.8240326499398E+08
!         Second value: -8.8240325467375E+08
!
!        [...]
!
!
!        Warning: incomplete output. Only 100 out of 80582 lines have
!        been provided.
!
!
! $ Restrictions
!
!     1)  Calls that do or may change DAF addresses of DAF summaries,
!         names, or data of a given DAF file should not be made during
!         a search of that file initiated by either DAFBFS or DAFBBS.
!         No such changes should be made between the start of a search
!         and calls to any entry point that reads or writes to the
!         summary of the "current array" found by that search, or
!         that returns a "found" flag indicating whether the current
!         array exists.
!
!         Changing the size of the comment area while a search is in
!         progress can invalidate record numbers stored in local data
!         structures of this routine. This can cause corrupted array
!         summaries and names to be returned upon read access and file
!         corruption to occur upon write access.
!
!         Adding arrays (aka "segments") while either a forward or
!         backward search is in progress can cause the search to miss
!         the new segments.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     H.A. Neilan        (JPL)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     F.S. Turner        (JPL)
!     I.M. Underwood     (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 3.2.0, 27-OCT-2021 (JDR) (NJB)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header of DAFFA umbrella routine and all its entry
!        to comply with NAIF standard.
!
!        Updated $Restrictions sections of this routine and its entry
!        points.
!
!-    SPICELIB Version 3.1.1, 14-MAR-2017 (NJB)
!
!        Updated second header code example in this routine: fixed
!        error check for array overflow, corrected indentation of
!        continuation characters, added IMPLICIT NONE, deleted unused
!        declaration, increased buffer size, and changed DAFRDA call to
!        DAFGDA call.
!
!        Updated header example in entry point DAFGH: changed DAFRDA
!        call to DAFGDA call.
!
!-    SPICELIB Version 3.1.0, 10-FEB-2014 (EDW) (BVS)
!
!        Added a functional code example to the $Examples section
!        in DAFBFS, DAFFNA, DAFGS.
!
!        Added check on value of "found" boolean returned from
!        DAFGSR calls. Failure to check this value can cause an
!        infinite loop during segment searches on damaged SPKs.
!
!        Eliminated unneeded $Revisions section.
!
!        Removed the obsolete Reference citation to "NAIF
!        Document 167.0."
!
!        Added full declaration of HANDLE to the $Declarations section
!        of the DAFCS header.
!
!-    SPICELIB Version 3.0.0, 16-NOV-2001 (FST)
!
!        This umbrella and its entry points were updated to
!        work properly with the changes in the DAF system as
!        a result of its utilization of the new handle manager.
!        Calls to DAFRDR were replaced with the translation-aware
!        interface DAFGSR for retrieving summary records from
!        DAFs.
!
!        Updated the entry points of DAFFA to enable its
!        internal state table size, TBSIZE, to be smaller
!        than the file table maintained by DAFAH: FTSIZE.
!
!        Since DAFAH now tracks FTSIZE files as defined in
!        the include file 'zzddhman.inc', it was decided that
!        in the interest of releasing the toolkit this module
!        would undergo simple changes. As such most previous
!        references to FTSIZE in this umbrella have been replaced
!        with TBSIZE where appropriate. DAFBFS and DAFBBS now signal
!        errors if there is not enough room to add a new DAF's
!        dossier to the state table. Also, after attempting to
!        clean up all files listed in the state table that are
!        not currently open, DAFBFS and DAFBBS attempt to locate
!        the first dossier with STADDG set to .FALSE. This is then
!        freed to make room for the new DAF. If DAFBNA fails
!        to locate such a dossier in the state table, it
!        signals the error SPICE(STFULL).
!
!        The parameter FILEN was removed, as it is defined
!        on an environmental basis in the include file
!        'zzddhman.inc'.
!
!-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT)
!
!        Updated to support simultaneous searches of multiple DAFs.
!
!        In previous versions of DAFFA, only one search could be
!        conducted at a time. Therefore, there was no question about
!        which DAF was being operated on by any of the DAFFA entry
!        points that don't accept file handles as input arguments.
!        In the current version of DAFFA, the entry points that don't
!        accept file handles as inputs operate on the `current DAF'.
!        The current DAF is the last one in which a search was
!        started by DAFBFS or DAFBBS, or continued by the new entry
!        point DAFCS. DAFCS was added to allow users to set the
!        current DAF, so that searches of multiple DAFs can be
!        interleaved.
!
!        Note that the notion of `current DAF' as discussed here applies
!        only to DAFs acted upon by entry points of DAFFA. In DAFANA,
!        there is a DAF that is treated as the `current DAF' for
!        adding data; there is no connection between the DAFs regarded
!        as current by DAFFA and DAFANA.
!
!        The two principal changes to DAFFA are the addition of the
!        new entry point DAFCS, and the addition of a data structure
!        called the `state table'. The state table is a collection of
!        parallel arrays that maintain information about the state
!        of each search that is currently in progress. The arrays are
!        indexed by a singly linked list pool; this mechanism allows
!        addition and deletion of information about searches without
!        requiring movement of data already in the state table. The
!        linked list pool contains an `active' list and a `free' list.
!        Nodes in the active list are used to index elements of the
!        state table where data about searches in progress is stored.
!        The head node of the active list is of particular significance:
!        the state information pointed to by this node is that of the
!        current DAF. Nodes in the free list index elements of the
!        state table that are available for use.
!
!        When a search is started on a DAF that is not already `known'
!        to DAFFA, information about the DAF is added to the state
!        table. If there are no free elements in the state table,
!        the routine starting the search (DAFBFS or DAFBBS) will
!        perform garbage collection: the routine will test the handles
!        of each file about which information in stored in the state
!        table to see whether that file is still open. Nodes containing
!        information about DAFs that are no longer open will be moved
!        to the free list.
!
!        Whenever a DAF becomes the current DAF, the linked list
!        that indexes the state table is adjusted so that the
!        information about the current DAF is at the head of the list.
!        This way, a slight efficiency is gained when repeated search
!        accesses are made to the same DAF, since the linear search
!        through the state table for information on that DAF will
!        be shortened.
!
!        Since the algorithms for maintenance of linked lists are well
!        known, they are not documented here. However, see the
!        internals of the SPICELIB routine SPKBSR for a nice diagram
!        describing a similar data structure.
!
!        The state table contains two arrays that are quite large:
!        there are buffers that contain the last character record
!        and summary record read from each DAF. A parallel situation
!        exists in DAFANA, where the name and array summary for each
!        array under construction are buffered. The total storage
!        required for these arrays (in DAFANA and DAFFA together) is
!        4000 * TBSIZE bytes. For this reason, it may be a good idea
!        to reduce the value of TBSIZE in SPICELIB versions for
!        machines where memory is scarce.
!
!-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
!
!        Literature references added to the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!
!-&


! $ Index_Entries
!
!     find DAF array
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               ELEMI
      LOGICAL               FAILED
      LOGICAL               RETURN



!
!     Local parameters
!
      INTEGER               CRLEN
      PARAMETER           ( CRLEN  = 1000 )

      INTEGER               IFNLEN
      PARAMETER           ( IFNLEN =   60 )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL =   -5 )

      INTEGER               MAXNDC
      PARAMETER           ( MAXNDC =  124 )

      INTEGER               DPRSIZ
      PARAMETER           ( DPRSIZ =  128 )

      INTEGER               MAXNIC
      PARAMETER           ( MAXNIC =  250 )

      INTEGER               NIL
      PARAMETER           ( NIL    =   -1 )


!
!     Local variables
!

!
!     State variables.
!
!     These variables define the state of each DAF to which data
!     is currently being added. For each DAF that we're writing to, we
!     maintain a copy of:
!
!        STFH           File handle.
!
!        STPREV         Record number of previous array summary.
!
!        STTHIS         Record number of current array summary.
!
!        STNEXT         Record number of next array summary.
!
!        STNSEG         Number of summaries in current summary record.
!
!        STCURR         Index of current summary within summary record.
!
!        STNR           Last name record read.
!
!        STHVNR         Flag indicating whether name record containing
!                       name of current array is buffered.
!
!        STSR           Last summary record read.
!
!     These variables are maintained in a table of parallel arrays;
!     the size of the table is TBSIZE.
!
      INTEGER               STFH   (         TBSIZE )
      INTEGER               STPREV (         TBSIZE )
      INTEGER               STTHIS (         TBSIZE )
      INTEGER               STNEXT (         TBSIZE )
      INTEGER               STNSEG (         TBSIZE )
      INTEGER               STCURR (         TBSIZE )
      CHARACTER*(CRLEN)     STNR   (         TBSIZE )
      LOGICAL               STHVNR (         TBSIZE )
      DOUBLE PRECISION      STSR   ( DPRSIZ, TBSIZE )

!
!     The table of state variables is indexed by a singly linked list
!     of pointers. This mechanism avoids the work of moving
!     the state variable data about as information about DAFs is
!     added to or deleted from the table.
!
!     The structure containing the linked list pointers is called a
!     `pool'. The pool contains a list of `active' nodes and a list
!     of free nodes. The head nodes of the active and free lists are
!     maintained as the variables STHEAD (`state table head') and
!     STFPTR (`state table free pointer'), respectively. Every node in
!     the pool is on exactly one of these lists.
!
      INTEGER               STPOOL ( TBSIZE )
      INTEGER               STHEAD
      INTEGER               STFPTR

!
!     The pool starts out with all of the nodes on the free list. The
!     first one of DAFBFS or DAFBBS to be called initializes the pool.
!     As new DAFs are searched, DAFBFS and DAFBBS add information about
!     them to the state table. Every time a search is started by DAFBFS
!     or DAFBBS, the routine in question `moves' the DAF's state
!     information to the head of the active list, if the state
!     information is not already there. This re-organization is
!     accomplished by deleting the node for the DAF from its current
!     position in the active list and inserting the node at the head of
!     the list. Thus, the change is made merely by setting pointers,
!     not by moving chunks of data in the state table.
!
!     It may happen that there is no room left in the state table
!     to accommodate information about a new DAF. In this case,
!     garbage collection must be performed:  whichever of DAFBFS or
!     DAFBBS needs more room frees all nodes in the table that index
!     DAFs that are not currently open.
!
!     Note that the routines DAFGS, DAFGN, DAFRS, DAFRN, and DAFWS do
!     not modify the state table; they merely act on the current array
!     in the DAF that is at the head of the active list.
!


!
!     Other local variables
!
      CHARACTER*(FILEN)     DAFNAM
      CHARACTER*(IFNLEN)    IFNAME

      DOUBLE PRECISION      EXDC   ( MAXNDC )
      DOUBLE PRECISION      EXSUM  ( MAXNDC )
      DOUBLE PRECISION      NEWDC  ( MAXNDC )

      INTEGER               BWARD
      INTEGER               EXIC   ( MAXNIC )
      INTEGER               FREE
      INTEGER               FWARD
      INTEGER               I
      INTEGER               NAMSIZ
      INTEGER               ND
      INTEGER               NEWIC  ( MAXNIC )
      INTEGER               NEXTP
      INTEGER               NI
      INTEGER               OPNSET ( LBCELL : FTSIZE )
      INTEGER               P
      INTEGER               PREV
      INTEGER               SUMSIZ

      LOGICAL               FIRST
      LOGICAL               FND
      INTEGER               OFFSET


!
!     Save everything between calls
!
      SAVE


!
!     Initial values
!
      DATA                  FIRST    / .TRUE.             /
      DATA                  STHVNR   /  TBSIZE * .FALSE.  /
      DATA                  STFPTR   /  NIL               /
      DATA                  STHEAD   /  NIL               /


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN  ( 'DAFFA'             )
         CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
         CALL CHKOUT ( 'DAFFA'             )
      END IF

      RETURN




!$Procedure DAFBFS ( DAF, begin forward search )

      ENTRY DAFBFS ( HANDLE )

! $ Abstract
!
!     Begin a forward search for arrays in a DAF.
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
!     DAF
!
! $ Keywords
!
!     FILES
!
! $ Declarations
!
!     INTEGER               HANDLE
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of file to be searched.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of a DAF on which a forward
!              search is to be conducted.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input handle is invalid, an error is signaled by a
!         routine in the call tree of this routine.
!
!     2)  If the first summary record of the DAF file cannot be read,
!         the error SPICE(RECORDNOTFOUND) is signaled.
!
! $ Files
!
!     See argument HANDLE.
!
! $ Particulars
!
!     See DAFFA.
!
! $ Examples
!
!     1) See DAFFA.
!
!     2) Create a simple program to output the double precision and
!        integer values stored in an SPK's segment's descriptors. This
!        program opens a DAF for read, performs a forwards search for
!        the DAF arrays, prints the segment description for each array
!        found, then closes the DAF.
!
!        Use the SPK kernel below as input DAF file for the program.
!
!           de421.bsp
!
!
!        Example code begins here.
!
!
!              PROGRAM DAFBFS_EX1
!              IMPLICIT NONE
!
!        C
!        C     Define the summary parameters appropriate
!        C     for an SPK file.
!        C
!              INTEGER               MAXSUM
!              PARAMETER           ( MAXSUM = 125 )
!
!              INTEGER               ND
!              PARAMETER           ( ND = 2 )
!
!              INTEGER               NI
!              PARAMETER           ( NI = 6 )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(32)        KERNEL
!
!              DOUBLE PRECISION      DC ( ND     )
!              DOUBLE PRECISION      SUM( MAXSUM )
!
!              INTEGER               HANDLE
!              INTEGER               IC( NI )
!
!              LOGICAL               FOUND
!
!
!        C
!        C     Open a DAF for read. Return a HANDLE referring to the
!        C     file.
!        C
!              KERNEL = 'de421.bsp'
!              CALL DAFOPR ( KERNEL, HANDLE )
!
!        C
!        C     Begin a forward search on the file.
!        C
!              CALL DAFBFS ( HANDLE )
!
!        C
!        C     Search until a DAF array is found.
!        C
!              CALL DAFFNA ( FOUND )
!
!        C
!        C     Loop while the search finds subsequent DAF arrays.
!        C
!              DO WHILE ( FOUND )
!
!                 CALL DAFGS ( SUM )
!                 CALL DAFUS ( SUM, ND, NI, DC, IC )
!
!                 WRITE(*,*)                'Doubles:', DC(1:ND)
!                 WRITE(*, FMT='(A,6I9)' ) 'Integers:', IC(1:NI)
!
!        C
!        C        Check for another segment.
!        C
!                 CALL DAFFNA ( FOUND )
!
!              END DO
!
!        C
!        C     Safely close the DAF.
!        C
!              CALL DAFCLS ( HANDLE )
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:        1        0        1        2      641   310404
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:        2        0        1        2   310405   423048
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:        3        0        1        2   423049   567372
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:        4        0        1        2   567373   628976
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:        5        0        1        2   628977   674740
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:        6        0        1        2   674741   715224
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:        7        0        1        2   715225   750428
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:        8        0        1        2   750429   785632
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:        9        0        1        2   785633   820836
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:       10        0        1        2   820837   944040
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:      301        3        1        2   944041  1521324
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:      399        3        1        2  1521325  2098608
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:      199        1        1        2  2098609  2098620
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:      299        2        1        2  2098621  2098632
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:      499        4        1        2  2098633  2098644
!
!
!        Note, the final entries in the integer array contains the
!        segment start/end indexes. The output indicates the search
!        proceeded from the start of the file (low value index) towards
!        the end (high value index).
!
! $ Restrictions
!
!     1)  Calls that do or may change DAF addresses of DAF summaries,
!         names, or data of a given DAF file should not be made during
!         a search of that file initiated by either DAFBFS or DAFBBS.
!         No such changes should be made between the start of a search
!         and calls to any entry point that reads or writes to the
!         summary of the "current array" found by that search, or
!         that returns a "found" flag indicating whether the current
!         array exists.
!
!         Changing the size of the comment area while a search is in
!         progress can invalidate record numbers stored in local data
!         structures of this routine. This can cause corrupted array
!         summaries and names to be returned upon read access and file
!         corruption to occur upon write access.
!
!         Adding arrays (aka "segments") while either a forward or
!         backward search is in progress can cause the search to miss
!         the new segments.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.1, 26-OCT-2021 (JDR) (NJB)
!
!        Edited the header to comply with NAIF standard.
!        Added undeclared variables to code example.
!
!        Updated $Restrictions section.
!
!-    SPICELIB Version 2.1.0, 10-OCT-2012 (EDW)
!
!        Added a functional code example to the $Examples section.
!
!        Added check on value of "found" boolean returned from
!        DAFGSR calls. Failure to check this value can cause an
!        infinite loop during segment searches on damaged SPKs.
!
!        Eliminated unneeded $Revisions section.
!
!        Removed the obsolete Reference citation to "NAIF
!        Document 167.0."
!
!-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT)
!
!        Updated to support simultaneous searches of multiple DAFs.
!
!        This routine now operates on the current DAF---the one at
!        the head of the active list. All saved state variables
!        used by this routine are now part of the state table, or
!        its associated set of pointers.
!
!        Also, the $Exceptions section was filled out.
!
!-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
!
!        Literature references added to the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!
!-&


! $ Index_Entries
!
!     begin DAF forward search
!
!-&


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFBFS' )
      END IF

!
!     Check out the file handle before going any further.
!
      CALL DAFSIH ( HANDLE, 'READ' )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DAFBFS' )
         RETURN
      END IF

!
!     Initialize the state table pool, if this hasn't been done yet.
!     Also initialize the cell used to obtain the set of handles of
!     open DAFs.
!
      IF ( FIRST ) THEN

         CALL SSIZEI ( FTSIZE, OPNSET )

         DO I = 1,  TBSIZE - 1
            STPOOL(I)   =  I + 1
         END DO

         STPOOL(TBSIZE) =  NIL
         STFPTR         =  1
         FIRST          = .FALSE.

      END IF

!
!     See whether we already have an entry for this DAF in the
!     state table. Find the previous node if possible.
!
      P      =   STHEAD
      PREV   =   NIL
      FND    =  .FALSE.

      DO WHILE (  ( P .NE. NIL )  .AND.  ( .NOT. FND )  )

         IF ( STFH(P) .EQ. HANDLE ) THEN
            FND   =  .TRUE.
         ELSE
            PREV  =  P
            P     =  STPOOL( P )
         END IF

      END DO

!
!     At this point, either FND is false, or P points to a
!     state table entry describing the DAF indicated by HANDLE.
!     In the latter case, PREV is the predecessor of P.
!
      IF ( FND ) THEN
!
!        We already have a dossier on this DAF. We already have
!        the information on the summary format, but we must re-set
!        our summary record pointers and our name record availability
!        flag.
!
!        Rather than doing the update here, we do it outside of this
!        IF block. That way, the update gets done in just one place.
!        This just makes life easier:  if the collection of state
!        variables is changed, there are fewer places to forget to
!        make the required code changes.
!
!        Move the node for this DAF to the head of the active list,
!        if it is not already there:
!
!           - Make the predecessor of P point to the successor of P.
!
!           - Make P point to the head of the active list.
!
!           - Make P the active list head node.
!
!
         IF ( P .NE. STHEAD ) THEN
!
!           P is in the active list, but is not at the head. So,
!           the predecessor of P is not NIL.
!
            STPOOL(PREV)  =  STPOOL(P)
            STPOOL(P)     =  STHEAD
            STHEAD        =  P

         END IF


      ELSE
!
!        We don't yet have any information on this DAF. Make a new
!        state table entry for the DAF. We may need to make room for
!        the new information by freeing space allocated to DAFs that
!        are no longer open.
!
         IF ( STFPTR .EQ. NIL ) THEN
!
!           Oops, we're out of space. Time for garbage collection.
!           Test each file handle to see whether it designates a DAF
!           that is still open. DAFHOF will tell us which handles
!           point to open DAFs.
!
            CALL DAFHOF ( OPNSET )

            P     =  STHEAD
            PREV  =  NIL
!
!           For every DAF file represented in the state table, we'll
!           delete the corresponding state information if the DAF is
!           now closed. We traverse the active list, examining each
!           file handle as we go.
!
            DO WHILE ( P .NE. NIL )

               IF (  ELEMI ( STFH(P), OPNSET )   )  THEN
!
!                 The file is open. Have a look at the next node.
!
                  PREV = P
                  P    = STPOOL( P )

               ELSE
!
!                 This file handle is not on the list, so free the
!                 node pointing to the information about the DAF it
!                 designated:
!
!                    - Save the successor of P.
!
!                    - Link the predecessor of node P to the successor
!                      of P, if the predecessor is not NIL.
!
!                    - If it happens that P is the head node of the
!                      active list, set the head equal to the
!                      successor of P.
!
!                    - Link P into the free list.
!
!                    - Set P equal to its saved successor.
!
!                    - (PREV remains unchanged.)
!
!
                  NEXTP  =  STPOOL( P )


                  IF ( P .EQ. STHEAD ) THEN
!
!                    Re-assign STHEAD so that we don't lose the head
!                    of the active list. P has no predecessor in this
!                    case, so there's no need to set the forward pointer
!                    of node PREV.
!
                     STHEAD = NEXTP

                  ELSE
!
!                    Since P is not the head node of the active list,
!                    PREV is not NIL, so we'll need to set the forward
!                    pointer of node PREV.
!
                     STPOOL( PREV ) = NEXTP

                  END IF

                  STPOOL( P )  =  STFPTR
                  STFPTR       =  P
                  P            =  NEXTP

               END IF

            END DO
!
!           At this point, we've freed all nodes from the active
!           list that were used to index information about DAFs that
!           are no longer open. If there's any more room in the state
!           table, we have it now.
!
         END IF

!
!        If there still is no room, there is a bug in DAFAH, since DAFAH
!        should not allow more than TBSIZE DAFs to be open. So, we
!        assume that we've found some room. The first free node is
!        indicated by STFPTR. We'll allocate this node and use it to
!        index the state information for the new DAF.
!
         P  =  STFPTR

!
!        Update the free list pointer, link P to the previous head
!        of the active list, and make P the head of the active list.
!
         STFPTR       =  STPOOL( P )
         STPOOL( P )  =  STHEAD
         STHEAD       =  P

      END IF

!
!     At this point, P is the head node of the active list, and P is
!     the index in the state table of the information for the current
!     DAF.
!

!
!     Read the file record and first summary record. Do not read the
!     corresponding name record until necessary. In most searches,
!     names are of no interest.
!
      CALL DAFRFR ( HANDLE, ND, NI, IFNAME, FWARD, BWARD, FREE )
      CALL DAFGSR ( HANDLE, FWARD, 1, 128, STSR(1,P), FND )

      IF ( .NOT. FND ) THEN

         CALL DAFHFN ( STFH(P), DAFNAM )

         CALL SETMSG ( 'Attempt to read descriptor record # '    // &
                       'of DAF ''#'' failed; record was '        // &
                       'not found. This condition may indicate ' // &
                       'a corrupted DAF.' )
         CALL ERRINT ( '#', STNEXT(P)                            )
         CALL ERRCH  ( '#', DAFNAM                               )
         CALL SIGERR ( 'SPICE(RECORDNOTFOUND)'                   )
         CALL CHKOUT ( 'DAFBFS'                                  )
         RETURN

      END IF

!
!     Set up the state information for this file. Note that we
!     don't have a name record yet, and we have no current array
!     yet.
!
      STFH  ( P )  =  HANDLE
      STTHIS( P )  =  FWARD
      STNEXT( P )  =  INT ( STSR(1,P) )
      STPREV( P )  =  INT ( STSR(2,P) )
      STNSEG( P )  =  INT ( STSR(3,P) )
      STHVNR( P )  = .FALSE.

!
!     The arrays are returned in forward order within each summary
!     record.
!
      STCURR( P )  =  0

      CALL CHKOUT ( 'DAFBFS' )
      RETURN




!$Procedure DAFFNA ( DAF, find next array )

      ENTRY DAFFNA ( FOUND )

! $ Abstract
!
!     Find the next (forward) array in the current DAF.
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
!     DAF
!
! $ Keywords
!
!     FILES
!
! $ Declarations
!
!     LOGICAL               FOUND
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FOUND      O   .TRUE. if an array was found.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     FOUND    is .TRUE. if an array was found, and is .FALSE. if,
!              when this routine is called, the current array is
!              the tail of the array list. (Recall that the
!              arrays in a DAF may be viewed as a doubly linked
!              list, with the tail being the last array in the file.)
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If this routine is called before a search is begun, the
!         error SPICE(DAFNOSEARCH) is signaled.
!
!     2)  If the DAF to be searched has actually been closed, an error
!         is signaled by a routine in the call tree of this routine.
!
!     3)  If the end of the array list has already been reached when
!         this routine is called, this routine has no effect.
!
!     4)  If the summary record of the next array (aka "segment") in
!         the DAF file cannot be read, the error SPICE(RECORDNOTFOUND)
!         is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     See DAFFA.
!
! $ Examples
!
!     1) See DAFFA.
!
!     2) Use a simple routine to output the double precision and
!        integer values stored in an SPK's segment's descriptors. This
!        function opens a DAF for read, performs a forwards search for
!        the DAF arrays, prints the segment descriptor for each array
!        found, then closes the DAF.
!
!        Use the SPK kernel below as input DAF file for the program.
!
!           de421.bsp
!
!
!        Example code begins here.
!
!
!              PROGRAM DAFFNA_EX1
!              IMPLICIT NONE
!
!        C
!        C     Define the summary parameters appropriate
!        C     for an SPK file.
!        C
!              INTEGER               MAXSUM
!              PARAMETER           ( MAXSUM = 125 )
!
!              INTEGER               ND
!              PARAMETER           ( ND = 2 )
!
!              INTEGER               NI
!              PARAMETER           ( NI = 6 )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(32)        KERNEL
!
!              DOUBLE PRECISION      DC ( ND     )
!              DOUBLE PRECISION      SUM( MAXSUM )
!
!              INTEGER               HANDLE
!              INTEGER               IC( NI )
!
!              LOGICAL               FOUND
!
!
!        C
!        C     Open a DAF for read. Return a HANDLE referring to the
!        C     file.
!        C
!              KERNEL = 'de421.bsp'
!              CALL DAFOPR ( KERNEL, HANDLE )
!
!        C
!        C     Begin a forward search on the file.
!        C
!              CALL DAFBFS ( HANDLE )
!
!        C
!        C     Search until a DAF array is found.
!        C
!              CALL DAFFNA ( FOUND )
!
!        C
!        C     Loop while the search finds subsequent DAF arrays.
!        C
!              DO WHILE ( FOUND )
!
!                 CALL DAFGS ( SUM )
!                 CALL DAFUS ( SUM, ND, NI, DC, IC )
!
!                 WRITE(*,*)                'Doubles:', DC(1:ND)
!                 WRITE(*, FMT='(A,6I9)' ) 'Integers:', IC(1:NI)
!
!        C
!        C        Check for another segment.
!        C
!                 CALL DAFFNA ( FOUND )
!
!              END DO
!
!        C
!        C     Safely close the DAF.
!        C
!              CALL DAFCLS ( HANDLE )
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:        1        0        1        2      641   310404
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:        2        0        1        2   310405   423048
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:        3        0        1        2   423049   567372
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:        4        0        1        2   567373   628976
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:        5        0        1        2   628977   674740
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:        6        0        1        2   674741   715224
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:        7        0        1        2   715225   750428
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:        8        0        1        2   750429   785632
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:        9        0        1        2   785633   820836
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:       10        0        1        2   820837   944040
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:      301        3        1        2   944041  1521324
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:      399        3        1        2  1521325  2098608
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:      199        1        1        2  2098609  2098620
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:      299        2        1        2  2098621  2098632
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:      499        4        1        2  2098633  2098644
!
!
!        Note, the final entries in the integer array contains the
!        segment start/end indexes. The output indicates the search
!        proceeded from the start of the file (low value index) towards
!        the end (high value index).
!
! $ Restrictions
!
!     1)  Calls that do or may change DAF addresses of DAF summaries,
!         names, or data of a given DAF file should not be made during
!         a search of that file initiated by either DAFBFS or DAFBBS.
!         No such changes should be made between the start of a search
!         and calls to any entry point that reads or writes to the
!         summary of the "current array" found by that search, or
!         that returns a "found" flag indicating whether the current
!         array exists.
!
!         Changing the size of the comment area while a search is in
!         progress can invalidate record numbers stored in local data
!         structures of this routine. This can cause corrupted array
!         summaries and names to be returned upon read access and file
!         corruption to occur upon write access.
!
!         Adding arrays (aka "segments") while either a forward or
!         backward search is in progress can cause the search to miss
!         the new segments.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.1, 26-OCT-2021 (JDR) (NJB)
!
!        Edited the header to comply with NAIF standard.
!        Added undeclared variables to code example.
!
!        Updated $Restrictions section.
!
!-    SPICELIB Version 2.1.0, 10-OCT-2012 (EDW)
!
!        Added a functional code example to the $Examples section.
!
!        Added check on value of "found" boolean returned from
!        DAFGSR calls. Failure to check this value can cause an
!        infinite loop during segment searches on damaged SPKs.
!
!        Eliminated unneeded $Revisions section.
!
!        Removed the obsolete Reference citation to "NAIF
!        Document 167.0."
!
!-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT)
!
!        Updated to support simultaneous searches of multiple DAFs.
!
!        This routine now operates on the current DAF---the one at
!        the head of the active list. All saved state variables
!        used by this routine are now part of the state table, or
!        its associated set of pointers.
!
!-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
!
!        Literature references added to the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!
!-&


! $ Index_Entries
!
!     find next DAF array
!
!-&


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFFNA' )
      END IF


!
!     FOUND will be false until we make it past the error checks.
!
      FOUND = .FALSE.

!
!     Operate on the last DAF in which a search has been started.
!
      P  =  STHEAD

!
!     Make sure that a search has been started in this DAF.
!
      IF ( P .EQ. NIL ) THEN

         CALL SETMSG ( 'No DAF is currently being searched.' )
         CALL SIGERR ( 'SPICE(DAFNOSEARCH)'                  )
         CALL CHKOUT ( 'DAFFNA'                              )
         RETURN

!
!     Make sure that the `current' DAF is still open.
!
      ELSE

         CALL DAFSIH ( STFH(P), 'READ' )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'DAFFNA' )
            RETURN
         END IF

      END IF


!
!     Now that we know a search is going on, assume that we will find
!     an array until proven otherwise.
!
      FOUND = .TRUE.


!
!     Either there are more summaries left in this record, or
!     there aren't. If there are, just incrementing the pointer
!     is sufficient. If there aren't, we have to find the next
!     record and point to the first array there. (If that
!     record is empty, or doesn't exist, then there are simply
!     no more arrays to be found.)
!

      STCURR(P) = STCURR(P) + 1

      IF ( STCURR(P) .GT. STNSEG(P) ) THEN

         IF ( STNEXT(P) .EQ. 0 ) THEN
!
!           There are no more arrays in the list.
!
            FOUND     =  .FALSE.
!
!           Make sure that the array pointer stays pointing to
!           the position following the end of the list. Otherwise,
!           a call to DAFFPA might fail to find the last array in
!           the list.
!
            STCURR(P) =   STNSEG(P) + 1
!
!           The careful reader may note that we're not updating any
!           of the pointers
!
!              STTHIS
!              STNEXT
!              STPREV
!
!           These will not be accessed if there is no current array.
!           If the array pointer is backed up again by a call to
!           DAFFPA, the values we have right now will be correct.
!
         ELSE

            CALL DAFGSR ( STFH(P), STNEXT(P), 1, 128, STSR(1,P), FND )

            IF ( .NOT. FND ) THEN

               CALL DAFHFN ( STFH(P), DAFNAM )

               CALL SETMSG ( 'Attempt to read descriptor record # '   // &
                             'of DAF ''#'' failed; record was '       // &
                             'not found. This condition may indicate '// &
                             'a corrupted DAF.' )
               CALL ERRINT ( '#', STNEXT(P)                            )
               CALL ERRCH  ( '#', DAFNAM                               )
               CALL SIGERR ( 'SPICE(RECORDNOTFOUND)'                   )
               CALL CHKOUT ( 'DAFFNA'                                  )
               RETURN

            END IF

!
!           The name (character) record we've saved no longer applies
!           to the current summary record. However, we've just updated
!           the summary record, so the summary record remains valid.
!
            STHVNR( P )  = .FALSE.

            STTHIS( P )  =  STNEXT(P)
            STNEXT( P )  =  INT ( STSR(1,P) )
            STPREV( P )  =  INT ( STSR(2,P) )
            STNSEG( P )  =  INT ( STSR(3,P) )
            STCURR( P )  =  1

            FOUND        = ( STNSEG(P) .GT. 0 )

         END IF

      END IF

      CALL CHKOUT ( 'DAFFNA' )
      RETURN




!$Procedure DAFBBS ( DAF, begin backward search )

      ENTRY DAFBBS ( HANDLE )

! $ Abstract
!
!     Begin a backward search for arrays in a DAF.
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
!     DAF
!
! $ Keywords
!
!     FILES
!
! $ Declarations
!
!     INTEGER               HANDLE
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of DAF to be searched.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of a DAF on which a backward
!              search is to be conducted.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input handle is invalid, an error is signaled by a
!         routine in the call tree of this routine.
!
!     2)  If the last summary record of the DAF file cannot be read,
!         the error SPICE(RECORDNOTFOUND) is signaled.
!
! $ Files
!
!     See argument HANDLE.
!
! $ Particulars
!
!     See DAFFA.
!
! $ Examples
!
!     1) See DAFFA.
!
!     2) Create a simple routine to output the double precision and
!        integer values stored in an SPK's segment's descriptors. This
!        program opens a DAF for read, performs a backward search for
!        the DAF arrays, prints the segment descriptor for each array
!        found, then closes the DAF.
!
!        Use the SPK kernel below as input DAF file for the program.
!
!           de421.bsp
!
!
!        Example code begins here.
!
!
!              PROGRAM DAFBBS_EX1
!              IMPLICIT NONE
!
!        C
!        C     Define the summary parameters appropriate
!        C     for an SPK file.
!        C
!              INTEGER               MAXSUM
!              PARAMETER           ( MAXSUM = 125 )
!
!              INTEGER               ND
!              PARAMETER           ( ND = 2 )
!
!              INTEGER               NI
!              PARAMETER           ( NI = 6 )
!
!        C
!        C     Local variables
!        C
!              CHARACTER*(32)        KERNEL
!
!              DOUBLE PRECISION      DC ( ND     )
!              DOUBLE PRECISION      SUM( MAXSUM )
!
!              INTEGER               HANDLE
!              INTEGER               IC( NI )
!
!              LOGICAL               FOUND
!
!
!        C
!        C     Open a DAF for read. Return a HANDLE referring to the
!        C     file.
!        C
!              KERNEL = 'de421.bsp'
!              CALL DAFOPR ( KERNEL, HANDLE )
!
!        C
!        C     Begin a backward search on the file.
!        C
!              CALL DAFBBS ( HANDLE )
!
!        C
!        C     Search until a DAF array is found.
!        C
!              CALL DAFFPA ( FOUND )
!
!        C
!        C     Loop while the search finds subsequent DAF arrays.
!        C
!              DO WHILE ( FOUND )
!
!                 CALL DAFGS ( SUM )
!                 CALL DAFUS ( SUM, ND, NI, DC, IC )
!
!                 WRITE(*,*)                'Doubles:', DC(1:ND)
!                 WRITE(*, FMT='(A,6I9)' ) 'Integers:', IC(1:NI)
!
!        C
!        C        Check for another segment.
!        C
!                 CALL DAFFPA ( FOUND )
!
!              END DO
!
!        C
!        C     Safely close the DAF.
!        C
!              CALL DAFCLS ( HANDLE )
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:      499        4        1        2  2098633  2098644
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:      299        2        1        2  2098621  2098632
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:      199        1        1        2  2098609  2098620
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:      399        3        1        2  1521325  2098608
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:      301        3        1        2   944041  1521324
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:       10        0        1        2   820837   944040
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:        9        0        1        2   785633   820836
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:        8        0        1        2   750429   785632
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:        7        0        1        2   715225   750428
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:        6        0        1        2   674741   715224
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:        5        0        1        2   628977   674740
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:        4        0        1        2   567373   628976
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:        3        0        1        2   423049   567372
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:        2        0        1        2   310405   423048
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:        1        0        1        2      641   310404
!
!
!        Note, the final entries in the integer arrays record the
!        segment start/end indexes. The output indicates the search
!        proceeded from the end of the file (high value index) towards
!        the beginning (low value index).
!
! $ Restrictions
!
!     1)  Calls that do or may change DAF addresses of DAF summaries,
!         names, or data of a given DAF file should not be made during
!         a search of that file initiated by either DAFBFS or DAFBBS.
!         No such changes should be made between the start of a search
!         and calls to any entry point that reads or writes to the
!         summary of the "current array" found by that search, or
!         that returns a "found" flag indicating whether the current
!         array exists.
!
!         Changing the size of the comment area while a search is in
!         progress can invalidate record numbers stored in local data
!         structures of this routine. This can cause corrupted array
!         summaries and names to be returned upon read access and file
!         corruption to occur upon write access.
!
!         Adding arrays (aka "segments") while either a forward or
!         backward search is in progress can cause the search to miss
!         the new segments.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.1, 26-OCT-2021 (JDR) (NJB)
!
!        Edited the header to comply with NAIF standard.
!        Added undeclared variables to code example.
!
!        Updated $Restrictions section.
!
!-    SPICELIB Version 2.1.0, 10-OCT-2012 (EDW)
!
!        Added a functional code example to the $Examples section.
!
!        Added check on value of "found" boolean returned from
!        DAFGSR calls. Failure to check this value can cause an
!        infinite loop during segment searches on damaged SPKs.
!
!        Eliminated unneeded $Revisions section.
!
!        Removed the obsolete Reference citation to "NAIF
!        Document 167.0."
!
!-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT)
!
!        Updated to support simultaneous searches of multiple DAFs.
!
!        This routine now makes the DAF designated by HANDLE the
!        current DAF---the one at the head of the active list. All
!        saved state variables used by this routine are now part of the
!        state table, or its associated set of pointers.
!
!        Also, the $Exceptions section was filled out.
!
!-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
!
!        Literature references added to the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!
!-&


! $ Index_Entries
!
!     begin DAF backward search
!
!-&


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFBBS' )
      END IF

!
!     Check out the file handle before going any further.
!
      CALL DAFSIH ( HANDLE, 'READ' )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DAFBBS' )
         RETURN
      END IF

!
!     Initialize the state table pool, if this hasn't been done yet.
!     Also initialize the cell used to obtain the set of handles of
!     open DAFs.
!
      IF ( FIRST ) THEN

         CALL SSIZEI ( FTSIZE, OPNSET )

         DO I = 1,  TBSIZE - 1
            STPOOL(I)   =  I + 1
         END DO

         STPOOL(TBSIZE) =  NIL
         STFPTR         =  1
         FIRST          = .FALSE.

      END IF

!
!     See whether we already have an entry for this DAF in the
!     state table. Find the previous node if possible.
!
      P     =   STHEAD
      PREV  =   NIL
      FND   =  .FALSE.

      DO WHILE (  ( P .NE. NIL )  .AND.  ( .NOT. FND )  )

         IF ( STFH(P) .EQ. HANDLE ) THEN
            FND   =  .TRUE.
         ELSE
            PREV  =  P
            P     =  STPOOL( P )
         END IF

      END DO

!
!     At this point, either FND is false, or P points to a
!     state table entry describing the DAF indicated by HANDLE.
!     In the latter case, PREV is the predecessor of P.
!
      IF ( FND ) THEN
!
!        We already have a dossier on this DAF. We already have
!        the information on the summary format, but we must re-set
!        our summary record pointers and our name record availability
!        flag.
!
!        Rather than doing the update here, we do it outside of this
!        IF block. That way, the update gets done in just one place.
!        This just makes life easier:  if the collection of state
!        variables is changed, there are fewer places to forget to
!        make the required code changes.
!
!        Move the node for this DAF to the head of the active list,
!        if it is not already there:
!
!           - Make the predecessor of P point to the successor of P.
!
!           - Make P point to the head of the active list.
!
!           - Make P the active list head node.
!
!
         IF ( P .NE. STHEAD ) THEN
!
!           P is in the active list, but is not at the head. So,
!           the predecessor of P is not NIL.
!
            STPOOL(PREV)  =  STPOOL(P)
            STPOOL(P)     =  STHEAD
            STHEAD        =  P

         END IF


      ELSE
!
!        We don't yet have any information on this DAF. Make a new
!        state table entry for the DAF. We may need to make room for
!        the new information by freeing space allocated to DAFs that
!        are no longer open.
!
         IF ( STFPTR .EQ. NIL ) THEN
!
!           Oops, we're out of space. Time for garbage collection.
!           Test each file handle to see whether it designates a DAF
!           that is still open. DAFHOF will tell us which handles
!           point to open DAFs.
!
            CALL DAFHOF ( OPNSET )

            P     =  STHEAD
            PREV  =  NIL
!
!           For every DAF file represented in the state table, we'll
!           delete the corresponding state information if the DAF is
!           now closed. We traverse the active list, examining each
!           file handle as we go.
!
            DO WHILE ( P .NE. NIL )

               IF (  ELEMI ( STFH(P), OPNSET )   )  THEN
!
!                 The file is open. Have a look at the next node.
!
                  PREV = P
                  P    = STPOOL( P )

               ELSE
!
!                 This file handle is not on the list, so free the
!                 node pointing to the information about the DAF it
!                 designated:
!
!                    - Save the successor of P.
!
!                    - Link the predecessor of node P to the successor
!                      of P, if the predecessor is not NIL.
!
!                    - If it happens that P is the head node of the
!                      active list, set the head equal to the
!                      successor of P.
!
!                    - Link P into the free list.
!
!                    - Set P equal to its saved successor.
!
!                    - (PREV remains unchanged.)
!
!
                  NEXTP  =  STPOOL( P )


                  IF ( P .EQ. STHEAD ) THEN
!
!                    Re-assign STHEAD so that we don't lose the head
!                    of the active list. P has no predecessor in this
!                    case, so there's no need to set the forward pointer
!                    of node PREV.
!
                     STHEAD = NEXTP

                  ELSE
!
!                    Since P is not the head node of the active list,
!                    PREV is not NIL, so we'll need to set the forward
!                    pointer of node PREV.
!
                     STPOOL( PREV ) = NEXTP

                  END IF


                  STPOOL( P )  =  STFPTR
                  STFPTR       =  P
                  P            =  NEXTP

               END IF

            END DO
!
!           At this point, we've freed all nodes from the active
!           list that were used to index information about DAFs that
!           are no longer open. If there's any more room in the state
!           table, we have it now.
!
         END IF

!
!        If there still is no room, there is a bug in DAFAH, since DAFAH
!        should not allow more than TBSIZE DAFs to be open. So, we
!        assume that we've found some room. The first free node is
!        indicated by STFPTR. We'll allocate this node and use it to
!        index the state information for the new DAF.
!
         P  =  STFPTR
!
!        Update the free list pointer, link P to the previous head
!        of the active list, and make P the head of the active list.
!
         STFPTR       =  STPOOL( P )
         STPOOL( P )  =  STHEAD
         STHEAD       =  P

      END IF

!
!     At this point, P is the head node of the active list, and P is
!     the index in the state table of the information for the current
!     DAF.
!

!
!     Read the file record and last summary record. Do not read the
!     corresponding name record until necessary. In most searches,
!     names are of no interest.
!
      CALL DAFRFR ( HANDLE, ND, NI, IFNAME, FWARD, BWARD, FREE )
      CALL DAFGSR ( HANDLE, BWARD, 1, 128, STSR(1,P), FND )

      IF ( .NOT. FND ) THEN

         CALL DAFHFN ( STFH(P), DAFNAM )

         CALL SETMSG ( 'Attempt to read descriptor record # '    // &
                       'of DAF ''#'' failed; record was '        // &
                       'not found. This condition may indicate ' // &
                       'a corrupted DAF.' )
         CALL ERRINT ( '#', STNEXT(P)                            )
         CALL ERRCH  ( '#', DAFNAM                               )
         CALL SIGERR ( 'SPICE(RECORDNOTFOUND)'                   )
         CALL CHKOUT ( 'DAFBBS'                                  )
         RETURN

      END IF

      STFH  ( P )  =  HANDLE
      STTHIS( P )  =  BWARD
      STNEXT( P )  =  INT ( STSR(1,P) )
      STPREV( P )  =  INT ( STSR(2,P) )
      STNSEG( P )  =  INT ( STSR(3,P) )
      STHVNR( P )  = .FALSE.

!
!     The arrays are returned in backward order from each summary
!     record.
!
      STCURR( P )  =  STNSEG( P ) + 1

      CALL CHKOUT ( 'DAFBBS' )
      RETURN




!$Procedure DAFFPA ( DAF, find previous array )

      ENTRY DAFFPA ( FOUND )

! $ Abstract
!
!     Find the previous (backward) array in the current DAF.
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
!     DAF
!
! $ Keywords
!
!     FILES
!
! $ Declarations
!
!     LOGICAL               FOUND
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FOUND      O   .TRUE. if an array was found.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     FOUND    is .TRUE. if an array was found, and is .FALSE. if,
!              when this routine is called, the current array is
!              the head of the array list. (Recall that the
!              arrays in a DAF may be viewed as a doubly linked
!              list, with the head being the first array in the
!              file.)
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If this routine is called before a search is begun, the
!         error SPICE(DAFNOSEARCH) is signaled.
!
!     2)  If the DAF to be searched has actually been closed, an error
!         is signaled by a routine in the call tree of this routine.
!
!     3)  If the beginning of the array list has already been reached
!         when this routine is called, this routine will not change the
!         current array. FOUND will be false on output.
!
!     4)  If the summary record of the previous array (aka "segment")
!         in the DAF file cannot be read, the error
!         SPICE(RECORDNOTFOUND) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     See DAFFA.
!
! $ Examples
!
!     1) See DAFFA.
!
!     2) Use a simple routine to output the double precision and
!        integer values stored in an SPK's segment's descriptors. This
!        function opens a DAF for read, performs a backward search for
!        the DAF arrays, prints the segment descriptor for each array
!        found, then closes the DAF.
!
!        Use the SPK kernel below as input DAF file for the program.
!
!           de421.bsp
!
!
!        Example code begins here.
!
!
!              PROGRAM DAFFPA_EX1
!              IMPLICIT NONE
!
!        C
!        C     Define the summary parameters appropriate
!        C     for an SPK file.
!        C
!              INTEGER               MAXSUM
!              PARAMETER           ( MAXSUM = 125 )
!
!              INTEGER               ND
!              PARAMETER           ( ND = 2 )
!
!              INTEGER               NI
!              PARAMETER           ( NI = 6 )
!
!        C
!        C     Local variables
!        C
!              CHARACTER*(32)        KERNEL
!
!              DOUBLE PRECISION      DC ( ND     )
!              DOUBLE PRECISION      SUM( MAXSUM )
!
!              INTEGER               HANDLE
!              INTEGER               IC( NI )
!
!              LOGICAL               FOUND
!
!
!        C
!        C     Open a DAF for read. Return a HANDLE referring to the
!        C     file.
!        C
!              KERNEL = 'de421.bsp'
!              CALL DAFOPR ( KERNEL, HANDLE )
!
!        C
!        C     Begin a backward search on the file.
!        C
!              CALL DAFBBS ( HANDLE )
!
!        C
!        C     Search until a DAF array is found.
!        C
!              CALL DAFFPA ( FOUND )
!
!        C
!        C     Loop while the search finds subsequent DAF arrays.
!        C
!              DO WHILE ( FOUND )
!
!                 CALL DAFGS ( SUM )
!                 CALL DAFUS ( SUM, ND, NI, DC, IC )
!
!                 WRITE(*,*)                'Doubles:', DC(1:ND)
!                 WRITE(*, FMT='(A,6I9)' ) 'Integers:', IC(1:NI)
!
!        C
!        C        Check for another segment.
!        C
!                 CALL DAFFPA ( FOUND )
!
!              END DO
!
!        C
!        C     Safely close the DAF.
!        C
!              CALL DAFCLS ( HANDLE )
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:      499        4        1        2  2098633  2098644
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:      299        2        1        2  2098621  2098632
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:      199        1        1        2  2098609  2098620
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:      399        3        1        2  1521325  2098608
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:      301        3        1        2   944041  1521324
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:       10        0        1        2   820837   944040
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:        9        0        1        2   785633   820836
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:        8        0        1        2   750429   785632
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:        7        0        1        2   715225   750428
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:        6        0        1        2   674741   715224
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:        5        0        1        2   628977   674740
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:        4        0        1        2   567373   628976
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:        3        0        1        2   423049   567372
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:        2        0        1        2   310405   423048
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:        1        0        1        2      641   310404
!
!
!        Note, the final entries in the integer arrays record the
!        segment start/end indexes. The output indicates the search
!        proceeded from the end of the file (high value index) towards
!        the beginning (low value index).
!
! $ Restrictions
!
!     1)  Calls that do or may change DAF addresses of DAF summaries,
!         names, or data of a given DAF file should not be made during
!         a search of that file initiated by either DAFBFS or DAFBBS.
!         No such changes should be made between the start of a search
!         and calls to any entry point that reads or writes to the
!         summary of the "current array" found by that search, or
!         that returns a "found" flag indicating whether the current
!         array exists.
!
!         Changing the size of the comment area while a search is in
!         progress can invalidate record numbers stored in local data
!         structures of this routine. This can cause corrupted array
!         summaries and names to be returned upon read access and file
!         corruption to occur upon write access.
!
!         Adding arrays (aka "segments") while either a forward or
!         backward search is in progress can cause the search to miss
!         the new segments.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.1, 26-OCT-2021 (JDR) (NJB)
!
!        Edited the header to comply with NAIF standard.
!        Added undeclared variables to code example.
!
!        Updated $Restrictions section.
!
!-    SPICELIB Version 2.1.0, 10-OCT-2012 (EDW)
!
!        Added a functional code example to the $Examples section.
!
!        Added check on value of "found" boolean returned from
!        DAFGSR calls. Failure to check this value can cause an
!        infinite loop during segment searches on damaged SPKs.
!
!        Eliminated unneeded $Revisions section.
!
!        Removed the obsolete Reference citation to "NAIF
!        Document 167.0."
!
!-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT)
!
!        Updated to support simultaneous searches of multiple DAFs.
!
!        This routine now operates on the current DAF---the one at
!        the head of the active list. All saved state variables
!        used by this routine are now part of the state table, or
!        its associated set of pointers.
!
!        Also, a bug fix was made to the array pointer adjustment
!        algorithm: the pointer is no longer decremented if it
!        is already less than 1 and the array summary pointer
!        is already pointing to the first array summary. In
!        addition, a test made to detect this condition was fixed:
!        the test
!
!           CURR .EQ. 0
!
!        was replaced by
!
!           STCURR(P) .LE. 0
!
!-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
!
!        Literature references added to the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!
!-&


! $ Index_Entries
!
!     find previous DAF array
!
!-&


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFFPA' )
      END IF


!
!     Operate on the last DAF in which a search has been started.
!
      P  =  STHEAD

!
!     FOUND will be false until we make it past the error checks.
!
      FOUND = .FALSE.

!
!     Make sure that a search has been started in this DAF.
!
      IF ( P .EQ. NIL ) THEN

         CALL SETMSG ( 'No DAF is currently being searched.' )
         CALL SIGERR ( 'SPICE(DAFNOSEARCH)'                  )
         CALL CHKOUT ( 'DAFFPA'                              )
         RETURN

!
!     Make sure that the `current' DAF is still open.
!
      ELSE

         CALL DAFSIH ( STFH(P), 'READ' )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'DAFFPA' )
            RETURN
         END IF

      END IF

!
!     Now that we know a search is going on, assume that we will find
!     an array until proven otherwise.
!
      FOUND = .TRUE.

!
!     Either there are more summaries left in this record, or
!     there aren't. If there are, just decrementing the pointer
!     is sufficient. If there aren't, we have to find the previous
!     record and point to the last array there. (If that
!     record is empty, or doesn't exist, then there are simply
!     no more arrays to be found.)
!
      STCURR( P )  =  STCURR( P ) - 1

      IF ( STCURR(P) .LE. 0 ) THEN

         IF ( STPREV(P) .EQ. 0 ) THEN
!
!           There is no predecessor of the current array in the list.
!
            FOUND = .FALSE.
!
!           Make sure that the array pointer stays pointing to
!           the position preceding the front of the list. Otherwise,
!           a call to DAFFNA might fail to find the first array in
!           the list.
!
            STCURR(P) = 0
!
!           The careful reader may note that we're not updating any
!           of the pointers
!
!              STTHIS
!              STNEXT
!              STPREV
!
!           These will not be accessed if there is no current array.
!           If the array pointer is moved forward again by a call to
!           DAFFNA, the values we have right now will be correct.
!
         ELSE

            CALL DAFGSR ( STFH(P), STPREV(P), 1, 128, STSR(1,P), FND )

            IF ( .NOT. FND ) THEN

               CALL DAFHFN ( STFH(P), DAFNAM )

               CALL SETMSG ( 'Attempt to read descriptor record # '   // &
                             'of DAF ''#'' failed; record was '       // &
                             'not found. This condition may indicate '// &
                             'a corrupted DAF.' )
               CALL ERRINT ( '#', STNEXT(P)                            )
               CALL ERRCH  ( '#', DAFNAM                               )
               CALL SIGERR ( 'SPICE(RECORDNOTFOUND)'                   )
               CALL CHKOUT ( 'DAFFPA'                                  )
               RETURN

            END IF

!
!           The name (character) record we've saved no longer applies
!           to the current summary record. However, we've just updated
!           the summary record, so the summary record remains valid.
!
            STHVNR( P )  = .FALSE.

            STTHIS( P )  =  STPREV( P )
            STNEXT( P )  =  INT ( STSR(1,P) )
            STPREV( P )  =  INT ( STSR(2,P) )
            STNSEG( P )  =  INT ( STSR(3,P) )
            STCURR( P )  =  STNSEG( P )

            FOUND        =  ( STNSEG(P) .GT. 0 )

         END IF

      END IF

      CALL CHKOUT ( 'DAFFPA' )
      RETURN




!$Procedure DAFGS ( DAF, get summary )

      ENTRY DAFGS ( SUM )

! $ Abstract
!
!     Return (get) the summary for the current array in the current
!     DAF.
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
!     DAF
!
! $ Keywords
!
!     FILES
!
! $ Declarations
!
!     DOUBLE PRECISION      SUM    ( * )
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     SUM        O   Summary for current array.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     SUM      is the summary for the current array (the array
!              found by the latest call to DAFFNA or DAFFPA).
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If this routine is called when no search is in progress in the
!         the current DAF, the error SPICE(DAFNOSEARCH) is signaled.
!
!     2)  If the DAF for which the "current" array's summary is to be
!         returned has actually been closed, an error is signaled by a
!         routine in the call tree of this routine.
!
!     3)  If no array is current in the current DAF, the error
!         SPICE(NOCURRENTARRAY) is signaled. There is no current array
!         when a search is started by DAFBFS or DAFBBS, but no calls to
!         DAFFNA or DAFFPA have been made yet, or whenever DAFFNA or
!         DAFFPA return the value .FALSE. in the FOUND argument.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     See DAFFA.
!
! $ Examples
!
!     1) See $Examples in DAFFA.
!
!     2) Use a simple routine to output the double precision and
!        integer values stored in an SPK's segment's descriptors. This
!        function opens a DAF for read, performs a forwards search for
!        the DAF arrays, prints the segment descriptor for each array
!        found, then closes the DAF.
!
!        Use the SPK kernel below as input DAF file for the program.
!
!           de421.bsp
!
!
!        Example code begins here.
!
!
!              PROGRAM DAFGS_EX1
!              IMPLICIT NONE
!
!        C
!        C     Define the summary parameters appropriate
!        C     for an SPK file.
!        C
!              INTEGER               MAXSUM
!              PARAMETER           ( MAXSUM = 125 )
!
!              INTEGER               ND
!              PARAMETER           ( ND = 2 )
!
!              INTEGER               NI
!              PARAMETER           ( NI = 6 )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(32)        KERNEL
!
!              DOUBLE PRECISION      DC ( ND     )
!              DOUBLE PRECISION      SUM( MAXSUM )
!
!              INTEGER               HANDLE
!              INTEGER               IC( NI )
!
!              LOGICAL               FOUND
!
!
!        C
!        C     Open a DAF for read. Return a HANDLE referring to the
!        C     file.
!        C
!              KERNEL = 'de421.bsp'
!              CALL DAFOPR ( KERNEL, HANDLE )
!
!        C
!        C     Begin a forward search on the file.
!        C
!              CALL DAFBFS ( HANDLE )
!
!        C
!        C     Search until a DAF array is found.
!        C
!              CALL DAFFNA ( FOUND )
!
!        C
!        C     Loop while the search finds subsequent DAF arrays.
!        C
!              DO WHILE ( FOUND )
!
!                 CALL DAFGS ( SUM )
!                 CALL DAFUS ( SUM, ND, NI, DC, IC )
!
!                 WRITE(*,*)                'Doubles:', DC(1:ND)
!                 WRITE(*, FMT='(A,6I9)' ) 'Integers:', IC(1:NI)
!
!        C
!        C        Check for another segment.
!        C
!                 CALL DAFFNA ( FOUND )
!
!              END DO
!
!        C
!        C     Safely close the DAF.
!        C
!              CALL DAFCLS ( HANDLE )
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:        1        0        1        2      641   310404
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:        2        0        1        2   310405   423048
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:        3        0        1        2   423049   567372
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:        4        0        1        2   567373   628976
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:        5        0        1        2   628977   674740
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:        6        0        1        2   674741   715224
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:        7        0        1        2   715225   750428
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:        8        0        1        2   750429   785632
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:        9        0        1        2   785633   820836
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:       10        0        1        2   820837   944040
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:      301        3        1        2   944041  1521324
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:      399        3        1        2  1521325  2098608
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:      199        1        1        2  2098609  2098620
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:      299        2        1        2  2098621  2098632
!         Doubles:  -3169195200.0000000        1696852800.0000000
!        Integers:      499        4        1        2  2098633  2098644
!
!
!        Note, the final entries in the integer array contains the
!        segment start/end indexes. The output indicates the search
!        proceeded from the start of the file (low value index) towards
!        the end (high value index).
!
! $ Restrictions
!
!     1)  Calls that do or may change DAF addresses of DAF summaries,
!         names, or data of a given DAF file should not be made during
!         a search of that file initiated by either DAFBFS or DAFBBS.
!         No such changes should be made between the start of a search
!         and calls to any entry point that reads or writes to the
!         summary of the "current array" found by that search, or
!         that returns a "found" flag indicating whether the current
!         array exists.
!
!         Changing the size of the comment area while a search is in
!         progress can invalidate record numbers stored in local data
!         structures of this routine. This can cause corrupted array
!         summaries and names to be returned upon read access and file
!         corruption to occur upon write access.
!
!         Adding arrays (aka "segments") while either a forward or
!         backward search is in progress can cause the search to miss
!         the new segments.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.3, 26-OCT-2021 (JDR) (NJB)
!
!        Edited the header to comply with NAIF standard.
!        Added undeclared variables to code example.
!
!        Fixed typo in $Exceptions entry #3: DAFFPA is used to find the
!        previous array, not the non existing API DAFBNA.
!
!        Updated $Restrictions section.
!
!-    SPICELIB Version 2.0.2, 10-OCT-2012 (EDW)
!
!        Added a functional code example to the $Examples section.
!
!        Eliminated unneeded $Revisions section.
!
!        Removed the obsolete Reference citation to "NAIF
!        Document 167.0."
!
!-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT)
!
!        Updated to support simultaneous searches of multiple DAFs.
!        Bug fix made to handle case of having no current array.
!
!        This routine now operates on the current DAF---the one at
!        the head of the active list. All saved state variables
!        used by this routine are now part of the state table, or
!        its associated set of pointers.
!
!        In addition, this routine now checks whether an array
!        is current before trying to read its summary. The routine
!        previously crashed under these conditions.
!
!-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
!
!        Literature references added to the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!
!-&


! $ Index_Entries
!
!     get DAF summary
!
!-&


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFGS' )
      END IF

!
!     Operate on the last DAF in which a search has been started.
!
      P  =  STHEAD

!
!     Make sure that a search has been started in this DAF.
!
      IF ( P .EQ. NIL ) THEN

         CALL SETMSG ( 'No DAF is currently being searched.' )
         CALL SIGERR ( 'SPICE(DAFNOSEARCH)'                  )
         CALL CHKOUT ( 'DAFGS'                               )
         RETURN

!
!     Make sure that the `current' DAF is still open.
!
      ELSE

         CALL DAFSIH ( STFH(P), 'READ' )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'DAFGS' )
            RETURN
         END IF

      END IF

!
!     Check the current pointer position to make sure that it's in
!     bounds. If there is no current array, then we cannot return
!     a summary. This situation occurs if DAFFNA was called when the
!     current array was the last, or if DAFFPA was called when the
!     current array was the first.
!
      IF ( STCURR(P) .EQ. 0 ) THEN

         CALL DAFHFN ( STFH(P), DAFNAM )

         CALL SETMSG ( 'No array is current; the `next'' array is '   // &
                       'the first array of DAF #'                      )
         CALL ERRCH  ( '#',     DAFNAM                                 )
         CALL SIGERR ( 'SPICE(NOCURRENTARRAY)'                         )
         CALL CHKOUT ( 'DAFGS'                                         )
         RETURN

      ELSE IF ( STCURR(P) .GT. STNSEG(P) ) THEN

         CALL DAFHFN ( STFH(P), DAFNAM )

         CALL SETMSG ( 'No array is current; the `previous'' array is'// &
                       ' the last array of DAF #'                      )
         CALL ERRCH  ( '#',     DAFNAM                                 )
         CALL SIGERR ( 'SPICE(NOCURRENTARRAY)'                         )
         CALL CHKOUT ( 'DAFGS'                                         )
         RETURN

      END IF

!
!     The location of the summary depends on the current pointer
!     position.
!
      CALL DAFHSF ( STFH(P), ND, NI )

      SUMSIZ  =  ND   +   ( NI + 1 ) / 2

      OFFSET  =  3    +   ( STCURR(P) - 1 )  *  SUMSIZ

      CALL MOVED (  STSR( OFFSET+1, P ),  SUMSIZ,  SUM )

      CALL CHKOUT ( 'DAFGS' )
      RETURN




!$Procedure DAFGN ( DAF, get array name )

      ENTRY DAFGN ( NAME )

! $ Abstract
!
!     Return (get) the name for the current array in the current DAF.
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
!     DAF
!
! $ Keywords
!
!     FILES
!
! $ Declarations
!
!     CHARACTER*(*)         NAME
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NAME       O   Name of current array.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     NAME     is the name for the current array (the array found by the
!              latest call to DAFFNA or DAFFPA).
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If this routine is called when no search is in progress in the
!         the current DAF, the error SPICE(DAFNOSEARCH) is signaled.
!
!     2)  If the DAF for which the "current" array's name is to be
!         returned has actually been closed, an error is signaled by a
!         routine in the call tree of this routine.
!
!     3)  If no array is current in the current DAF, the error
!         SPICE(NOCURRENTARRAY) is signaled. There is no current array
!         when a search is started by DAFBFS or DAFBBS, but no calls to
!         DAFFNA or DAFFPA have been made yet, or whenever DAFFNA or
!         DAFFPA return the value .FALSE. in the FOUND argument.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     See DAFFA.
!
! $ Examples
!
!     See $Examples in DAFFA.
!
! $ Restrictions
!
!     1)  Calls that do or may change DAF addresses of DAF summaries,
!         names, or data of a given DAF file should not be made during
!         a search of that file initiated by either DAFBFS or DAFBBS.
!         No such changes should be made between the start of a search
!         and calls to any entry point that reads or writes to the
!         summary of the "current array" found by that search, or
!         that returns a "found" flag indicating whether the current
!         array exists.
!
!         Changing the size of the comment area while a search is in
!         progress can invalidate record numbers stored in local data
!         structures of this routine. This can cause corrupted array
!         summaries and names to be returned upon read access and file
!         corruption to occur upon write access.
!
!         Adding arrays (aka "segments") while either a forward or
!         backward search is in progress can cause the search to miss
!         the new segments.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.3, 26-OCT-2021 (JDR) (NJB)
!
!        Edited the header to comply with NAIF standard.
!
!        Fixed typo in $Exceptions entry #3: DAFFPA is used to find the
!        previous array, not the non existing API DAFBNA.
!
!        Updated $Restrictions section.
!
!-    SPICELIB Version 2.0.2, 18-AUG-2011 (EDW)
!
!        Eliminated unneeded $Revisions section.
!
!        Removed the obsolete Reference citation to "NAIF
!        Document 167.0."
!
!-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT)
!
!        Updated to support simultaneous searches of multiple DAFs.
!        Bug fix made to handle case of having no current array.
!
!        This routine now operates on the current DAF---the one at
!        the head of the active list. All saved state variables
!        used by this routine are now part of the state table, or
!        its associated set of pointers.
!
!        In addition, this routine now checks whether an array
!        is current before trying to read its summary. The routine
!        previously crashed under these conditions.
!
!-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
!
!        Literature references added to the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!
!-&


! $ Index_Entries
!
!     get DAF array name
!
!-&


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFGN' )
      END IF


!
!     Operate on the last DAF in which a search has been started.
!
      P  =  STHEAD

!
!     Make sure that a search has been started in this DAF.
!
      IF ( P .EQ. NIL ) THEN

         CALL SETMSG ( 'No DAF is currently being searched.' )
         CALL SIGERR ( 'SPICE(DAFNOSEARCH)'                  )
         CALL CHKOUT ( 'DAFGN'                               )
         RETURN

!
!     Make sure that the `current' DAF is still open.
!
      ELSE

         CALL DAFSIH ( STFH(P), 'READ' )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'DAFGN' )
            RETURN
         END IF

      END IF

!
!     Check the current pointer position to make sure that it's in
!     bounds. If there is no current array, then we cannot get the
!     array's summary's name. This situation occurs if DAFFNA was
!     called when the current array was the last, or if DAFFPA was
!     called when the current array was the first.
!
      IF ( STCURR(P) .EQ. 0 ) THEN

         CALL DAFHFN ( STFH(P), DAFNAM )

         CALL SETMSG ( 'No array is current; the `next'' array is '   // &
                       'the first array of DAF #'                      )
         CALL ERRCH  ( '#',     DAFNAM                                 )
         CALL SIGERR ( 'SPICE(NOCURRENTARRAY)'                         )
         CALL CHKOUT ( 'DAFGN'                                         )
         RETURN

      ELSE IF ( STCURR(P) .GT. STNSEG(P) ) THEN

         CALL DAFHFN ( STFH(P), DAFNAM )

         CALL SETMSG ( 'No array is current; the `previous'' array is'// &
                       ' the last array of DAF #'                      )
         CALL ERRCH  ( '#',     DAFNAM                                 )
         CALL SIGERR ( 'SPICE(NOCURRENTARRAY)'                         )
         CALL CHKOUT ( 'DAFGN'                                         )
         RETURN

      END IF


!
!     Read the name record for this summary record, if we don't have it
!     already.
!
      IF ( .NOT. STHVNR(P) ) THEN

         CALL DAFRCR ( STFH(P), STTHIS(P)+1, STNR(P) )

         STHVNR( P )  =  .TRUE.

      END IF


!
!     The location of the name depends on the current pointer
!     position.
!
      CALL DAFHSF ( STFH(P), ND, NI )

      SUMSIZ  =  ND   +   ( NI + 1 ) / 2

      NAMSIZ  =  SUMSIZ * 8

      OFFSET  =  ( STCURR(P) - 1 )  *  NAMSIZ

      NAME    =  STNR(P) (  OFFSET + 1  :  OFFSET + NAMSIZ  )

      CALL CHKOUT ( 'DAFGN' )
      RETURN





!$Procedure DAFGH ( DAF, get handle )

      ENTRY DAFGH ( HANDLE )

! $ Abstract
!
!     Return (get) the handle of the DAF currently being searched.
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
!     DAF
!
! $ Keywords
!
!     FILES
!
! $ Declarations
!
!     INTEGER               HANDLE
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     O   Handle for current DAF.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     HANDLE   is the handle for the current DAF (the handle connected
!              to the DAF that is currently being actively searched).
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If this routine is called when no search is in progress in the
!         the current DAF, the error SPICE(DAFNOSEARCH) is signaled.
!
!     2)  If the DAF whose handle is to be returned has actually been
!         closed, an error is signaled by a routine in the call tree of
!         this routine.
!
! $ Files
!
!     This routine returns the handle of a DAF that is currently
!     being searched.
!
! $ Particulars
!
!     Under rare circumstances, it may be necessary to identify the
!     particular DAF that is being searched (such as when the search is
!     begun by one module and continued by another).
!
! $ Examples
!
!     Consider a program like the following, which examines the
!     individual arrays in a DAF and examines the contents of those
!     meeting certain criteria.
!
!        CALL DAFOPW ( FNAME, HANDLE )
!        CALL DAFBFS ( HANDLE )
!        CALL DAFFNA ( FOUND  )
!
!        DO WHILE ( FOUND )
!
!           CALL CHECK_DAF ( STATUS )
!
!           IF ( STATUS .EQ. 'EXAMINE' ) THEN
!              CALL EXAMINE_DAF
!           END IF
!
!           CALL DAFFNA ( FOUND )
!
!        END DO
!
!     The subroutine CHECK_DAF, which assumes that a search is in
!     progress, gets the summary and name for the current array, and
!     uses them to decide whether the data in the array merit further
!     consideration.
!
!        SUBROUTINE CHECK_DAF ( STATUS )
!
!        CALL DAFGS ( SUM )
!        CALL DAFGN ( NAME )
!        CALL DAFUS ( SUM, ND, NI, DC, IC )
!         .
!         .
!
!     The subroutine EXAMINE_DAF needs to examine the data in
!     the array itself. In order to do do, it needs to have access
!     not only to the summary, but to the handle of the file
!     containing the array. This is provided by DAFGH.
!
!        SUBROUTINE EXAMINE_DAF
!
!        CALL DAFGS ( SUM  )
!        CALL DAFGH ( HANDLE )
!        CALL DAFUS ( SUM, ND, NI, DC, IC )
!
!        CALL DAFGDA ( HANDLE, BEGIN, END, DATA )
!         .
!         .
!
! $ Restrictions
!
!     1)  Calls that do or may change DAF addresses of DAF summaries,
!         names, or data of a given DAF file should not be made during
!         a search of that file initiated by either DAFBFS or DAFBBS.
!         No such changes should be made between the start of a search
!         and calls to any entry point that reads or writes to the
!         summary of the "current array" found by that search, or
!         that returns a "found" flag indicating whether the current
!         array exists.
!
!         Changing the size of the comment area while a search is in
!         progress can invalidate record numbers stored in local data
!         structures of this routine. This can cause corrupted array
!         summaries and names to be returned upon read access and file
!         corruption to occur upon write access.
!
!         Adding arrays (aka "segments") while either a forward or
!         backward search is in progress can cause the search to miss
!         the new segments.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.4, 26-OCT-2021 (JDR) (NJB)
!
!        Edited the header to comply with NAIF standard. Extended $Files
!        section.
!
!        Updated $Restrictions section.
!
!-    SPICELIB Version 2.0.3, 14-MAR-2017 (NJB)
!
!        Updated header example: changed DAFRDA call to DAFGDA call.
!
!-    SPICELIB Version 2.0.2, 18-AUG-2011 (EDW)
!
!        Eliminated unneeded $Revisions section.
!
!        Removed the obsolete Reference citation to "NAIF
!        Document 167.0."
!
!-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT)
!
!        Updated to support simultaneous searches of multiple DAFs.
!
!        This routine now operates on the current DAF---the one at
!        the head of the active list. All saved state variables
!        used by this routine are now part of the state table, or
!        its associated set of pointers.
!
!-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
!
!        Literature references added to the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!
!-&


! $ Index_Entries
!
!     get DAF handle
!
!-&


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFGH' )
      END IF

!
!     Operate on the last DAF in which a search has been started.
!
      P  =  STHEAD

!
!     Make sure that a search has been started in this DAF.
!
      IF ( P .EQ. NIL ) THEN

         CALL SETMSG ( 'No DAF is currently being searched.' )
         CALL SIGERR ( 'SPICE(DAFNOSEARCH)'                  )
         CALL CHKOUT ( 'DAFGH'                               )
         RETURN

!
!     Make sure that the `current' DAF is still open.
!
      ELSE

         CALL DAFSIH ( STFH(P), 'READ' )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'DAFGH' )
            RETURN
         END IF

      END IF

      HANDLE = STFH( P )

      CALL CHKOUT ( 'DAFGH' )
      RETURN





!$Procedure DAFRS ( DAF, replace summary )

      ENTRY DAFRS ( SUM )

! $ Abstract
!
!     Change the summary for the current array in the current DAF.
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
!     DAF
!
! $ Keywords
!
!     FILES
!
! $ Declarations
!
!     DOUBLE PRECISION      SUM    ( * )
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     SUM        I   New summary for current array.
!
! $ Detailed_Input
!
!     SUM      is the new summary for the current array. This
!              replaces the existing summary. However, the addresses
!              (the final two integer components) of the original
!              summary are not changed.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If this routine is called when no search is in progress in the
!         the current DAF, the error SPICE(DAFNOSEARCH) is signaled.
!
!     2)  If the DAF containing the "current" array has actually been
!         closed, an error is signaled by a routine in the call tree of
!         this routine.
!
!     3)  If the DAF containing the "current" array is not open for
!         writing, an error is signaled by a routine in the call tree of
!         this routine.
!
!     4)  If no array is current in the current DAF, the error
!         SPICE(NOCURRENTARRAY) is signaled. There is no current array
!         when a search is started by DAFBFS or DAFBBS, but no calls to
!         DAFFNA or DAFFPA have been made yet, or whenever DAFFNA or
!         DAFFPA return the value .FALSE. in the FOUND argument.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     See DAFFA.
!
! $ Examples
!
!     See $Examples in DAFFA.
!
! $ Restrictions
!
!     1)  Calls that do or may change DAF addresses of DAF summaries,
!         names, or data of a given DAF file should not be made during
!         a search of that file initiated by either DAFBFS or DAFBBS.
!         No such changes should be made between the start of a search
!         and calls to any entry point that reads or writes to the
!         summary of the "current array" found by that search, or
!         that returns a "found" flag indicating whether the current
!         array exists.
!
!         Changing the size of the comment area while a search is in
!         progress can invalidate record numbers stored in local data
!         structures of this routine. This can cause corrupted array
!         summaries and names to be returned upon read access and file
!         corruption to occur upon write access.
!
!         Adding arrays (aka "segments") while either a forward or
!         backward search is in progress can cause the search to miss
!         the new segments.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.3, 27-OCT-2021 (JDR) (NJB)
!
!        Fixed typo in the $Declarations section.
!
!        Edited the header to comply with NAIF standard.
!
!        Fixed typo in $Exceptions entry #4: DAFFPA is used to find the
!        previous array, not the non existing API DAFBNA.
!
!        Updated $Restrictions section.
!
!-    SPICELIB Version 2.0.2, 18-AUG-2011 (EDW)
!
!        Eliminated unneeded $Revisions section.
!
!        Removed the obsolete Reference citation to "NAIF
!        Document 167.0."
!
!-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT)
!
!        Updated to support simultaneous searches of multiple DAFs.
!        Bug fix made to handle case of having no current array.
!
!        This routine now operates on the current DAF---the one at
!        the head of the active list. All saved state variables
!        used by this routine are now part of the state table, or
!        its associated set of pointers.
!
!        In addition, this routine now checks whether an array
!        is current before trying to read its summary. The routine
!        previously crashed under these conditions.
!
!-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
!
!        Literature references added to the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!
!-&


! $ Index_Entries
!
!     replace DAF summary
!
!-&


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFRS' )
      END IF

!
!     Operate on the last DAF in which a search has been started.
!
      P  =  STHEAD

!
!     Make sure that a search has been started in this DAF.
!
      IF ( P .EQ. NIL ) THEN

         CALL SETMSG ( 'No DAF is currently being searched.' )
         CALL SIGERR ( 'SPICE(DAFNOSEARCH)'                  )
         CALL CHKOUT ( 'DAFRS'                               )
         RETURN

!
!     Make sure that the `current' DAF is still open, and that it
!     is open for writing.
!
      ELSE

         CALL DAFSIH ( STFH(P), 'WRITE' )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'DAFRS' )
            RETURN
         END IF

      END IF

!
!     Check the current pointer position to make sure that it's in
!     bounds. If there is no current array, then we cannot replace the
!     array's  summary. This situation occurs if DAFFNA was called
!     when the current array was the last, or if DAFFPA was called when
!     the current array was the first.
!
      IF ( STCURR(P) .EQ. 0 ) THEN

         CALL DAFHFN ( STFH(P), DAFNAM )

         CALL SETMSG ( 'No array is current; the `next'' array is '   // &
                       'the first array of DAF #'                      )
         CALL ERRCH  ( '#',     DAFNAM                                 )
         CALL SIGERR ( 'SPICE(NOCURRENTARRAY)'                         )
         CALL CHKOUT ( 'DAFRS'                                         )
         RETURN

      ELSE IF ( STCURR(P) .GT. STNSEG(P) ) THEN

         CALL DAFHFN ( STFH(P), DAFNAM )

         CALL SETMSG ( 'No array is current; the `previous'' array is'// &
                       ' the last array of DAF #'                      )
         CALL ERRCH  ( '#',     DAFNAM                                 )
         CALL SIGERR ( 'SPICE(NOCURRENTARRAY)'                         )
         CALL CHKOUT ( 'DAFRS'                                         )
         RETURN

      END IF

!
!     The location of the summary depends on the current pointer
!     position.
!
      CALL DAFHSF ( STFH(P), ND, NI )

      SUMSIZ  =  ND   +   ( NI + 1 ) / 2

      OFFSET  =  3    +   ( STCURR(P) - 1 ) * SUMSIZ

!
!     Get the existing summary, and unpack it. Replace everything
!     but the addresses (the final two integer components), and
!     repack. Then replace the existing summary within the record.
!
      CALL MOVED (  STSR( OFFSET+1, P ),  SUMSIZ,  EXSUM )

      CALL DAFUS ( EXSUM, ND, NI, EXDC,  EXIC  )
      CALL DAFUS ( SUM,   ND, NI, NEWDC, NEWIC )

      CALL MOVED ( NEWDC, ND,   EXDC )
      CALL MOVEI ( NEWIC, NI-2, EXIC )
      CALL DAFPS ( ND, NI, EXDC, EXIC, EXSUM )

      CALL MOVED (  EXSUM,  SUMSIZ,  STSR( OFFSET+1, P )  )

!
!     Rewrite the modified summary record.
!
      CALL DAFWDR ( STFH(P), STTHIS(P), STSR(1,P) )

      CALL CHKOUT ( 'DAFRS' )
      RETURN




!$Procedure DAFRN ( DAF, change array name )

      ENTRY DAFRN ( NAME )

! $ Abstract
!
!     Replace the name for the current array in the current DAF.
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
!     DAF
!
! $ Keywords
!
!     FILES
!
! $ Declarations
!
!     CHARACTER*(*)         NAME
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NAME       I   New name for current array.
!
! $ Detailed_Input
!
!     NAME     is the new name for the current array.
!              This replaces the existing name.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If this routine is called when no search is in progress in the
!         the current DAF, the error SPICE(DAFNOSEARCH) is signaled.
!
!     2)  If the DAF containing the "current" array has actually been
!         closed, an error is signaled by a routine in the call tree of
!         this routine.
!
!     3)  If the DAF containing the "current" array is not open for
!         writing, an error is signaled by a routine in the call tree of
!         this routine.
!
!     4)  If no array is current in the current DAF, the error
!         SPICE(NOCURRENTARRAY) is signaled. There is no current
!         array when a search is started by DAFBFS or DAFBBS, but no
!         calls to DAFFNA or DAFBNA have been made yet, or whenever
!         DAFFNA or DAFFPA return the value .FALSE. in the FOUND
!         argument.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     See DAFFA.
!
! $ Examples
!
!     See $Examples in DAFFA.
!
! $ Restrictions
!
!     1)  Calls that do or may change DAF addresses of DAF summaries,
!         names, or data of a given DAF file should not be made during
!         a search of that file initiated by either DAFBFS or DAFBBS.
!         No such changes should be made between the start of a search
!         and calls to any entry point that reads or writes to the
!         summary of the "current array" found by that search, or
!         that returns a "found" flag indicating whether the current
!         array exists.
!
!         Changing the size of the comment area while a search is in
!         progress can invalidate record numbers stored in local data
!         structures of this routine. This can cause corrupted array
!         summaries and names to be returned upon read access and file
!         corruption to occur upon write access.
!
!         Adding arrays (aka "segments") while either a forward or
!         backward search is in progress can cause the search to miss
!         the new segments.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.3, 26-OCT-2021 (JDR) (NJB)
!
!        Edited the header to comply with NAIF standard.
!
!        Updated $Restrictions section.
!
!-    SPICELIB Version 2.0.2, 18-AUG-2011 (EDW)
!
!        Eliminated unneeded $Revisions section.
!
!        Removed the obsolete Reference citation to "NAIF
!        Document 167.0."
!
!-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT)
!
!        Updated to support simultaneous searches of multiple DAFs.
!
!        This routine now operates on the current DAF---the one at
!        the head of the active list. All saved state variables
!        used by this routine are now part of the state table, or
!        its associated set of pointers.
!
!        In addition, this routine now checks whether an array
!        is current before trying to read its summary. The routine
!        previously crashed under these conditions.
!
!-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
!
!        Literature references added to the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!
!-&


! $ Index_Entries
!
!     change DAF array name
!
!-&


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFRN' )
      END IF

!
!     Operate on the last DAF in which a search has been started.
!
      P  =  STHEAD

!
!     Make sure that a search has been started in this DAF.
!
      IF ( P .EQ. NIL ) THEN

         CALL SETMSG ( 'No DAF is currently being searched.' )
         CALL SIGERR ( 'SPICE(DAFNOSEARCH)'                  )
         CALL CHKOUT ( 'DAFRN'                               )
         RETURN

!
!     Make sure that the `current' DAF is still open, and that it
!     is open for writing.
!
      ELSE

         CALL DAFSIH ( STFH(P), 'WRITE' )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'DAFRN' )
            RETURN
         END IF

      END IF

!
!     Check the current pointer position to make sure that it's in
!     bounds. If there is no current array, then we cannot replace
!     the array's summary's name. This situation occurs if DAFFNA was
!     called when the current array was the last, or if DAFFPA was
!     called when the current array was the first.
!
      IF ( STCURR(P) .EQ. 0 ) THEN

         CALL DAFHFN ( STFH(P), DAFNAM )

         CALL SETMSG ( 'No array is current; the `next'' array is '   // &
                       'the first array of DAF #'                      )
         CALL ERRCH  ( '#',     DAFNAM                                 )
         CALL SIGERR ( 'SPICE(NOCURRENTARRAY)'                         )
         CALL CHKOUT ( 'DAFRN'                                         )
         RETURN

      ELSE IF ( STCURR(P) .GT. STNSEG(P) ) THEN

         CALL DAFHFN ( STFH(P), DAFNAM )

         CALL SETMSG ( 'No array is current; the `previous'' array is'// &
                       ' the last array of DAF #'                      )
         CALL ERRCH  ( '#',     DAFNAM                                 )
         CALL SIGERR ( 'SPICE(NOCURRENTARRAY)'                         )
         CALL CHKOUT ( 'DAFRN'                                         )
         RETURN

      END IF



!
!     Read the name record for this summary record, if we don't have it
!     already.
!
      IF ( .NOT. STHVNR(P) ) THEN

         CALL DAFRCR ( STFH(P), STTHIS(P)+1, STNR(P) )

         STHVNR( P )  =  .TRUE.

      END IF


!
!     The location of the name depends on the current pointer
!     position.
!
      CALL DAFHSF ( STFH(P), ND, NI )

      SUMSIZ  =  ND   +   ( NI + 1 ) / 2

      NAMSIZ  =  SUMSIZ * 8

      OFFSET  =  ( STCURR(P) - 1 )  *  NAMSIZ

      STNR(P) (  OFFSET + 1  :  OFFSET + NAMSIZ  )   =   NAME

!
!     Rewrite the character record.
!
      CALL DAFWCR ( STFH(P), STTHIS(P)+1, STNR(P) )

      CALL CHKOUT ( 'DAFRN' )
      RETURN




!$Procedure DAFWS ( DAF, write summary )

      ENTRY DAFWS ( SUM )

! $ Abstract
!
!     Write a new summary for the current array in the current DAF.
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
!     DAF
!
! $ Keywords
!
!     FILES
!
! $ Declarations
!
!     DOUBLE PRECISION      SUM ( * )
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     SUM        I   New summary for current array in the current DAF.
!
! $ Detailed_Input
!
!     SUM      is the new summary for the current array. This
!              replaces the existing summary, including the
!              addresses (the final two integer components) of
!              the original summary.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If this routine is called when no search is in progress in the
!         the current DAF, the error SPICE(DAFNOSEARCH) is signaled.
!
!     2)  If the DAF containing the "current" array has actually been
!         closed, an error is signaled by a routine in the call tree of
!         this routine.
!
!     3)  If the DAF containing the "current" array is not open for
!         writing, an error is signaled by a routine in the call tree of
!         this routine.
!
!     4)  If no array is current in the current DAF, the error
!         SPICE(NOCURRENTARRAY) is signaled. There is no current
!         array when a search is started by DAFBFS or DAFBBS, but no
!         calls to DAFFNA or DAFBNA have been made yet, or whenever
!         DAFFNA or DAFFPA return the value .FALSE. in the FOUND
!         argument.
!
! $ Files
!
!     DAFWS updates the DAF currently being searched. The handle
!     of this DAF can be retrieved using the routine DAFGH.
!
! $ Particulars
!
!     Unless you are reordering the arrays in the file being searched,
!     you should be using DAFRS instead of this routine.
!
!     See also DAFFA, DAFRS.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     1)  Calls that do or may change DAF addresses of DAF summaries,
!         names, or data of a given DAF file should not be made during
!         a search of that file initiated by either DAFBFS or DAFBBS.
!         No such changes should be made between the start of a search
!         and calls to any entry point that reads or writes to the
!         summary of the "current array" found by that search, or
!         that returns a "found" flag indicating whether the current
!         array exists.
!
!         Changing the size of the comment area while a search is in
!         progress can invalidate record numbers stored in local data
!         structures of this routine. This can cause corrupted array
!         summaries and names to be returned upon read access and file
!         corruption to occur upon write access.
!
!         Adding arrays (aka "segments") while either a forward or
!         backward search is in progress can cause the search to miss
!         the new segments.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.3, 27-OCT-2021 (JDR) (NJB)
!
!        Edited the header to comply with NAIF standard.
!
!        Updated $Restrictions section.
!
!-    SPICELIB Version 2.0.2, 18-AUG-2011 (EDW)
!
!        Eliminated unneeded $Revisions section.
!
!        Removed the obsolete Reference citation to "NAIF
!        Document 167.0."
!
!-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT)
!
!        Updated to support simultaneous searches of multiple DAFs.
!        Bug fix made to handle case of having no current array.
!
!        This routine now operates on the current DAF---the one at
!        the head of the active list. All saved state variables
!        used by this routine are now part of the state table, or
!        its associated set of pointers.
!
!        In addition, this routine now checks whether an array
!        is current before trying to read its summary. The routine
!        previously crashed under these conditions.
!
!-    SPICELIB Version 1.0.0, 28-MAR-1991 (IMU)
!
!-&


! $ Index_Entries
!
!     write DAF summary
!
!-&


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFWS' )
      END IF

!
!     Operate on the last DAF in which a search has been started.
!
      P  =  STHEAD

!
!     Make sure that a search has been started in this DAF.
!
      IF ( P .EQ. NIL ) THEN

         CALL SETMSG ( 'No DAF is currently being searched.' )
         CALL SIGERR ( 'SPICE(DAFNOSEARCH)'                  )
         CALL CHKOUT ( 'DAFWS'                               )
         RETURN

!
!     Make sure that the `current' DAF is still open, and that it is
!     open for writing.
!
      ELSE

         CALL DAFSIH ( STFH(P), 'READ' )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'DAFWS' )
            RETURN
         END IF

      END IF

!
!     Check the current pointer position to make sure that it's in
!     bounds. If there is no current array, then we cannot write a
!     new array summary. This situation occurs if DAFFNA was called
!     when the current array was the last, or if DAFFPA was called
!     when the current array was the first.
!
      IF ( STCURR(P) .EQ. 0 ) THEN

         CALL DAFHFN ( STFH(P), DAFNAM )

         CALL SETMSG ( 'No array is current; the `next'' array is '   // &
                       'the first array of DAF #'                      )
         CALL ERRCH  ( '#',     DAFNAM                                 )
         CALL SIGERR ( 'SPICE(NOCURRENTARRAY)'                         )
         CALL CHKOUT ( 'DAFWS'                                         )
         RETURN

      ELSE IF ( STCURR(P) .GT. STNSEG(P) ) THEN

         CALL DAFHFN ( STFH(P), DAFNAM )

         CALL SETMSG ( 'No array is current; the `previous'' array is'// &
                       ' the last array of DAF #'                      )
         CALL ERRCH  ( '#',     DAFNAM                                 )
         CALL SIGERR ( 'SPICE(NOCURRENTARRAY)'                         )
         CALL CHKOUT ( 'DAFWS'                                         )
         RETURN

      END IF



!
!     The location of the summary depends on the current pointer
!     position.
!
      CALL DAFHSF ( STFH(P), ND, NI )

      SUMSIZ  =  ND   +   ( NI + 1 ) / 2

      OFFSET  =  3    +   ( STCURR(P) - 1 ) * SUMSIZ

      CALL MOVED (  SUM,  SUMSIZ,  STSR( OFFSET+1, P )  )

!
!     Rewrite the modified summary record.
!
      CALL DAFWDR ( STFH(P), STTHIS(P), STSR(1,P) )

      CALL CHKOUT ( 'DAFWS' )
      RETURN




!$Procedure DAFCS ( DAF, continue search )

      ENTRY DAFCS ( HANDLE )

! $ Abstract
!
!     Select a DAF that already has a search in progress as the
!     one to continue searching.
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
!     DAF
!
! $ Keywords
!
!     FILES
!
! $ Declarations
!
!     INTEGER               HANDLE
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of DAF to continue searching.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of a DAF in which either a forward
!              or backward search has already been started by
!              DAFBFS or DAFBBS. The DAF may be open for read
!              or write access.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input handle is invalid, an error is signaled by a
!         routine in the call tree of this routine.
!
!     2)  If this routine is called when no search is in progress in the
!         the current DAF, the error SPICE(DAFNOSEARCH) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     DAFCS supports simultaneous searching of multiple DAFs. In
!     applications that use this capability, DAFCS should be called
!     prior to each call to DAFFNA, DAFFPA, DAFGN, DAFGS, DAFRS, or
!     DAFWS, to specify which DAF is to be acted upon.
!
! $ Examples
!
!     See $Examples in DAFFA.
!
! $ Restrictions
!
!     1)  Calls that do or may change DAF addresses of DAF summaries,
!         names, or data of a given DAF file should not be made during
!         a search of that file initiated by either DAFBFS or DAFBBS.
!         No such changes should be made between the start of a search
!         and calls to any entry point that reads or writes to the
!         summary of the "current array" found by that search, or
!         that returns a "found" flag indicating whether the current
!         array exists.
!
!         Changing the size of the comment area while a search is in
!         progress can invalidate record numbers stored in local data
!         structures of this routine. This can cause corrupted array
!         summaries and names to be returned upon read access and file
!         corruption to occur upon write access.
!
!         Adding arrays (aka "segments") while either a forward or
!         backward search is in progress can cause the search to miss
!         the new segments.
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
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.3, 26-OCT-2021 (JDR) (NJB)
!
!        Edited the header to comply with NAIF standard.
!
!        Updated $Restrictions section.
!
!-    SPICELIB Version 1.0.2, 10-FEB-2014 (BVS)
!
!        Added full declaration of HANDLE to the $Declarations section
!        of the header.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 04-SEP-1991 (NJB) (WLT)
!
!-&


! $ Index_Entries
!
!     select a DAF to continue searching
!
!-&


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFCS' )
      END IF

!
!     Validate the DAF's handle before going any further. DAFSIH will
!     signal an error if HANDLE doesn't designate an open DAF.
!
      CALL DAFSIH ( HANDLE, 'READ' )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DAFCS' )
         RETURN
      END IF

!
!     See whether we already have an entry for this DAF in the
!     state table. Find the previous node if possible.
!
      P     =   STHEAD
      PREV  =   NIL
      FND   =  .FALSE.

      DO WHILE (  ( P .NE. NIL )  .AND.  ( .NOT. FND )  )

         IF ( STFH(P) .EQ. HANDLE ) THEN
            FND   =  .TRUE.
         ELSE
            PREV  =  P
            P     =  STPOOL( P )
         END IF

      END DO

!
!     Either FND is false, or P is the index in the state table of
!     the DAF specified by HANDLE, and PREV is the predecessor of P.
!

!
!     You can't continue searching a DAF that you're not already
!     searching.
!
      IF ( .NOT. FND ) THEN

         CALL SETMSG ( 'No DAF is currently being searched.' )
         CALL SIGERR ( 'SPICE(DAFNOSEARCH)'                  )
         CALL CHKOUT ( 'DAFCS'                               )
         RETURN

      END IF


!
!     Move the node for this DAF to the head of the active list,
!     if it is not already there:
!
!        - Make the predecessor of P point to the successor of P.
!
!        - Make P point to the head of the active list.
!
!        - Make P the active list head node.
!

      IF ( P .NE. STHEAD ) THEN
!
!        P is in the active list, but is not at the head. So,
!        the predecessor of P is not NIL.
!
         STPOOL(PREV)  =  STPOOL(P)
         STPOOL(P)     =  STHEAD
         STHEAD        =  P

      END IF

      CALL CHKOUT ( 'DAFCS' )
      RETURN
      END
