!$Procedure DASRDC ( DAS, read data, character )

      SUBROUTINE DASRDC ( HANDLE, FIRST, LAST, BPOS, EPOS, DATA )

! $ Abstract
!
!     Read character data from a range of DAS logical addresses.
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
!     DAS
!
! $ Keywords
!
!     ARRAY
!     ASSIGNMENT
!     DAS
!     FILES
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               HANDLE
      INTEGER               FIRST
      INTEGER               LAST
      INTEGER               BPOS
      INTEGER               EPOS
      CHARACTER*(*)         DATA   ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   DAS file handle.
!     FIRST,
!     LAST       I   Range of DAS character logical addresses.
!     BPOS,
!     EPOS       I   Begin and end positions of substrings.
!     DATA       O   Data having addresses FIRST through LAST.
!
! $ Detailed_Input
!
!     HANDLE   is a file handle for an open DAS file.
!
!     FIRST,
!     LAST     are a range of DAS character logical addresses.
!              FIRST and LAST must be greater than or equal to
!              1 and less than or equal to the highest character
!              logical address in the DAS file designated by
!              HANDLE.
!
!     BPOS,
!     EPOS     are the begin and end character positions that define the
!              substrings in each of the elements of the output array
!              DATA into which character data is to be read.
!
! $ Detailed_Output
!
!     DATA     is an array of strings. On output, the character words in
!              the logical address range FIRST through LAST are copied
!              into the characters
!
!                 DATA(1)(BPOS:BPOS),
!                 DATA(1)(BPOS+1:BPOS+1),
!                             .
!                             .
!                             .
!                 DATA(1)(EPOS:EPOS),
!                 DATA(2)(BPOS:BPOS),
!                 DATA(2)(BPOS+1:BPOS+1),
!                             .
!                             .
!                             .
!                 DATA(R)(BPOS:BPOS)
!                 DATA(R)(BPOS+1:BPOS+1)
!                             .
!                             .
!                             .
!
!              in that order. Note that the character positions of DATA
!              **other** than the ones shown in the diagram remain
!              unmodified.
!
!              DATA must be declared at least as
!
!                 CHARACTER*(EPOS)        DATA   ( R )
!
!              with the dimension R being at least
!
!                 R = INT( ( LAST - FIRST + SUBLEN ) / SUBLEN )
!
!              and SUBLEN, the length of each of the substrings read
!              into the array elements from the DAS file, being
!
!                 SUBLEN  =  EPOS - BPOS + 1
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input file handle is invalid, an error is signaled
!         by a routine in the call tree of this routine. DATA will
!         not be modified.
!
!     2)  If EPOS or BPOS are outside of the range
!
!            [  1,  LEN( DATA(1) )  ]
!
!         or if EPOS < BPOS, the error SPICE(BADSUBSTRINGBOUNDS) is
!         signaled.
!
!     3)  If FIRST or LAST are out of range, an error is signaled by a
!         routine in the call tree of this routine. DATA will not be
!         modified.
!
!     4)  If FIRST is greater than LAST, DATA is left unchanged.
!
!     5)  If DATA is declared with length less than
!
!            ( LAST - FIRST + ( EPOS-BPOS+1 )  ) / ( EPOS-BPOS+1 )
!
!         the error cannot be diagnosed by this routine.
!
! $ Files
!
!     See the description of the argument HANDLE in $Detailed_Input.
!
! $ Particulars
!
!     DAS is a low-level format meant to store and transmit data. As
!     such, character data in DAS files are not interpreted by SPICELIB
!     DAS input or output routines. There are no limits on which
!     character values may be placed in the virtual character array of a
!     DAS file.
!
!     This routine provides random read access to the character data in
!     a DAS file. These data are logically structured as a
!     one-dimensional array of characters.
!
!     However, since Fortran programs usually use strings rather than
!     arrays of individual characters, the interface of this routine
!     provides for extraction of data from a DAS file into an array of
!     strings.
!
!     DASRDC allows the caller to control the amount of character data
!     read into each array element. This feature allows a program to
!     read character data into an array that has a different string
!     length from the one used to write the character data, without
!     losing the correspondence between input and output array elements.
!     For example, an array of strings of 32 characters can be written
!     to a DAS file and read back by DASRDC into a buffer of strings
!     having length 80 characters, mapping each 32-character string to
!     characters 1--32 of the output buffer.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) The following example demonstrates the capabilities of the
!        DAS character data routines. The reader should notice that
!        in these interfaces, the character data are treated not as
!        strings (or arrays of strings) but as a stream of single
!        characters: DAS character data are not limited to
!        human-readable text. For example, one can store images or
!        DEM data as DAS character data.
!
!        The example shows how to add a variable amount of character
!        data to a new DAS file, how to update some of the character
!        logical addresses within that file, and how to read that
!        data out to a different array.
!
!
!        Example code begins here.
!
!
!              PROGRAM DASRDC_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local parameters.
!        C
!              CHARACTER*(*)         FNAME
!              PARAMETER           ( FNAME = 'dasrdc_ex1.das' )
!
!              CHARACTER*(*)         TYPE
!              PARAMETER           ( TYPE  = 'TEST'           )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(22)        CDATIN ( 3  )
!              CHARACTER*(30)        CDATOU ( 10 )
!
!              INTEGER               HANDLE
!              INTEGER               I
!
!              DATA CDATOU  / '..............................',
!             .               '..............................',
!             .               '..............................',
!             .               '..............................',
!             .               '..............................',
!             .               '..............................',
!             .               '..............................',
!             .               '..............................',
!             .               '         1         2         3',
!             .               '123456789012345678901234567890' /
!
!        C
!        C     Open a new DAS file. Use the file name as the internal
!        C     file name, and reserve no records for comments.
!        C
!              CALL DASONW ( FNAME, TYPE, FNAME, 0, HANDLE )
!
!        C
!        C     Set the input data. Note that these data will be
!        C     considered as a binary data stream: DAS character data
!        C     are not limited to human-readable text. For example,
!        C     one can store images or DEM data as DAS character data.
!        C
!              CDATIN ( 1 ) = '--F-345678901234567890'
!              CDATIN ( 2 ) = '--S-345678901234567890'
!              CDATIN ( 3 ) = '--T-IRDxxxxxxxxxxxxxxx'
!
!        C
!        C     Add the last 20 characters of the first two elements
!        C     of CDATIN, and the 3rd character from the third one.
!        C
!              CALL DASADC ( HANDLE, 41, 3, 22, CDATIN )
!
!        C
!        C     Update the 10th, 20th and 30th character in the DAS
!        C     file with a vertical bar.
!        C
!              DO I = 1, 3
!
!                 CALL DASUDC ( HANDLE, I*10, I*10, 1, 1, '|' )
!
!              END DO
!
!        C
!        C     Close the file.
!        C
!              CALL DASCLS ( HANDLE )
!
!        C
!        C     Now verify the addition of data by opening the
!        C     file for read access and retrieving the data.
!        C
!              CALL DASOPR ( FNAME, HANDLE )
!
!        C
!        C     Read the 41 characters that we stored on the DAS
!        C     file. Update the data on the CDATOU array, placing
!        C     6 characters on each element, starting from the
!        C     10th position.
!        C
!              CALL DASRDC ( HANDLE, 1, 41, 10, 15, CDATOU )
!
!        C
!        C     Dump the data to the screen. Note that the last
!        C     three lines should remain unmodified, and that
!        C     only 5 characters will be written on the 7th line.
!        C
!              WRITE (*,*)
!              WRITE (*,*) 'Data from "', FNAME, '":'
!              WRITE (*,*)
!
!              DO I = 1, 10
!                 WRITE (*,*) CDATOU(I)
!              END DO
!
!        C
!        C     Close the file.
!        C
!              CALL DASCLS ( HANDLE )
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!         Data from "dasrdc_ex1.das":
!
!         .........F-3456...............
!         .........789|12...............
!         .........345678...............
!         .........9|S-34...............
!         .........56789|...............
!         .........123456...............
!         .........7890T................
!         ..............................
!                  1         2         3
!         123456789012345678901234567890
!
!
!        Note that after run completion, a new DAS file exists in the
!        output directory.
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
!     K.R. Gehringer     (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.3.0, 09-OCT-2021 (JDR) (NJB)
!
!        Added IMPLICIT NONE statement.
!
!        Added FAILED call following DASA2L call.
!
!        Updated entries in $Revisions section.
!
!        Edited the header to comply with NAIF standard.
!
!        Replaced example code with one that demonstrates the usage and
!        effect of all DAS character data routines.
!
!-    SPICELIB Version 1.2.2, 03-JUL-1996 (NJB)
!
!        Various errors in the header comments were fixed.
!
!-    SPICELIB Version 1.2.1, 19-DEC-1995 (NJB)
!
!        Corrected title of permuted index entry section.
!
!-    SPICELIB Version 1.2.0, 03-NOV-1995 (NJB)
!
!        Routine now uses discovery check-in. FAILED test moved inside
!        loops.
!
!-    SPICELIB Version 1.2.0, 14-SEP-1995 (NJB)
!
!        Bug fix: reference to DASADS in CHKOUT calls corrected.
!
!-    SPICELIB Version 1.1.0, 12-MAY-1994 (KRG) (NJB)
!
!        Test of FAILED() added to loop termination conditions.
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input section of the header. This was done in order
!        to minimize documentation changes if the DAS open routines ever
!        change.
!
!        Modified the $Examples section to demonstrate the new ID word
!        format which includes a file type and to include a call to the
!        new routine DASONW, open new for write, which makes use of the
!        file type. Also,  a variable for the type of the file to be
!        created was added.
!
!-    SPICELIB Version 1.0.0, 12-NOV-1992 (NJB) (WLT)
!
!-&


! $ Index_Entries
!
!     read character data from a DAS file
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.2.0, 03-NOV-1995 (NJB)
!
!        Routine now uses discovery check-in. FAILED test moved inside
!        loops.
!
!-    SPICELIB Version 1.2.0, 03-NOV-1995 (NJB)
!
!        Bug fix: reference to DASADS in CHKOUT calls corrected.
!        These references have been changed to 'DASRDC'.
!
!
!-    SPICELIB Version 1.1.0, 12-MAY-1994 (KRG) (NJB)
!
!        Test of FAILED() added to loop termination conditions. Without
!        this test, an infinite loop could result if DASA2L or DASRRC
!        signaled an error inside the loops.
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               FAILED

!
!     Local parameters
!
      INTEGER               NWC
      PARAMETER           ( NWC =  1024 )

      INTEGER               CHAR
      PARAMETER           ( CHAR   =  1  )



!
!     Local variables
!
      INTEGER               CHR
      INTEGER               CLBASE
      INTEGER               CLSIZE
      INTEGER               ELT
      INTEGER               L
      INTEGER               N
      INTEGER               NMOVE
      INTEGER               NMOVED
      INTEGER               NREAD
      INTEGER               NUMCHR
      INTEGER               RECNO
      INTEGER               RCPOS
      INTEGER               WORDNO



!
!     Make sure BPOS and EPOS are ok; stop here if not.
!
      IF (      ( BPOS .LT. 1              ) &
           .OR. ( EPOS .LT. 1              ) &
           .OR. ( BPOS .GT. LEN( DATA(1) ) ) &
           .OR. ( EPOS .GT. LEN( DATA(1) ) )  )  THEN

         CALL CHKIN  ( 'DASRDC'                                        )
         CALL SETMSG ( 'Substring bounds must be in range [1,#]. '    // &
                       'Actual range [BPOS,EPOS] was [#,#].'           )
         CALL ERRINT ( '#',  LEN(DATA(1))                              )
         CALL ERRINT ( '#',  BPOS                                      )
         CALL ERRINT ( '#',  EPOS                                      )
         CALL SIGERR ( 'SPICE(BADSUBSTRINGBOUNDS)'                     )
         CALL CHKOUT ( 'DASRDC'                                        )
         RETURN


      ELSE IF ( EPOS .LT. BPOS ) THEN

         CALL CHKIN  ( 'DASRDC'                                        )
         CALL SETMSG ( 'Substring upper bound must not be less than ' // &
                       'lower bound.  Actual range [BPOS,EPOS] was '  // &
                       '[#,#].'                                        )
         CALL ERRINT ( '#',  BPOS                                      )
         CALL ERRINT ( '#',  EPOS                                      )
         CALL SIGERR ( 'SPICE(BADSUBSTRINGBOUNDS)'                     )
         CALL CHKOUT ( 'DASRDC'                                        )
         RETURN

      END IF

!
!     Find out the physical location of the first character to read.  If
!     FIRST is out of range, DASA2L will cause an error to be signaled.
!
      CALL DASA2L ( HANDLE, CHAR, FIRST, CLBASE, CLSIZE, RECNO, WORDNO )

      IF ( FAILED() ) THEN
         RETURN
      END IF

!
!     Get the length of the elements of DATA.  Count the total number
!     of characters to read.
!
      L       =  EPOS - BPOS  + 1
      N       =  LAST - FIRST + 1
      NREAD   =  0

!
!     Read as much data from record RECNO as is necessary and possible.
!
      NUMCHR  =  MIN ( N,  NWC - WORDNO + 1 )

      ELT     =  1
      CHR     =  BPOS
      NMOVED  =  0
      RCPOS   =  WORDNO


      DO WHILE ( NMOVED .LT. NUMCHR )

         IF ( FAILED() ) THEN
            RETURN
         END IF

         IF ( CHR .GT. EPOS ) THEN
            ELT = ELT + 1
            CHR = BPOS
         END IF
!
!        Find out how many characters to move from the current record
!        to the current array element.
!
         NMOVE   =  MIN (  NUMCHR - NMOVED,   EPOS - CHR + 1  )

         CALL DASRRC (  HANDLE, &
                        RECNO, &
                        RCPOS, &
                        RCPOS + NMOVE - 1, &
                        DATA(ELT) ( CHR  :  CHR + NMOVE - 1 )   )

         NMOVED  =  NMOVED + NMOVE
         RCPOS   =  RCPOS  + NMOVE
         CHR     =  CHR    + NMOVE

      END DO

      NREAD  =  NUMCHR
      RECNO  =  RECNO + 1

!
!     Read from as many additional records as necessary.
!

      DO WHILE ( NREAD .LT. N )

         IF ( FAILED() ) THEN
            RETURN
         END IF

!
!        At this point, RECNO is the correct number of the
!        record to read from next.  CLBASE is the number
!        of the first record of the cluster we're about
!        to read from.
!
!
         IF (  RECNO  .LT.  ( CLBASE + CLSIZE )  ) THEN
!
!           We can continue reading from the current cluster.  Find
!           out how many elements to read from the current record,
!           and read them.
!
            NUMCHR  =  MIN ( N - NREAD,  NWC )
            NMOVED  =  0
            RCPOS   =  1

            DO WHILE ( ( NMOVED .LT. NUMCHR ) .AND. ( .NOT. FAILED() ) )

               IF ( CHR .GT. EPOS ) THEN
                  ELT = ELT + 1
                  CHR = BPOS
               END IF
!
!              Find out how many characters to move from the current
!              record to the current array element.
!
               NMOVE   =  MIN (  NUMCHR - NMOVED,   EPOS - CHR + 1  )

               CALL DASRRC (  HANDLE, &
                              RECNO, &
                              RCPOS, &
                              RCPOS + NMOVE - 1, &
                              DATA(ELT) ( CHR  :  CHR + NMOVE - 1 )   )

               NMOVED  =  NMOVED + NMOVE
               RCPOS   =  RCPOS  + NMOVE
               CHR     =  CHR    + NMOVE

            END DO

            NREAD   =   NREAD + NUMCHR
            RECNO   =   RECNO + 1


         ELSE
!
!           We must find the next character cluster to
!           read from.  The first character in this
!           cluster has address FIRST + NREAD.
!
            CALL DASA2L ( HANDLE, &
                          CHAR, &
                          FIRST + NREAD, &
                          CLBASE, &
                          CLSIZE, &
                          RECNO, &
                          WORDNO  )

         END IF

      END DO

      RETURN
      END
