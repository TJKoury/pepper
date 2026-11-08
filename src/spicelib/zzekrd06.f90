!$Procedure   ZZEKRD06 ( EK, read class 6 column entry elements )

      SUBROUTINE ZZEKRD06 ( HANDLE,  SEGDSC,  COLDSC,  RECPTR, &
                            BEG,     END,     CVALS,   ISNULL,  FOUND )

      IMPLICIT NONE

! $ Abstract
!
!     Read a specified element range from a column entry in a specified
!     record in a class 6 column.  Class 6 columns have character arrays
!     as column entries.
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
!     EK
!
! $ Keywords
!
!     EK
!     PRIVATE
!
! $ Declarations

      INCLUDE 'ekbool.inc'
      INCLUDE 'ekcnamsz.inc'
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekdatpag.inc'
      INCLUDE 'ekrecptr.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'

      INTEGER               HANDLE
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               COLDSC ( CDSCSZ )
      INTEGER               RECPTR
      INTEGER               BEG
      INTEGER               END
      CHARACTER*(*)         CVALS  ( * )
      LOGICAL               ISNULL
      LOGICAL               FOUND

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle attached to EK file.
!     SEGDSC     I   Segment descriptor.
!     COLDSC     I   Column descriptor.
!     RECPTR     I   Record pointer.
!     BEG        I   Start element index.
!     END        I   End element index.
!     CVALS      O   Character values in column entry.
!     ISNULL     O   Flag indicating whether column entry is null.
!     FOUND      O   Flag indicating whether elements were found.
!
! $ Detailed_Input
!
!     HANDLE         is an EK file handle.
!
!     SEGDSC         is the descriptor of the segment from which data is
!                    to be read.
!
!     COLDSC         is the descriptor of the column from which data is
!                    to be read.
!
!     RECPTR         is a pointer to the record containing the column
!                    entry to be written.
!
!     BEG,
!     END            are, respectively, the start and end indices of
!                    the contiguous range of elements to be read from
!                    the specified column entry.
!
! $ Detailed_Output
!
!     CVALS          are the values read from the specified column
!                    entry.  The mapping of elements of the column entry
!                    to elements of CVALS is as shown below:
!
!                       Column entry element       CVALS element
!                       --------------------       -------------
!                       BEG                        1
!                       BEG+1                      2
!                       .                          .
!                       .                          .
!                       .                          .
!                       END                        END-BEG+1
!
!                    CVALS must have sufficient string length to hold
!                    the longest returned string value.  Entries that
!                    are shorter than the string length of CVALS are
!                    padded with trailing blanks.
!
!                    CVALS is valid only if the output argument
!                    FOUND is returned .TRUE.
!
!     ISNULL         is a logical flag indicating whether the entry is
!                    null.  ISNULL is set on output whether or not
!                    the range of elements designated by BEG and END
!                    exists.
!
!     FOUND          is a logical flag indicating whether the range
!                    of elements designated by BEG and END exists.
!                    If the number of elements in the specified column
!                    entry is not at least END, FOUND will be returned
!                    .FALSE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If HANDLE is invalid, the error will be diagnosed by routines
!         called by this routine.
!
!     2)  If the specified column entry has not been initialized, the
!         error SPICE(UNINITIALIZED) is signaled.
!
!     3)  If the ordinal position of the column specified by COLDSC
!         is out of range, the error SPICE(INVALIDINDEX) is signaled.
!
!     4)  If the string length of CVALS is shorter than the declared
!         string length of the specified column, the error
!         SPICE(STRINGTRUNCATED) is signaled.
!
!     5)  If an I/O error occurs while reading the indicated file,
!         the error will be diagnosed by routines called by this
!         routine.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine is a utility for reading data from class 6 columns.
!
! $ Examples
!
!     See EKRCEC.
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 07-FEB-2015 (NJB)
!
!        Now uses ERRHAN to insert DAS file name into
!        long error messages.
!
!        Bug fix: changed max column index in long error
!        message from NREC to NCOLS.
!
!-    SPICELIB Version 1.1.0, 28-JUL-1997 (NJB)
!
!        Error check for string truncation on output was added.
!        SHORT error message SPICE(UNINITIALIZEDVALUE) was shortened
!        to SPICE(UNINITIALIZED).  Error messages were enhanced so
!        as to use column names rather than indices.
!
!-    SPICELIB Version 1.0.0, 18-OCT-1995 (NJB)
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.1.0, 28-JUL-1997 (NJB)
!
!        Error check for string truncation on output was added.
!        SHORT error message SPICE(UNINITIALIZEDVALUE) was shortened
!        to SPICE(UNINITIALIZED).  Error messages were enhanced so
!        as to use column names rather than indices.
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               FAILED

!
!     Non-SPICELIB functions
!
      INTEGER               ZZEKRP2N

!
!     Local variables
!
      CHARACTER*(CNAMSZ)    COLUMN

      INTEGER               AVAIL
      INTEGER               BASE
      INTEGER               COLIDX
      INTEGER               CVLEN
      INTEGER               D
      INTEGER               DATPTR
      INTEGER               DELTA
      INTEGER               ELTIDX
      INTEGER               ELTOFF
      INTEGER               MAXELT
      INTEGER               NCOLS
      INTEGER               NELT
      INTEGER               NREAD
      INTEGER               NSKIP
      INTEGER               OFFSET
      INTEGER               P
      INTEGER               PAGNUM
      INTEGER               PG
      INTEGER               PTRLOC
      INTEGER               PTROFF
      INTEGER               RECNO
      INTEGER               REMAIN
      INTEGER               START
      INTEGER               STRLEN


!
!     Use discovery check-in.
!
!     Make sure the column exists.
!
      NCOLS  =  SEGDSC ( NCIDX  )
      COLIDX =  COLDSC ( ORDIDX )

      IF (  ( COLIDX .LT. 1 ) .OR. ( COLIDX .GT. NCOLS )  ) THEN

         CALL CHKIN  ( 'ZZEKRD06'                              )
         CALL SETMSG ( 'Column index = #; valid range is 1:#.' )
         CALL ERRINT ( '#',  COLIDX                            )
         CALL ERRINT ( '#',  NCOLS                             )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                   )
         CALL CHKOUT ( 'ZZEKRD06'                              )
         RETURN

      END IF

!
!     Make sure the output buffer is wide enough to hold the returned
!     strings.
!
      CVLEN   =  LEN    ( CVALS(1) )
      STRLEN  =  COLDSC ( LENIDX   )

      IF ( STRLEN .GT. CVLEN ) THEN
!
!        We have a string truncation error.  Look up the column
!        name, record number, and file name before signaling an
!        error.
!
         CALL ZZEKCNAM ( HANDLE, COLDSC, COLUMN )

         RECNO  =  ZZEKRP2N ( HANDLE, SEGDSC(SNOIDX), RECPTR )

         CALL CHKIN  ( 'ZZEKRD06'                                  )
         CALL SETMSG ( 'String value has length #; output string '// &
                       'can hold only # characters.  COLUMN = #; '// &
                       'SEGNO = #; RECNO = #; EK = #'              )
         CALL ERRINT ( '#',  STRLEN                                )
         CALL ERRINT ( '#',  CVLEN                                 )
         CALL ERRCH  ( '#',  COLUMN                                )
         CALL ERRINT ( '#',  SEGDSC(SNOIDX)                        )
         CALL ERRINT ( '#',  RECNO                                 )
         CALL ERRHAN ( '#',  HANDLE                                )
         CALL SIGERR ( 'SPICE(STRINGTRUNCATED)'                    )
         CALL CHKOUT ( 'ZZEKRD06'                                  )
         RETURN

      END IF


!
!     Compute the data pointer location, and read the pointer.
!
      PTRLOC  =  RECPTR + DPTBAS + COLIDX

      CALL DASRDI ( HANDLE, PTRLOC, PTRLOC, DATPTR )


      IF ( DATPTR .GT. 0 ) THEN
!
!        The entry is non-null.
!
         ISNULL  =  .FALSE.

!
!        Get the element count.  Check for range specifications that
!        can't be met.
!
         CALL ZZEKGEI ( HANDLE, DATPTR, NELT )

         IF (  ( BEG .LT. 1 ) .OR. ( BEG .GT. NELT )  )  THEN

            FOUND  =  .FALSE.
            RETURN

         ELSE IF (  ( END .LT. 1 ) .OR. ( END .GT. NELT )  )  THEN

            FOUND  =  .FALSE.
            RETURN

         ELSE IF ( END .LT. BEG ) THEN

            FOUND  =  .FALSE.
            RETURN

         END IF

!
!        The request is valid, so read the data.  The first step is to
!        locate the element at index BEG.  We'll first decide on which
!        page the desired element starts.  The first page holds up to
!        CPSIZE - ENCSIZ characters; the rest hold CPSIZE characters.
!        While we're at it, we'll compute the offset ELTOFF of the
!        element from the base of the page on which the element starts.
!        We'll use the name OFFSET to represent the character offset
!        of the element from the base of the page on which the column
!        entry starts.
!
         CALL ZZEKPGPG ( CHR, DATPTR, P, BASE )

         PTROFF  =  DATPTR  -  BASE
         OFFSET  =  PTROFF  +  ENCSIZ   +   STRLEN * ( BEG - 1 )

         IF ( OFFSET .LE. CPSIZE ) THEN

            PAGNUM   =    1
            ELTOFF   =    OFFSET

         ELSE

            PAGNUM   =  ( OFFSET  +  CPSIZE   -   1 ) / CPSIZE
            ELTOFF   =    OFFSET  -    ( PAGNUM - 1 ) * CPSIZE

         END IF

!
!        Get the absolute page number and base address of the page
!        on which the element starts.  If this is not the page on
!        which the column entry starts, we'll chain along using
!        the page's forward links until we arrive at the correct page.
!
         PG  =  1

         DO WHILE ( PG .LT. PAGNUM )
!
!           Get the link to the next page, then look up the base
!           address of that page.
!
            CALL ZZEKGEI  ( HANDLE, BASE+CFPIDX, P    )
            CALL ZZEKPGBS ( CHR,    P,           BASE )

            PG  =  PG + 1

         END DO

!
!        The desired element starts at address BASE + ELTOFF.
!
         DATPTR  =  BASE + ELTOFF

!
!        At this point, P is the page on which the element having index
!        BEG is located.  BASE is the base address of this page.
!
!        Read the strings one at a time.
!
         ELTIDX   =  1
         MAXELT   =  END - BEG + 1

         DO WHILE (  ( ELTIDX .LE. MAXELT ) .AND. ( .NOT. FAILED() )  )
!
!           Read the current string.  The string may be continued over
!           multiple pages.  Read only as many characters as will fit
!           in the output buffer element CVALS(ELTIDX).
!
            REMAIN  =  MIN ( CVLEN, STRLEN )
            START   =  1

            DO WHILE (  ( REMAIN .GT. 0 ) .AND. ( .NOT. FAILED() )  )

               AVAIL  =  BASE + CPSIZE - DATPTR + 1
               NREAD  =  MIN  ( REMAIN,  AVAIL )

               IF ( NREAD .GT. 0 ) THEN

                  CALL DASRDC ( HANDLE, &
                                DATPTR, &
                                DATPTR + NREAD - 1, &
                                START, &
                                START + NREAD - 1, &
                                CVALS ( ELTIDX )    )

                  START  =  START  + NREAD
                  REMAIN =  REMAIN - NREAD
                  DATPTR =  DATPTR + NREAD

               ELSE
!
!                 Go to the next page for the continuation of the
!                 current string.
!
                  CALL ZZEKGEI  ( HANDLE, BASE+CFPIDX, P    )
                  CALL ZZEKPGBS ( CHR,    P,           BASE )

                  DATPTR  =  BASE   +  1

               END IF

            END DO

!
!           If we did not read all of the current array element,
!           we'll need to advance DATPTR past the end of the element.
!           If this advance moved DATPTR beyond the last character
!           of the current page, the logic above will set DATPTR to
!           indicate the first character of the next continuation page.
!
            DELTA  =  STRLEN - CVLEN

            IF ( DELTA .GT. 0 ) THEN

               D  =  DELTA

               DO WHILE ( D .GT. 0 )

                  AVAIL  =  BASE + CPSIZE - DATPTR + 1
                  NSKIP  =  MIN  ( D,  AVAIL )

                  IF ( NSKIP .GT. 0 ) THEN

                     D      =  D      - NSKIP
                     DATPTR =  DATPTR + NSKIP

                  ELSE
!
!                    Go to the next page for the continuation of the
!                    current string.
!
                     CALL ZZEKGEI  ( HANDLE, BASE+CFPIDX, P    )
                     CALL ZZEKPGBS ( CHR,    P,           BASE )

                     DATPTR  =  BASE   +  1

                  END IF

               END DO

            END IF

!
!           Blank-pad the output string if necessary.
!
            IF ( CVLEN .GT. STRLEN ) THEN
               CVALS ( ELTIDX ) ( STRLEN+1: ) = ' '
            END IF


            ELTIDX  =  ELTIDX + 1

         END DO


         FOUND =  .NOT. FAILED()


      ELSE IF ( DATPTR .EQ. NULL ) THEN
!
!        The value is null.
!
         ISNULL =  .TRUE.
         FOUND  =  .TRUE.


      ELSE IF ( DATPTR .EQ. UNINIT ) THEN
!
!        The data value is absent.  This is an error.
!
         RECNO  =  ZZEKRP2N ( HANDLE, SEGDSC(SNOIDX), RECPTR )

         CALL ZZEKCNAM ( HANDLE,   COLDSC, COLUMN )

         CALL CHKIN  ( 'ZZEKRD06'                                    )
         CALL SETMSG ( 'Attempted to read uninitialized column '    // &
                       'entry.  SEGNO = #; COLUMN = #; RECNO = #; ' // &
                       'EK = #'                                      )
         CALL ERRINT ( '#',  SEGDSC(SNOIDX)                          )
         CALL ERRCH  ( '#',  COLUMN                                  )
         CALL ERRINT ( '#',  RECNO                                   )
         CALL ERRHAN ( '#',  HANDLE                                  )
         CALL SIGERR ( 'SPICE(UNINITIALIZED)'                        )
         CALL CHKOUT ( 'ZZEKRD06'                                    )
         RETURN


      ELSE
!
!        The data pointer is corrupted.
!
         RECNO  =  ZZEKRP2N ( HANDLE, SEGDSC(SNOIDX), RECPTR )

         CALL ZZEKCNAM ( HANDLE,   COLDSC, COLUMN )

         CALL CHKIN  ( 'ZZEKRD06'                                )
         CALL SETMSG ( 'Data pointer is corrupted. SEGNO = #; '  // &
                       'COLUMN =  #; RECNO = #; EK = #'          )
         CALL ERRINT ( '#',  SEGDSC(SNOIDX)                      )
         CALL ERRCH  ( '#',  COLUMN                              )
         CALL ERRINT ( '#',  RECNO                               )
         CALL ERRHAN ( '#',  HANDLE                              )
         CALL SIGERR ( 'SPICE(BUG)'                              )
         CALL CHKOUT ( 'ZZEKRD06'                                )
         RETURN

      END IF


      RETURN
      END
