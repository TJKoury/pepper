!$Procedure   ZZEKRD04 ( EK, read class 4 column entry elements )

      SUBROUTINE ZZEKRD04 ( HANDLE,  SEGDSC,  COLDSC,  RECPTR, &
                            BEG,     END,     IVALS,   ISNULL,  FOUND )

! $ Abstract
!
!     Read a specified element range from a column entry in a specified
!     record in a class 4 column.  Class 4 columns have integer arrays
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
!     FILES
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'ekbool.inc'
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
      INTEGER               IVALS  ( * )
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
!     IVALS      O   Integer values in column entry.
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
!     IVALS          are the values read from the specified column
!                    entry.  The mapping of elements of the column entry
!                    to elements of IVALS is as shown below:
!
!                       Column entry element       IVALS element
!                       --------------------       -------------
!                       BEG                        1
!                       BEG+1                      2
!                       .                          .
!                       .                          .
!                       .                          .
!                       END                        END-BEG+1
!
!                    IVALS is valid only if the output argument
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
!         error SPICE(UNINITIALIZEDVALUE) is signaled.
!
!     3)  If the ordinal position of the column specified by COLDSC
!         is out of range, the error SPICE(INVALIDINDEX) is signaled.
!
!     4)  If an I/O error occurs while reading the indicated file,
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
!     This routine is a utility for reading data from class 4 columns.
!
! $ Examples
!
!     See EKRCEI.
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
!-    SPICELIB Version 1.1.0, 08-SEP-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in ZZEKGFWD call.
!
!-    SPICELIB Version 1.0.0, 18-OCT-1995 (NJB)
!
!-&

! $ Revisions
!
!-    SPICELIB Version 1.1.0, 08-SEP-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in ZZEKGFWD call.
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
      INTEGER               BASE
      INTEGER               COLIDX
      INTEGER               DATPTR
      INTEGER               MAXIDX
      INTEGER               MINIDX
      INTEGER               NCOLS
      INTEGER               NELT
      INTEGER               NREAD
      INTEGER               P
      INTEGER               PTEMP
      INTEGER               PTRLOC
      INTEGER               RECNO
      INTEGER               REMAIN
      INTEGER               START

!
!     Use discovery check-in.
!

!
!     Make sure the column exists.
!
      NCOLS  =  SEGDSC ( NCIDX  )
      COLIDX =  COLDSC ( ORDIDX )

      IF (  ( COLIDX .LT. 1 ) .OR. ( COLIDX .GT. NCOLS )  ) THEN

         CALL CHKIN  ( 'ZZEKRD04'                              )
         CALL SETMSG ( 'Column index = #; valid range is 1:#.' )
         CALL ERRINT ( '#',  COLIDX                            )
         CALL ERRINT ( '#',  NCOLS                             )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                   )
         CALL CHKOUT ( 'ZZEKRD04'                              )
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
         CALL DASRDI ( HANDLE, DATPTR, DATPTR, NELT )

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
!        locate the element at index BEG.
!
         CALL ZZEKPGPG ( INT, DATPTR, P, BASE )

         MINIDX  =  1
         MAXIDX  =  BASE   + IPSIZE - DATPTR
         DATPTR  =  DATPTR + BEG


         DO WHILE ( MAXIDX .LT. BEG )
!
!           Locate the page on which the element is continued.
!
            CALL DASRDI ( HANDLE, BASE+IFPIDX, BASE+IFPIDX, P )

!
!           Determine the highest-indexed element of the column entry
!           located on the current page.
!
            CALL ZZEKPGBS ( INT, P, BASE )

            MINIDX  =  MAXIDX  +   1
            MAXIDX  =  MIN ( MAXIDX + IPSIZE, NELT )

!
!           The following assignment will set DATPTR to the correct
!           value on the last pass through this loop.
!
            DATPTR  =  BASE    +   1   +  ( BEG - MINIDX )

         END DO

!
!        At this point, P is the page on which the element having index
!        BEG is located.  BASE is the base address of this page.
!        MAXIDX is the highest index of any element on the current page.
!
         REMAIN  =  END    -   BEG   +   1
         START   =  1

!
!        Decide how many elements to read from the current page, and
!        read them.
!
         NREAD   =  MIN ( REMAIN,  ( BASE + IPSIZE - DATPTR + 1 )  )

         CALL DASRDI ( HANDLE, DATPTR, DATPTR+NREAD-1, IVALS(START) )

         REMAIN  =  REMAIN  -  NREAD


         DO WHILE (  ( REMAIN .GT. 0 ) .AND.  ( .NOT. FAILED() )  )
!
!           Locate the page on which the element is continued.
!
            CALL ZZEKGFWD ( HANDLE, INT, P, PTEMP )
            P = PTEMP

            CALL ZZEKPGBS ( INT,         P, BASE  )

            DATPTR  =  BASE   +  1
            START   =  START  +  NREAD
            NREAD   =  MIN ( REMAIN,  IPSIZE )

            CALL DASRDI ( HANDLE, DATPTR, DATPTR+NREAD-1, IVALS(START) )

            REMAIN  =  REMAIN  -  NREAD

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

         CALL CHKIN  ( 'ZZEKRD04'                                    )
         CALL SETMSG ( 'Attempted to read uninitialized column '    // &
                       'entry.  SEGNO = #; COLIDX = #; RECNO = #; ' // &
                       'EK = #'                                      )
         CALL ERRINT ( '#',  SEGDSC(SNOIDX)                          )
         CALL ERRINT ( '#',  COLIDX                                  )
         CALL ERRINT ( '#',  RECNO                                   )
         CALL ERRHAN ( '#',  HANDLE                                  )
         CALL SIGERR ( 'SPICE(UNINITIALIZEDVALUE)'                   )
         CALL CHKOUT ( 'ZZEKRD04'                                    )
         RETURN


      ELSE
!
!        The data pointer is corrupted.
!
         CALL CHKIN  ( 'ZZEKRD04'                                )
         CALL SETMSG ( 'Data pointer is corrupted. SEGNO = #; '  // &
                       'COLIDX =  #; RECNO = #; EK = #'          )
         CALL ERRINT ( '#',  SEGDSC(SNOIDX)                      )
         CALL ERRINT ( '#',  COLIDX                              )
         CALL ERRINT ( '#',  RECNO                               )
         CALL ERRHAN ( '#',  HANDLE                              )
         CALL SIGERR ( 'SPICE(BUG)'                              )
         CALL CHKOUT ( 'ZZEKRD04'                                )
         RETURN

      END IF


      RETURN
      END
