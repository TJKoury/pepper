!$Procedure     ZZEKFRX ( EK, find record in index )

      SUBROUTINE ZZEKFRX ( HANDLE, SEGDSC, COLDSC, RECPTR, POS )

! $ Abstract
!
!     Find the ordinal position of a specified record in a specified,
!     indexed EK column.
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
!     PRIVATE
!
! $ Declarations


      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekqlimit.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'

      INTEGER               HANDLE
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               COLDSC ( CDSCSZ )
      INTEGER               RECPTR
      INTEGER               POS

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     SEGDSC     I   Segment descriptor.
!     COLDSC     I   Column descriptor.
!     RECPTR     I   Pointer to record to locate.
!     POS        O   Ordinal position of record.
!
! $ Detailed_Input
!
!     HANDLE         is an EK file handle.  The file may be open for
!                    reading or writing.
!
!     SEGDSC         is the segment descriptor of the segment
!                    containing the column to be searched.
!
!     COLDSC         is the column descriptor of the column to be
!                    searched.
!
!     RECPTR         is a pointer to the record whose ordinal position
!                    is to be found.
!
! $ Detailed_Output
!
!     POS            is the ordinal position in the specified column
!                    of the input record, where the order relation is
!                    specified by the column's index.
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
!     2)  If an I/O error occurs while reading the indicated
!         file, the error will be diagnosed by routines called by this
!         routine.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     Various EK write operations require the capability of locating
!     the index key that maps to a given record number.  An example is
!     updating a column's index to reflect deletion of a specified
!     record:  the key that maps to the record must be deleted.
!     Locating this key is the inverse of the problem that the index
!     is meant to solve.
!
! $ Examples
!
!     See ZZEKIXDL.
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
!-    SPICELIB Version 2.1.0, 07-FEB-2015 (NJB)
!
!        Now uses ERRHAN to insert DAS file name into
!        long error message.
!
!-    SPICELIB Version 2.0.0, 31-MAY-2010 (NJB)
!
!        Bug fix: substring bound out-of-range violation
!        in reference to local variable CVAL has been
!        corrected. This error could occur if the a
!        class 3 column entry had length exceeding MAXSTR.
!
!-    Beta Version 1.0.0, 10-OCT-1995 (NJB)
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Non-SPICELIB functions
!
      INTEGER               ZZEKRP2N

!
!     Local variables
!
      CHARACTER*(MAXSTR)    CVAL

      DOUBLE PRECISION      DVAL

      INTEGER               CMPLEN
      INTEGER               CVLEN
      INTEGER               DTYPE
      INTEGER               IVAL
      INTEGER               PRVPTR
      INTEGER               RECNO

      LOGICAL               FOUND
      LOGICAL               ISNULL

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZEKFRX' )

!
!     Determine the data type of the column, and look up the value
!     associated with RECPTR.
!
      DTYPE  =  COLDSC ( TYPIDX )

      IF ( DTYPE .EQ. CHR ) THEN

         CALL ZZEKRSC (  HANDLE,  SEGDSC,  COLDSC,  RECPTR, &
                         1,       CVLEN,   CVAL,    ISNULL,  FOUND  )


         IF (  FOUND  .AND. ( .NOT. ISNULL )  ) THEN

            CMPLEN = MIN ( CVLEN, MAXSTR )
         ELSE
            CMPLEN = 0
         END IF


      ELSE IF (  ( DTYPE .EQ. DP ) .OR. ( DTYPE .EQ. TIME )  ) THEN

         CALL ZZEKRSD (  HANDLE,  SEGDSC,  COLDSC,  RECPTR, &
                         1,       DVAL,    ISNULL,  FOUND  )


      ELSE IF ( DTYPE .EQ. INT ) THEN

         CALL ZZEKRSI (  HANDLE,  SEGDSC,  COLDSC,  RECPTR, &
                         1,       IVAL,    ISNULL,  FOUND  )

      ELSE

         CALL SETMSG ( 'File = #; COLIDX = #. Unrecognized data ' // &
                       'type code # found in descriptor.'           )
         CALL ERRHAN ( '#',     HANDLE                              )
         CALL ERRINT ( '#',     COLDSC(ORDIDX)                      )
         CALL ERRINT ( '#',     DTYPE                               )
         CALL SIGERR ( 'SPICE(ITEMNOTFOUND)'                        )
         CALL CHKOUT ( 'ZZEKFRX'                                    )
         RETURN

      END IF



      IF ( .NOT. FOUND ) THEN
!
!        We have a most heinous situation.  We should always be able
!        to find the value associated with a record.
!
         RECNO  =  ZZEKRP2N ( HANDLE, SEGDSC(SNOIDX), RECPTR )

         CALL SETMSG ( 'File = #; RECNO = #; COLIDX = #. Column '     // &
                       'entry was not found.  This probably indicates'// &
                       ' a corrupted file or a bug in the EK code.'    )
         CALL ERRHAN ( '#',     HANDLE                                 )
         CALL ERRINT ( '#',     RECNO                                  )
         CALL ERRINT ( '#',     COLDSC(ORDIDX)                         )
         CALL SIGERR ( 'SPICE(ITEMNOTFOUND)'                           )
         CALL CHKOUT ( 'ZZEKFRX'                                       )
         RETURN

      END IF

!
!     Find the last column entry less than or equal to the one
!     associated with the input record, where the order relation is
!     dictionary ordering on (<column value>, <record number>) pairs.
!     These ordered pairs are distinct, even if the column entries
!     are not.  Therefore, the ordinal position POS will actually be
!     the ordinal position of our record.
!
      IF ( DTYPE .EQ. CHR ) THEN

         CALL ZZEKLERC ( HANDLE,  SEGDSC,  COLDSC,  CVAL(:CMPLEN), &
                         RECPTR,  ISNULL,  POS,     PRVPTR         )


      ELSE IF (  ( DTYPE .EQ. DP ) .OR. ( DTYPE .EQ. TIME )  ) THEN

         CALL ZZEKLERD ( HANDLE,  SEGDSC,  COLDSC,  DVAL, &
                         RECPTR,  ISNULL,  POS,     PRVPTR  )


      ELSE
!
!        The data type is INT.  (We've already checked for invalid
!        types.)
!

         CALL ZZEKLERI ( HANDLE,  SEGDSC,  COLDSC,  IVAL, &
                         RECPTR,  ISNULL,  POS,     PRVPTR  )

      END IF


      IF ( PRVPTR .NE. RECPTR ) THEN
!
!        Big problem.  This should never happen.
!
         RECNO  =  ZZEKRP2N ( HANDLE, SEGDSC(SNOIDX), RECPTR )

         CALL SETMSG ( 'File = #; RECNO = #; COLIDX = #.  Record ' // &
                       'that was last less than or equal to RECNO '// &
                       'was not equal to RECNO.  This probably '   // &
                       'indicates  a corrupted file or a bug in '  // &
                       'the EK code.'                               )
         CALL ERRHAN ( '#',     HANDLE                              )
         CALL ERRINT ( '#',     RECNO                               )
         CALL ERRINT ( '#',     COLDSC(ORDIDX)                      )
         CALL SIGERR ( 'SPICE(ITEMNOTFOUND)'                        )
         CALL CHKOUT ( 'ZZEKFRX'                                    )
         RETURN

      END IF


      CALL CHKOUT ( 'ZZEKFRX' )
      RETURN
      END
