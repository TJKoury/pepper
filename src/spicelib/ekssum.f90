!$Procedure EKSSUM ( EK, return segment summary )

      SUBROUTINE EKSSUM (  HANDLE,  SEGNO,   TABNAM,  NROWS, &
                           NCOLS,   CNAMES,  DTYPES,  SIZES, &
                           STRLNS,  INDEXD,  NULLOK          )

! $ Abstract
!
!     Return summary information for a specified segment in a
!     specified EK.
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
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'ekbool.inc'
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekglimit.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'

      INTEGER               HANDLE
      INTEGER               SEGNO
      CHARACTER*(*)         TABNAM
      INTEGER               NROWS
      INTEGER               NCOLS
      CHARACTER*(*)         CNAMES ( * )
      CHARACTER*(*)         DTYPES ( * )
      INTEGER               SIZES  ( * )
      INTEGER               STRLNS ( * )
      LOGICAL               INDEXD ( * )
      LOGICAL               NULLOK ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of EK.
!     SEGNO      I   Number of segment to be summarized.
!     TABNAM     O   Name of table containing segment.
!     NROWS      O   Number of rows in segment.
!     NCOLS      O   Number of columns in segment.
!     CNAMES     O   Names of columns in segment.
!     DTYPES     O   Data types of columns in segment.
!     SIZES      O   Entry sizes of columns in segment.
!     STRLNS     O   String lengths of columns in segment.
!     INDEXD     O   Flags indicating whether columns are indexed.
!     NULLOK     O   Flags indicating whether columns allow nulls.
!
! $ Detailed_Input
!
!     HANDLE   is an EK file handle specifying the EK containing
!              the segment to be summarized.
!
!     SEGNO    is the number of the segment whose summary is
!              desired. Segments are numbered from 1 to NSEG,
!              where NSEG is the count of segments in the file.
!
! $ Detailed_Output
!
!     TABNAM   is the name of the table to which the segment
!              belongs.
!
!     NROWS    is the number of rows in the segment.
!
!     NCOLS    is the number of columns in the segment. The
!              maximum number of columns in a segment is given
!              by the parameter MXCLSG, which is defined in the
!              include file
!
!                 ekglimit.inc.
!
!              Currently, this limit is set at 100 columns.
!
!     CNAMES   is an array of names of columns in the segment.
!
!     DTYPES   is an array of data types of columns in the
!              segment. Each data type is indicated by a short
!              character string. The strings and their meanings
!              are:
!
!                 'CHR'       Character type.
!                 'DP'        Double precision type.
!                 'INT'       Integer type.
!                 'TIME'      Time type.
!
!              The Ith element of DTYPES corresponds to the
!              column whose name is the Ith element of CNAMES.
!
!     SIZES    is an array of declared sizes of column entries.
!              The Ith element of SIZES is the declared size of
!              the column whose name is the Ith element of CNAMES.
!              Scalar-valued columns have size 1; fixed-size,
!              array-valued columns have size greater than 1.
!              Array valued columns of variable size have a size
!              value of -1.
!
!     STRLNS   is an array of declared string lengths of
!              character column entries. These lengths are
!              defined only for columns of character type.
!              The Ith element of SIZES is the declared size of
!              the column whose name is the Ith element of CNAMES,
!              if that column has character type; otherwise, the
!              Ith element of STRLNS is undefined. For
!              character columns having variable string length,
!              the returned value of STRLNS is -1.
!
!     INDEXD   is an array of logical flags indicating whether the
!              corresponding columns are indexed. The Ith element
!              of INDEXD applies to the column whose name is the
!              Ith element of CNAMES.
!
!     NULLOK   is an array of logical flags indicating whether the
!              corresponding columns allow null values. The Ith
!              element of NULLOK applies to the column whose name
!              is the Ith element of CNAMES.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If HANDLE is invalid, an error is signaled by a routine in the
!         call tree of this routine. The output arguments will not be
!         modified.
!
!     2)  If SEGNO is not the index of an existing segment in the
!         specified file, the error SPICE(INDEXOUTOFRANGE) is signaled.
!         The output arguments will not be modified.
!
!     3)  If an I/O error occurs while attempting to obtain summary
!         information for the specified segment, the error is signaled
!         by a routine in the call tree of this routine. The output
!         arguments may be modified in this case.
!
! $ Files
!
!     See the description of HANDLE in $Detailed_Input.
!
! $ Particulars
!
!     This routine supports the function of summarizing a binary
!     EK file, allowing NAIF Toolkit users to determine whether it
!     contains data of interest. The routine also also provides
!     address information necessary to retrieve information from the
!     segment.
!
! $ Examples
!
!     1)  Dump the table and column names of the segments in an EK.
!
!            C
!            C     Open the EK for read access and get the number of
!            C     segments it contains.
!            C
!                  CALL EKOPR ( EKNAME, HANDLE )
!
!                  NSEG = EKNSEG ( HANDLE )
!
!            C
!            C     Loop through the segments, dumping the desired
!            C     summary information for each one.
!            C
!                  WRITE (*,*) ' '
!                  WRITE (*,*) ' '
!                  WRITE (*,*) 'Segment summary for file ', EKNAME
!                  WRITE (*,*) ' '
!                  WRITE (*,*) ' '
!
!                  DO I = 1, NSEG
!
!                     CALL EKSSUM (  HANDLE,  SEGNO,   TABNAM,  NROWS,
!                 .                  NCOLS,   CNAMES,  DTYPES,  SIZES,
!                 .                  STRLNS,  INDEXD,  NULLOK         )
!
!                     WRITE (*,*)
!                 .   '========================================'      //
!                 .   '========================================'
!
!
!                     WRITE (*,*) 'Table containing segment: ', TABNAM
!
!                     WRITE (*,*) ' '
!                     WRITE (*,*) 'Number of rows:     ', NROWS
!                     WRITE (*,*) 'Number of columns:  ', NCOLS
!                     WRITE (*,*) ' '
!                     WRITE (*,*) 'Column names and attributes: '
!                     WRITE (*,*) ' '
!
!                     DO J = 1, NCOLS
!
!                        WRITE (*,*) 'Column:   '//CNAMES(J)
!                        WRITE (*,*) ' '
!                        WRITE (*,*) 'Data type: ', DTYPES(J)
!                        WRITE (*,*) 'Dimension: ', SIZES(J)
!
!                        IF ( DTYPES(J) .EQ. 'CHR' ) THEN
!                           WRITE (*,*) 'String length: ', STRLNS(J)
!                        END IF
!
!                        IF ( INDEXD(J) ) THEN
!                           WRITE (*,*) 'Indexed'
!                        END IF
!
!                        IF ( NULLOK(J) ) THEN
!                           WRITE (*,*) 'Nulls allowed'
!                        ELSE
!                           WRITE (*,*) 'Nulls not allowed'
!                        END IF
!
!                        WRITE (*,*) ' '
!                     END DO
!
!                     WRITE (*,*)
!                 .   '========================================'      //
!                 .   '========================================'
!
!                  END DO
!
!                  END
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
!-    SPICELIB Version 1.2.0, 02-JUL-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Fixed previous
!        version number.
!
!-    SPICELIB Version 1.1.0, 07-JUL-1996 (NJB)
!
!        Bug fix: correct parameter is now used to set dimension
!        of local variable SEGDSC.
!
!-    SPICELIB Version 1.0.0, 26-SEP-1995 (NJB)
!
!-&


! $ Index_Entries
!
!     return EK segment summary
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.1.0, 07-JUL-1996 (NJB)
!
!        Bug fix: correct parameter SDSCSZ is now used to set dimension
!        of local variable SEGDSC. Previously, the parameter
!        CDSCSZ had been used.
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               NTYPES
      PARAMETER           ( NTYPES = 4 )

      INTEGER               SHORT
      PARAMETER           ( SHORT  = 4 )

!
!     Local variables
!
      CHARACTER*(SHORT)     TYPSTR ( NTYPES )

      INTEGER               CDSCRS ( CDSCSZ, MXCLSG )
      INTEGER               I
      INTEGER               SEGDSC ( SDSCSZ )

!
!     Saved variables
!
      SAVE                  TYPSTR

!
!     Initial values
!
      DATA                  TYPSTR / 'CHR', 'DP', 'INT', 'TIME' /


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EKSSUM' )
      END IF

!
!     Get the info from a knowledgeable source.
!
      CALL ZZEKSINF ( HANDLE, SEGNO, TABNAM, SEGDSC, CNAMES, CDSCRS )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'EKSSUM' )
         RETURN
      END IF

      NROWS  =  SEGDSC ( NRIDX )
      NCOLS  =  SEGDSC ( NCIDX )

      DO I = 1, NCOLS

         DTYPES(I)  =  TYPSTR (  CDSCRS(TYPIDX,I)  )

         SIZES (I)  =  CDSCRS(SIZIDX,I)

         IF ( CDSCRS(TYPIDX,I) .EQ. CHR ) THEN
            STRLNS(I)  =  CDSCRS(LENIDX,I)
         ELSE
            STRLNS(I)  =  0
         END IF

         INDEXD(I)  =  CDSCRS(IXTIDX,I) .NE. IFALSE
         NULLOK(I)  =  CDSCRS(NFLIDX,I) .NE. IFALSE

      END DO

      CALL CHKOUT ( 'EKSSUM' )
      RETURN
      END
