!$Procedure EKSSUM ( EK, return segment summary )

      subroutine ekssum (  handle,  segno,   tabnam,  nrows, &
                           ncols,   cnames,  dtypes,  sizes, &
                           strlns,  indexd,  nullok          )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Return summary information for a specified segment in a
!     specified EK.
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

      implicit none

      include 'ekbool.inc'
      include 'ekcoldsc.inc'
      include 'ekglimit.inc'
      include 'eksegdsc.inc'
      include 'ektype.inc'

      integer(ip)           handle
      integer(ip)           segno
      character(*)          tabnam
      integer(ip)           nrows
      integer(ip)           ncols
      character(*)          cnames ( * )
      character(*)          dtypes ( * )
      integer(ip)           sizes  ( * )
      integer(ip)           strlns ( * )
      logical               indexd ( * )
      logical               nullok ( * )

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
      logical               failed
      logical               return

!
!     Local parameters
!
      integer(ip)           ntypes
      parameter           ( ntypes = 4 )

      integer(ip)           short
      parameter           ( short  = 4 )

!
!     Local variables
!
      character(short)      typstr ( ntypes )

      integer(ip)           cdscrs ( cdscsz, mxclsg )
      integer(ip)           i
      integer(ip)           segdsc ( sdscsz )

!
!     Saved variables
!
      save                  typstr

!
!     Initial values
!
      data                  typstr / 'CHR', 'DP', 'INT', 'TIME' /


!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'EKSSUM' )
      end if

!
!     Get the info from a knowledgeable source.
!
      call zzeksinf ( handle, segno, tabnam, segdsc, cnames, cdscrs )

      if ( failed() ) then
         call chkout ( 'EKSSUM' )
         return
      end if

      nrows  =  segdsc ( nridx )
      ncols  =  segdsc ( ncidx )

      do i = 1, ncols

         dtypes(i)  =  typstr (  cdscrs(typidx,i)  )

         sizes (i)  =  cdscrs(sizidx,i)

         if ( cdscrs(typidx,i) == chr ) then
            strlns(i)  =  cdscrs(lenidx,i)
         else
            strlns(i)  =  0
         end if

         indexd(i)  =  cdscrs(ixtidx,i) /= ifalse
         nullok(i)  =  cdscrs(nflidx,i) /= ifalse

      end do

      call chkout ( 'EKSSUM' )
      return
      end
