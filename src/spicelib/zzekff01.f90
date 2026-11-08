!$Procedure      ZZEKFF01 ( EK, finish fast load, segment type 1 )

      SUBROUTINE ZZEKFF01 ( HANDLE, SEGNO, RCPTRS )

! $ Abstract
!
!     Complete a fast load operation on a new type 1 E-kernel segment.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!
! $ Declarations


      INCLUDE 'ekdatpag.inc'
      INCLUDE 'ekglimit.inc'
      INCLUDE 'ekpage.inc'
      INCLUDE 'ekrecptr.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'

      INTEGER               HANDLE
      INTEGER               SEGNO
      INTEGER               RCPTRS ( * )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     SEGNO      I   Segment number.
!     RCPTRS    I-O  Record pointers.
!
! $ Detailed_Input
!
!     HANDLE         the handle of an EK file that is open for writing.
!                    A `begin segment for fast load' operation must
!                    have already been performed for the designated
!                    segment.
!
!     SEGNO          is the number of the type 1 segment to complete.
!
!     RCPTRS         is an array of record pointers for the input
!                    segment.  This array is obtained as an output
!                    from EKIFLD, the routine called to initiate a
!                    fast load.
!
! $ Detailed_Output
!
!     WORK           is the input work space array, after use.  WORK
!                    will generally be modified by this routine.
!
!     See the $Particulars section for a description of the
!     effects of this routine.
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
!     2)  If an attempt is made to finish a segment other than the
!         one last initialized by EKIFLD, the error SPICE(WRONGSEGMENT)
!         is signaled.
!
!     3)  If an I/O error occurs while reading or writing the indicated
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
!     This routine completes a type 1 EK segment after the data has been
!     written via the fast column loader routines.
!
! $ Examples
!
!     See EKFFLD.
!
! $ Restrictions
!
!     1)  Only one segment can be created at a time using the fast
!         load routines.
!
!     2)  No other EK operation may interrupt a fast load.  For
!         example, it is not valid to issue a query while a fast load
!         is in progress.
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
!-    SPICELIB Version 1.1.0, 11-OCT-2021 (NJB)
!
!        All local arrays are now completely initialized.
!
!        Corrected typos in comments.
!
!-    Beta Version 1.0.0, 08-NOV-1995 (NJB)
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = 100 )

!
!     Local variables
!
      INTEGER               ADDRSS
      INTEGER               ADRBUF ( BUFSIZ )
      INTEGER               BASE
      INTEGER               COL
      INTEGER               COLIDX
      INTEGER               COLORD ( MXCLSG )
      INTEGER               I
      INTEGER               IPAGE  ( PGSIZI )
      INTEGER               J
      INTEGER               LOC
      INTEGER               MBASE
      INTEGER               NCOLS
      INTEGER               NPAGE
      INTEGER               NR
      INTEGER               NROWS
      INTEGER               NRP
      INTEGER               P
      INTEGER               PAGLOC
      INTEGER               PBASE
      INTEGER               RECNO
      INTEGER               REMAIN
      INTEGER               ROW
      INTEGER               RPSIZE
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               STKBAS
      INTEGER               STKHAN
      INTEGER               STKSEG
      INTEGER               TREE

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZEKFF01' )

!
!     Initialize all arrays.
!
      CALL CLEARI ( BUFSIZ, ADRBUF )
      CALL CLEARI ( MXCLSG, COLORD )
      CALL CLEARI ( PGSIZI, IPAGE  )
      CALL CLEARI ( SDSCSZ, SEGDSC )

!
!     Dig the handle and segment number out of the EK stack.  If the
!     stacked values don't match the inputs HANDLE and SEGNO, we've
!     got trouble.
!
      CALL ZZEKSRD ( 1, 1, STKHAN )
      CALL ZZEKSRD ( 2, 2, STKSEG )

      IF (  ( STKHAN .NE. HANDLE ) .OR. ( STKSEG .NE. SEGNO )  ) THEN

         CALL SETMSG ( 'Attempt to finish fast load of wrong segment.'// &
                       '  Input segment number is #; stacked segment '// &
                       'number is #.  Input handle is #; stacked '    // &
                       'handle is #.'                                 )
         CALL ERRINT ( '#',  SEGNO                                    )
         CALL ERRINT ( '#',  STKSEG                                   )
         CALL ERRINT ( '#',  HANDLE                                   )
         CALL ERRINT ( '#',  STKHAN                                   )
         CALL SIGERR ( 'SPICE(WRONGSEGMENT)'                          )
         CALL CHKOUT ( 'ZZEKFF01'                                     )
         RETURN

      END IF

!
!     Look up the segment descriptor for the indicated segment.  Find
!     out how many rows and columns the segment contains.
!
      CALL ZZEKMLOC (  HANDLE,  SEGNO,    P,             MBASE   )
      CALL DASRDI   (  HANDLE,  MBASE+1,  MBASE+SDSCSZ,  SEGDSC  )

      NROWS  =  SEGDSC ( NRIDX )
      NCOLS  =  SEGDSC ( NCIDX )

!
!     Determine the order in which the columns were added.  The order
!     may differ from that in which the columns were declared.  The
!     ordinal position of each column is stored on the stack right
!     before its address data.  COLORD will map ordinal positions given
!     by a column declaration to ordinal positions on the stack.
!
!
      DO I = 1, NCOLS

         LOC  =  (I-1)*(NROWS+1)  +  3

         CALL ZZEKSRD ( LOC, LOC, COLIDX )

         COLORD ( COLIDX ) = I

      END DO

!
!     We'll need to create a record pointer structure for each row
!     in the segment.  We compute the number of record pointers that
!     can fit on one page.  We also compute the number of pages we'll
!     need to hold the pointers.
!
      RPSIZE  =    DPTBAS + NCOLS
      NRP     =    IPSIZE / RPSIZE
      NPAGE   =  ( NROWS + NRP - 1 ) / NRP

!
!     We'll write out record pointers a pageful at a time.  Each
!     record pointer is initialized to indicate that the record is
!     old, and that there is no corresponding modified record.
!
      REMAIN  =  NROWS
      RECNO   =  0

      DO I = 1, NPAGE
!
!        Get the base address of the current page.  The address
!        can be derived from the address of the first record pointer
!        structure on the page.
!
         ADDRSS  =  RCPTRS(RECNO+1) + 1

         CALL ZZEKPGPG ( INT, ADDRSS, P, PBASE )

         CALL CLEARI   ( IPSIZE, IPAGE )

!
!        NR is the number of record pointers we'll write to this page.
!
         NR  =  MIN ( NRP, REMAIN )

         DO J = 1, NR
!
!           Initialize the modified record pointer and status for
!           each record pointer on the page.
!
            BASE                     =  (J-1) * RPSIZE
            IPAGE ( BASE + STAIDX )  =  OLD
            IPAGE ( BASE + RCPIDX )  =  UNINIT

         END DO

!
!        For each column, take NR addresses off the stack and
!        write them into the page.
!
         DO COL = 1, NCOLS
!
!           The stack starts out with the target file handle and
!           segment number.  Next comes the data for each column.
!           Each column is identified by its ordinal position.  The
!           addresses for the data of each column follow.  The addresses
!           for each column are stored contiguously.
!
            J       =  COLORD( COL )
            STKBAS  =  (J-1)*(NROWS+1)  +  3
            LOC     =  STKBAS           +  RECNO

            CALL ZZEKSRD ( LOC+1, LOC+NR, ADRBUF )

            DO ROW = 1, NR

               BASE           =  (ROW-1) * RPSIZE
               PAGLOC         =  BASE + DPTBAS + COL
               IPAGE(PAGLOC)  =  ADRBUF(ROW)

            END DO

         END DO

!
!        Write out the initialized pointer page.
!
         CALL ZZEKPGWI ( HANDLE, P, IPAGE )


         RECNO   =  RECNO   +  NR
         REMAIN  =  REMAIN  -  NR

      END DO

!
!     Create the record pointer tree for this segment.
!
      CALL ZZEKTRIT (  HANDLE,  TREE                   )
      CALL ZZEKTR1S (  HANDLE,  TREE,  NROWS,  RCPTRS  )

!
!     Update the record tree pointer and row count in the segment
!     descriptor.  Set the records of the last DAS words in use
!     to their maximum values, to ensure allocation of new pages
!     if further writes are done.
!
      CALL ZZEKMLOC (  HANDLE,  SEGNO,        P,            BASE   )
      CALL DASUDI   (  HANDLE,  BASE+RTIDX,   BASE+RTIDX,   TREE   )
      CALL DASUDI   (  HANDLE,  BASE+NRIDX,   BASE+NRIDX,   NROWS  )
      CALL DASUDI   (  HANDLE,  BASE+LCWIDX,  BASE+LCWIDX,  CPSIZE  )
      CALL DASUDI   (  HANDLE,  BASE+LDWIDX,  BASE+LDWIDX,  DPSIZE  )
      CALL DASUDI   (  HANDLE,  BASE+LIWIDX,  BASE+LIWIDX,  IPSIZE  )


      CALL CHKOUT ( 'ZZEKFF01' )
      RETURN
      END
