!$Procedure     ZZEKWPAL ( EK, write paged array, logical )

      SUBROUTINE ZZEKWPAL ( HANDLE, SEGDSC, NVALS, LVALS, P, BASE )

! $ Abstract
!
!     Write a logical array out to a contiguous set of EK pages.
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
!
! $ Declarations


      INCLUDE  'ekbool.inc'
      INCLUDE  'ekdatpag.inc'
      INCLUDE  'ekpage.inc'
      INCLUDE  'eksegdsc.inc'
      INCLUDE  'ektype.inc'

      INTEGER               HANDLE
      INTEGER               SEGDSC ( * )
      INTEGER               NVALS
      LOGICAL               LVALS  ( * )
      INTEGER               P
      INTEGER               BASE

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle attached to EK file.
!     SEGDSC     I   Descriptor of segment that owns the array.
!     NVALS      I   Number of values to write.
!     LVALS      I   Logical values.
!     P          O   Number of first page containing array.
!     BASE       O   Base address of first page.
!
! $ Detailed_Input
!
!     HANDLE         the handle of an EK file that is open for writing.
!
!     SEGDSC         is a descriptor for the segment to which data is
!                    to be added.  The segment descriptor is not
!                    updated by this routine, but some fields in the
!                    descriptor will become invalid after this routine
!                    returns.
!
!     NVALS          is the number of logical values to write.
!
!     LVALS          is an array of logical values.  The values will
!                    be stored as characters, with one character used
!                    per element of LVALS.
!
! $ Detailed_Output
!
!     P              is the number of the first page to which the
!                    input values are written.  The character
!                    representing LVALS(1) is written to the first DAS
!                    character of page P.  The values are written
!                    to a contiguous set of pages in increasing order.
!
!     BASE           is the base address of P.  BASE is the predecessor
!                    of the first DAS address belonging to page P.
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
!     2)  If an I/O error occurs while reading or writing the indicated
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
!     This routine writes an array of logical values to a contiguous
!     set of new character pages.  The first element of the input array
!     is written to a range of values starting at the first DAS address
!     of the first page of the set.
!
!     This routine supports creation of null flag arrays for fixed-count
!     column classes.
!
!     Note that the values do not occupy a contiguous range of DAS
!     character words, since each page contains several addresses
!     reserved for bookkeeping information, and since there may be
!     unused space at the end of a data page.
!
! $ Examples
!
!     See ZZEKAC07.
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
!-    Beta Version 1.0.0, 08-NOV-1995 (NJB)
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Local variables
!
      CHARACTER*(PGSIZC)    PAGE

      INTEGER               FROM
      INTEGER               NPAGE
      INTEGER               TO

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZEKWPAL' )
      END IF

!
!     Decide how many pages are required to hold the array, and
!     allocate that many new, contiguous pages.
!
      NPAGE  =  ( NVALS + CPSIZE - 1 ) / CPSIZE

      CALL ZZEKACPS ( HANDLE, SEGDSC, CHR, NPAGE, P, BASE )

!
!     Write the input data out to the target file a page at a time.
!
!     We'll use FROM to indicate the element of LVALS we're
!     considering and TO to indicate the element of PAGE to write
!     to.
!
      TO      =  1
      PAGE    =  ' '

      DO FROM = 1, NVALS

!
!        The Assignment.
!
         IF ( LVALS(FROM) ) THEN
            PAGE( TO : TO ) =  CTRUE
         ELSE
            PAGE( TO : TO ) =  CFALSE
         END IF

         TO  =  TO + 1


         IF (  ( TO .GT. CPSIZE ) .OR. ( FROM .EQ. NVALS )  ) THEN
!
!           Either the current data page is full, or we've buffered
!           the last of the available data.  It's time to write out the
!           current page.
!
!           Write out the data page.
!
            CALL ZZEKPGWC ( HANDLE, P, PAGE  )

!
!           Set the link count.
!
            CALL ZZEKSLNK ( HANDLE, CHR, P, TO-1 )

!
!           Next page.
!
            P   =  P + 1
            TO  =  1

         END IF

      END DO


      CALL CHKOUT ( 'ZZEKWPAL' )
      RETURN
      END
