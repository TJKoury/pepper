!$Procedure     ZZEKWPAI ( EK, write paged array, integer )

      SUBROUTINE ZZEKWPAI ( HANDLE, SEGDSC, NVALS, IVALS, P, BASE )

! $ Abstract
!
!     Write an integer array out to a contiguous set of EK pages.
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


      INCLUDE  'ekdatpag.inc'
      INCLUDE  'ekpage.inc'
      INCLUDE  'eksegdsc.inc'
      INCLUDE  'ektype.inc'

      INTEGER               HANDLE
      INTEGER               SEGDSC ( * )
      INTEGER               NVALS
      INTEGER               IVALS  ( * )
      INTEGER               P
      INTEGER               BASE

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle attached to EK file.
!     SEGDSC     I   Descriptor of segment that owns the array.
!     NVALS      I   Number of values to write.
!     IVALS      I   Integer values.
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
!     NVALS          is the number of integer values to write.
!
!     IVALS          is an array of integer values.
!
! $ Detailed_Output
!
!     P              is the number of the first page to which the
!                    input values are written.  IVALS(1) is written to
!                    the first word of page P.  The values are written
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
!     This routine writes an array of integer values to a contiguous
!     set of new integer pages.  The first element of the input array
!     is written to the first DAS address of the first page of the set.
!
!     Note that the values do not occupy a contiguous range of DAS
!     integer words, since each page contains several addresses
!     reserved for bookkeeping information.  However, since each page
!     contains exactly IPSIZE integers and has size PGSIZI, it's easy
!     to compute the DAS address of the Ith element in the array:
!
!        Q          =   (I-1)  /  IPSIZE
!        R          =   I  -  Q * IPSIZE
!
!        ADDRSS(I)  =  BASE +  Q * PGSIZI + R
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
      INTEGER               FROM
      INTEGER               NPAGE
      INTEGER               PAGE   ( PGSIZI )
      INTEGER               TO

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZEKWPAI' )
      END IF

!
!     Decide how many pages are required to hold the array, and
!     allocate that many new, contiguous pages.
!
      NPAGE  =  ( NVALS + IPSIZE - 1 ) / IPSIZE

      CALL ZZEKACPS ( HANDLE, SEGDSC, INT, NPAGE, P, BASE )

!
!     We'll use FROM to indicate the element of IVALS we're
!     considering and TO to indicate the element of PAGE to write
!     to.
!
      TO  =  1
      CALL CLEARI ( PGSIZI, PAGE )


      DO FROM = 1, NVALS
!
!        The Assignment.
!
         PAGE(TO) =  IVALS(FROM)
         TO       =  TO + 1


         IF (  ( TO .GT. IPSIZE ) .OR. ( FROM .EQ. NVALS )  ) THEN
!
!           Either the current data page is full, or we've buffered
!           the last of the available data.  It's time to write out the
!           current page.  First set the link count.
!
            PAGE ( ILCIDX ) =  TO - 1

!
!           Write out the data page.
!
            CALL ZZEKPGWI ( HANDLE, P, PAGE  )

!
!           Next page.
!
            P   =  P + 1
            TO  =  1

         END IF

      END DO


      CALL CHKOUT ( 'ZZEKWPAI' )
      RETURN
      END
