!$Procedure ZZEKIF01 ( EK, initialize type 1 segment for fast load )

      SUBROUTINE ZZEKIF01 ( HANDLE, SEGNO, RCPTRS )

! $ Abstract
!
!     Initialize a new type 1 EK segment to allow fast loading.
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


      INCLUDE 'ekdatpag.inc'
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
!     RCPTRS     O   Array of record pointers.
!
! $ Detailed_Input
!
!     HANDLE         is the handle of an EK file open for write access.
!                    A new type 1 segment is to be created in this file
!                    via a fast load.  The segment's metadata has
!                    already been set up by EKBSEG.
!
!     SEGNO          is the number of the segment to prepare for a
!                    fast load.
!
! $ Detailed_Output
!
!     RCPTRS         is an array of record pointers for the input
!                    segment.  This array must not be modified by the
!                    caller.
!
!                    The array RCPTRS must be passed as an input to
!                    each column addition routine called while
!                    writing the specified segment.
!
!                    RCPTRS must be declared with dimension equal to
!                    the number of rows in the segment.
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
!     This routine carries out the type-1-specific preparation for
!     populating a type 1 EK segment with data via the fast column
!     loader routines.  This routine expects the segment's metadata to
!     already have been written by EKBSEG.
!
! $ Examples
!
!     See EKIFLD.
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
!-    SPICELIB Version 1.0.1, 06-SEP-2006 (NJB)
!
!        Added Restrictions section to header.  Changed
!        previous version line's product from "Beta" to "SPICELIB."
!
!-    SPICELIB Version 1.0.0, 06-NOV-1995 (NJB)
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Local variables
!
      INTEGER               BASE
      INTEGER               I
      INTEGER               J
      INTEGER               NCOLS
      INTEGER               NPAGE
      INTEGER               NR
      INTEGER               NROWS
      INTEGER               NRP
      INTEGER               P
      INTEGER               PBASE
      INTEGER               RECNO
      INTEGER               REMAIN
      INTEGER               RPSIZE
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               TOP

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZEKIF01' )
      END IF

!
!     Read in the segment descriptor.
!
      CALL ZZEKSDSC (  HANDLE,  SEGNO,  SEGDSC )

      NCOLS  =  SEGDSC(NCIDX)
      NROWS  =  SEGDSC(NRIDX)

!
!     Empty the EK scratch area stack.
!
      CALL ZZEKSTOP ( TOP )
      CALL ZZEKSDEC ( TOP )

!
!     Push the handle and segment number onto the stack.
!
      CALL ZZEKSPSH ( 1, HANDLE )
      CALL ZZEKSPSH ( 1, SEGNO  )

!
!     The segment will require a record pointer structure for each row
!     in the segment.  Right now, all we're going to do is allocate
!     integer pages to hold these structures and save the base
!     addresses of each structure.
!
!     We compute the number of record pointers that can fit on one page.
!     We also compute the number of pages we'll need to hold the
!     pointers.
!
      RPSIZE  =    DPTBAS + NCOLS
      NRP     =    IPSIZE / RPSIZE
      NPAGE   =  ( NROWS + NRP - 1 ) / NRP

!
!     We'll compute addresses of record pointers a pageful at a time.
!
      REMAIN  =  NROWS
      RECNO   =  0

      DO I = 1, NPAGE
!
!        Allocate a page to hold the record pointers.  A page from
!        the free list is acceptable, hence the argument .FALSE.
!        passed to ZZEKAPS.
!
         CALL ZZEKAPS  ( HANDLE, SEGDSC, INT, .FALSE., P, PBASE )

!
!        NR is the number of record pointers we'll eventually write to
!        this page.
!
         NR  =  MIN ( NRP, REMAIN )

         DO J = 1, NR
!
!           Record the base address of the current record pointer
!           in the record pointer array.
!
            BASE                =  (J-1) * RPSIZE
            RCPTRS ( RECNO+J )  =  PBASE + BASE

         END DO

         RECNO   =  RECNO   +  NR
         REMAIN  =  REMAIN  -  NR

      END DO


      CALL CHKOUT ( 'ZZEKIF01' )
      RETURN
      END
