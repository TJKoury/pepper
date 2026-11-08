!$Procedure      ZZEKFF01 ( EK, finish fast load, segment type 1 )

      subroutine zzekff01 ( handle, segno, rcptrs )

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


      include 'ekdatpag.inc'
      include 'ekglimit.inc'
      include 'ekpage.inc'
      include 'ekrecptr.inc'
      include 'eksegdsc.inc'
      include 'ektype.inc'

      integer               handle
      integer               segno
      integer               rcptrs ( * )

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
      logical               return

!
!     Local parameters
!
      integer               bufsiz
      parameter           ( bufsiz = 100 )

!
!     Local variables
!
      integer               addrss
      integer               adrbuf ( bufsiz )
      integer               base
      integer               col
      integer               colidx
      integer               colord ( mxclsg )
      integer               i
      integer               ipage  ( pgsizi )
      integer               j
      integer               loc
      integer               mbase
      integer               ncols
      integer               npage
      integer               nr
      integer               nrows
      integer               nrp
      integer               p
      integer               pagloc
      integer               pbase
      integer               recno
      integer               remain
      integer               row
      integer               rpsize
      integer               segdsc ( sdscsz )
      integer               stkbas
      integer               stkhan
      integer               stkseg
      integer               tree

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin ( 'ZZEKFF01' )

!
!     Initialize all arrays.
!
      call cleari ( bufsiz, adrbuf )
      call cleari ( mxclsg, colord )
      call cleari ( pgsizi, ipage  )
      call cleari ( sdscsz, segdsc )

!
!     Dig the handle and segment number out of the EK stack.  If the
!     stacked values don't match the inputs HANDLE and SEGNO, we've
!     got trouble.
!
      call zzeksrd ( 1, 1, stkhan )
      call zzeksrd ( 2, 2, stkseg )

      if (  ( stkhan .ne. handle ) .or. ( stkseg .ne. segno )  ) then

         call setmsg ( 'Attempt to finish fast load of wrong segment.'// &
                       '  Input segment number is #; stacked segment '// &
                       'number is #.  Input handle is #; stacked '    // &
                       'handle is #.'                                 )
         call errint ( '#',  segno                                    )
         call errint ( '#',  stkseg                                   )
         call errint ( '#',  handle                                   )
         call errint ( '#',  stkhan                                   )
         call sigerr ( 'SPICE(WRONGSEGMENT)'                          )
         call chkout ( 'ZZEKFF01'                                     )
         return

      end if

!
!     Look up the segment descriptor for the indicated segment.  Find
!     out how many rows and columns the segment contains.
!
      call zzekmloc (  handle,  segno,    p,             mbase   )
      call dasrdi   (  handle,  mbase+1,  mbase+sdscsz,  segdsc  )

      nrows  =  segdsc ( nridx )
      ncols  =  segdsc ( ncidx )

!
!     Determine the order in which the columns were added.  The order
!     may differ from that in which the columns were declared.  The
!     ordinal position of each column is stored on the stack right
!     before its address data.  COLORD will map ordinal positions given
!     by a column declaration to ordinal positions on the stack.
!
!
      do i = 1, ncols

         loc  =  (i-1)*(nrows+1)  +  3

         call zzeksrd ( loc, loc, colidx )

         colord ( colidx ) = i

      end do

!
!     We'll need to create a record pointer structure for each row
!     in the segment.  We compute the number of record pointers that
!     can fit on one page.  We also compute the number of pages we'll
!     need to hold the pointers.
!
      rpsize  =    dptbas + ncols
      nrp     =    ipsize / rpsize
      npage   =  ( nrows + nrp - 1 ) / nrp

!
!     We'll write out record pointers a pageful at a time.  Each
!     record pointer is initialized to indicate that the record is
!     old, and that there is no corresponding modified record.
!
      remain  =  nrows
      recno   =  0

      do i = 1, npage
!
!        Get the base address of the current page.  The address
!        can be derived from the address of the first record pointer
!        structure on the page.
!
         addrss  =  rcptrs(recno+1) + 1

         call zzekpgpg ( int, addrss, p, pbase )

         call cleari   ( ipsize, ipage )

!
!        NR is the number of record pointers we'll write to this page.
!
         nr  =  min ( nrp, remain )

         do j = 1, nr
!
!           Initialize the modified record pointer and status for
!           each record pointer on the page.
!
            base                     =  (j-1) * rpsize
            ipage ( base + staidx )  =  old
            ipage ( base + rcpidx )  =  uninit

         end do

!
!        For each column, take NR addresses off the stack and
!        write them into the page.
!
         do col = 1, ncols
!
!           The stack starts out with the target file handle and
!           segment number.  Next comes the data for each column.
!           Each column is identified by its ordinal position.  The
!           addresses for the data of each column follow.  The addresses
!           for each column are stored contiguously.
!
            j       =  colord( col )
            stkbas  =  (j-1)*(nrows+1)  +  3
            loc     =  stkbas           +  recno

            call zzeksrd ( loc+1, loc+nr, adrbuf )

            do row = 1, nr

               base           =  (row-1) * rpsize
               pagloc         =  base + dptbas + col
               ipage(pagloc)  =  adrbuf(row)

            end do

         end do

!
!        Write out the initialized pointer page.
!
         call zzekpgwi ( handle, p, ipage )


         recno   =  recno   +  nr
         remain  =  remain  -  nr

      end do

!
!     Create the record pointer tree for this segment.
!
      call zzektrit (  handle,  tree                   )
      call zzektr1s (  handle,  tree,  nrows,  rcptrs  )

!
!     Update the record tree pointer and row count in the segment
!     descriptor.  Set the records of the last DAS words in use
!     to their maximum values, to ensure allocation of new pages
!     if further writes are done.
!
      call zzekmloc (  handle,  segno,        p,            base   )
      call dasudi   (  handle,  base+rtidx,   base+rtidx,   tree   )
      call dasudi   (  handle,  base+nridx,   base+nridx,   nrows  )
      call dasudi   (  handle,  base+lcwidx,  base+lcwidx,  cpsize  )
      call dasudi   (  handle,  base+ldwidx,  base+ldwidx,  dpsize  )
      call dasudi   (  handle,  base+liwidx,  base+liwidx,  ipsize  )


      call chkout ( 'ZZEKFF01' )
      return
      end
