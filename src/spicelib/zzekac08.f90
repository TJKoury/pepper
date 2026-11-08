!$Procedure     ZZEKAC08 ( EK, add class 8 column to segment )

      subroutine zzekac08 (  handle,  segdsc,  coldsc, &
                             dvals,   nlflgs,  wkindx  )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Add an entire class 8 column to an EK segment.
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


      include  'ekbool.inc'
      include  'ekclas08.inc'
      include  'ekcnamsz.inc'
      include  'ekcoldsc.inc'
      include  'ekdatpag.inc'
      include  'ekpage.inc'
      include  'eksegdsc.inc'
      include  'ektype.inc'

      integer(ip)           handle
      integer(ip)           segdsc ( * )
      integer(ip)           coldsc ( * )
      real(wp)              dvals  ( * )
      logical               nlflgs ( * )
      integer(ip)           wkindx ( * )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle attached to new EK file.
!     SEGDSC     I   Segment descriptor.
!     COLDSC     I   Column descriptor.
!     DVALS      I   D.p. values to add to column.
!     NLFLGS     I   Array of null flags for column entries.
!     WKINDX    I-O  Work space for column index.
!
! $ Detailed_Input
!
!     HANDLE         the handle of an EK file that is open for writing.
!                    A `begin segment for fast load' operation must
!                    have already been performed for the designated
!                    segment.
!
!     SEGDSC         is a descriptor for the segment to which data is
!                    to be added.  The segment descriptor is not
!                    updated by this routine, but some fields in the
!                    descriptor will become invalid after this routine
!                    returns.
!
!     COLDSC         is a descriptor for the column to be added.  The
!                    column attributes must be filled in, but any
!                    pointers may be uninitialized.
!
!     DVALS          is an array containing the entire set of column
!                    entries for the specified column.  The entries
!                    are listed in row-order:  the column entry for the
!                    first row of the segment is first, followed by the
!                    column entry for the second row, and so on.  The
!                    number of column entries must match the declared
!                    number of rows in the segment.  Elements must be
!                    allocated for each column entry, including null
!                    entries.
!
!     NLFLGS         is an array of logical flags indicating whether
!                    the corresponding entries are null.  If the Ith
!                    element of NLFLGS is .FALSE., the Ith column entry
!                    defined by DVALS is added to the specified segment
!                    in the specified kernel file.
!
!                    If the Ith element of NLFGLS is .TRUE., the
!                    contents of the Ith column entry are undefined.
!
!                    NLFLGS is used only for columns that allow null
!                    values; it's ignored for other columns.
!
!     WKINDX         is a work space array used for building a column
!                    index.  If the column is indexed, the dimension of
!                    WKINDX must be at NROWS, where NROWS is the number
!                    of rows in the column.  If the column is not
!                    indexed, this work space is not used, so the
!                    dimension may be any positive value.
!
! $ Detailed_Output
!
!     None.  See $Particulars for a description of the effect of this
!     routine.
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
!     This routine operates by side effects:  it modifies the named
!     EK file by adding data to the specified column.  This routine
!     writes the entire contents of the specified column in one shot.
!     This routine creates columns much more efficiently than can be
!     done by sequential calls to EKACED, but has the drawback that
!     the caller must use more memory for the routine's inputs.  This
!     routine cannot be used to add data to a partially completed
!     column.
!
!     Class 8 columns have fixed record counts and contain scalar,
!     double precision data.
!
! $ Examples
!
!     See EKACLD.
!
! $ Restrictions
!
!     1)  This routine assumes the EK file has been set up
!         properly for a fast load operation.
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
!-    Beta Version 1.0.0, 13-NOV-1995 (NJB)
!
!-&

!
!     SPICELIB functions
!
      logical               return

!
!     Local variables
!
      character*(cnamsz)    column

      real(wp)              page   ( pgsizd )

      integer(ip)           class
      integer(ip)           cmbase
      integer(ip)           colidx
      integer(ip)           datbas
      integer(ip)           dscbas
      integer(ip)           from
      integer(ip)           idxbas
      integer(ip)           idxpag
      integer(ip)           idxtyp
      integer(ip)           p
      integer(ip)           mbase
      integer(ip)           nflbas
      integer(ip)           nflpag
      integer(ip)           npage
      integer(ip)           nrows
      integer(ip)           nulptr
      integer(ip)           to

      logical               indexd
      logical               nullok

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'ZZEKAC08' )
      end if

!
!     Grab the column's attributes.
!
      class  =  coldsc( clsidx )
      idxtyp =  coldsc( ixtidx )
      nulptr =  coldsc( nflidx )
      colidx =  coldsc( ordidx )

      nullok =  nulptr .ne. ifalse
      indexd =  idxtyp .ne. ifalse

!
!     This column had better be class 8.
!
      if ( class .ne. 8 ) then

         call zzekcnam ( handle, coldsc, column )

         call setmsg ( 'Column class code # found in descriptor for ' // &
                       'column #.  Class should be 8.'                 )
         call errint ( '#', class                                      )
         call errch  ( '#', column                                     )
         call sigerr ( 'SPICE(NOCLASS)'                                )
         call chkout ( 'ZZEKAC08'                                      )
         return

      end if

!
!     Find the number of rows in the segment.
!
      nrows = segdsc ( nridx )

!
!     Decide how many pages are required to hold the array, and
!     allocate that many new, contiguous pages.
!
      npage  =  ( nrows + dpsize - 1 ) / dpsize

      call zzekacps ( handle, segdsc, dp, npage, p, datbas )

!
!     We'll use FROM to indicate the element of DVALS we're
!     considering and TO to indicate the element of PAGE to write
!     to.
!
      to  =  1
      call cleard ( pgsizd, page )


      do from = 1, nrows

!
!        The Assignment.
!
         if (  ( .not. nullok ) .or. ( .not. nlflgs(from) )  ) then
!
!           The current item is non-null.
!
            page(to)  =  dvals(from)

         end if

         to =  to + 1


         if (  ( to .gt. dpsize ) .or. ( from .eq. nrows )  ) then
!
!           Either the current data page is full, or we've buffered
!           the last of the available data.  It's time to write out the
!           current page.  First set the link count.
!
            page ( dlcidx ) =  to - 1

!
!           Write out the data page.
!
            call zzekpgwd ( handle, p, page  )

!
!           Next page.
!
            p   =  p + 1
            to  =  1

         end if

      end do


!
!     Update the column's metadata area to point to the data array.
!
      cmbase = coldsc(metidx)

      call dasudi ( handle, cmbase+dbix08, cmbase+dbix08, datbas )

!
!     If the column is supposed to have an index, now is the time to
!     build that index.  Type 2 indexes are just order vectors.
!
      if ( indexd ) then
!
!        Compute the order vector.
!
         call zzekordd ( dvals,  nullok, nlflgs, nrows, wkindx )

!
!        Write out the index.
!
         call zzekwpai ( handle, segdsc, nrows, wkindx, idxpag, idxbas )

!
!        Update the column's metadata to point to the index.  The
!        pointer indicates base address of the index.  Also set the
!        index type in the column descriptor.
!
         mbase   =  segdsc ( imdidx )
         dscbas  =  mbase  +  sdscsz  +  (colidx-1)*cdscsz

         call dasudi ( handle,  dscbas+ixpidx,  dscbas+ixpidx,  idxbas )
         call dasudi ( handle,  dscbas+ixtidx,  dscbas+ixtidx,  2      )

      end if


      if ( nullok ) then
!
!        Nulls are allowed.  Write out the null flag array.
!
         call zzekwpal ( handle, segdsc, nrows, nlflgs, nflpag, nflbas )

!
!        Update the column's metadata area to point to the null flag
!        array.
!
         call dasudi ( handle, cmbase+nfix08, cmbase+nfix08, nflbas )

      end if


      call chkout ( 'ZZEKAC08' )
      return
      end
