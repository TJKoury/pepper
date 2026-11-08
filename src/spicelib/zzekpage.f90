!$Procedure  ZZEKPAGE ( Private: Manage EK DAS paging system )

      subroutine zzekpage (  handle,  type,   addrss,  stat,  p, &
                             pagec,   paged,  pagei,   base,  value  )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Manage EK DAS paging system.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     PRIVATE
!
! $ Declarations

      include 'ekarch.inc'
      include 'ekpage.inc'
      include 'ektype.inc'

      integer(ip)           handle
      integer(ip)           type
      integer(ip)           addrss
      character(*)          stat
      integer(ip)           p
      character(*)          pagec
      real(wp)              paged ( * )
      integer(ip)           pagei ( * )
      integer(ip)           base
      integer(ip)           value

! $ Brief_I/O
!
!     Variable  I/O  Entries
!     --------  ---  --------------------------------------------------
!     HANDLE     I   PGIN, PGAN, PGAL, PGFR, PGRx, PGWx, PGST.
!     TYPE       I   PGBS, PGPG.
!     ADDRSS     I   PGPG.
!     STAT       I   PGST.
!     P         I-O  PGAN, PGAL, PGFR, PGRx, PGWx, PGBS, PGPG.
!     PAGEC     I-O  PGRC, PGWC.
!     PAGED     I-O  PGRD, PGWD.
!     PAGEI     I-O  PGRI, PGWI.
!     BASE       O   PGAN, PGAL, PGBS, PGPG.
!     VALUE      O   PGST.
!
! $ Detailed_Input
!
!     See the entry points for descriptions of their inputs.
!
! $ Detailed_Output
!
!     See the entry points for descriptions of their outputs.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If this routine is called directly, the error
!         SPICE(BOGUSENTRY) will be signaled.
!
!     See the entry points for discussions of errors particular to
!     those routines.
!
! $ Files
!
!     This suite of routines provides paged access to DAS files.  Only
!     DAS files initialized via a call to ZZEKPGIN may be written or
!     read by these routines.
!
! $ Particulars
!
!     The EK paging system provides a means for the rest of the EK
!     system to allocate and deallocate contiguous blocks of DAS
!     addresses of character, d.p. and integer type.  The rest of the EK
!     system never accesses EK files directly; it only reads and writes
!     pages allocated via this system.
!
!     Much of the page allocation and de-allocation performed by
!     higher-level routines is done via the routines ZZEKAPS and
!     ZZEKDPS; those routines should be called if applicable, rather
!     than ZZEKPGAL, ZZEKPGAN, or ZZEKPGFR.
!
! $ Examples
!
!     Initialization:               see EKOPN.
!     Page allocation:              see EKAPS.
!     Writing:                      see ZZEKAD01, ZZEKAD02, ZZEKAD03.
!     Reading:                      see ZZEKRD01, ZZEKRD02, ZZEKRD03.
!     Freeing pages:                see ZZEKDPS.
!     Address-to-page mapping:      see EKDELR.
!     Page number-to-base mapping:  see ZZEKAD0x
!
! $ Restrictions
!
!     1) Only `empty' DAS files may be initialized for paged access.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     B.V. Semenov       (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 14-OCT-2021 (BVS)
!
!        Bug fix: fixed routine name in CHKIN/CHKOUT calls in
!        entry point ZZEKPGPG (ZZEKPGBS -> ZZEKPGPG).
!
!-    SPICELIB Version 1.1.0, 07-FEB-2015 (NJB)
!
!        Now uses ERRHAN to insert DAS file name into
!        long error messages.
!
!-    Beta Version 1.0.0, 20-OCT-1995 (NJB)
!
!-&

!
!     SPICELIB functions
!
      logical               eqstr
      logical               failed

!
!     Local variables
!
!     Note:  the integer fill buffer should be as large as the maximum
!     of the integer page size and the metadata area size.
!
      character(pgsizc)     cfill
      character(5)          encpag

      real(wp)              dfill  ( pgsizd )
      real(wp)              dpptr

      integer(ip)           addr
      integer(ip)           e
      integer(ip)           forwrd
      integer(ip)           freec
      integer(ip)           freed
      integer(ip)           freei
      integer(ip)           ifill  ( pgsizi )
      integer(ip)           l
      integer(ip)           lastc
      integer(ip)           lastd
      integer(ip)           lasti
      integer(ip)           nfreec
      integer(ip)           nfreed
      integer(ip)           nfreei
      integer(ip)           npc
      integer(ip)           npd
      integer(ip)           npi

!
!     Saved variables
!
      save

      call sigerr ( 'SPICE(BOGUSENTRY)' )
      return

!$Procedure  ZZEKPGIN ( Private: Initialize DAS for paged access )

      entry zzekpgin ( handle )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Initialize an open DAS file for paged access.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     PRIVATE
!
! $ Declarations
!
!     INTEGER               HANDLE
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   DAS file handle.
!
! $ Detailed_Input
!
!     HANDLE         is a handle of a DAS file open for write access.
!                    The file must be empty:  the last address of
!                    each type (character, d.p. and integer) must be
!                    zero.
!
! $ Detailed_Output
!
!     None.  This routine operates by side effects; see $Particulars
!     for a description of the effect of this routine.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the DAS file designated by HANDLE is not empty, the error
!         SPICE(DASNOTEMPTY) is signaled.
!
!     2)  Any read or write errors detected during reading or writing
!         the DAS file will be diagnosed by routines called by this
!         routine.
!
! $ Files
!
!     This suite of routines provides paged access to DAS files.  Only
!     DAS files initialized via a call to ZZEKPGIN may be written or
!     read by these routines.
!
! $ Particulars
!
!     This routine initializes a DAS file for paged access.
!     Initialization consists of:
!
!        - Setting up the metadata area.  This structure is defined in
!          the include file ekpage.inc.  For each data type, there is
!          a free list pointer and an allocated page count.
!
!        - Writing the architecture code to the file.  This code is
!          defined in the include file ekarch.inc.
!
! $ Examples
!
!     See EKOPN.
!
! $ Restrictions
!
!     1) Only `empty' DAS files may be initialized for paged access.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 07-FEB-2015 (NJB)
!
!        Now uses ERRHAN to insert DAS file name into
!        long error messages.
!
!-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
!
!-&

!
!     The file must be open for write access.
!
      call dassih ( handle, 'WRITE' )

      if ( failed() ) return

!
!     Find out which addresses are already in use.  A file containing
!     data cannot be initialized.
!
      call daslla ( handle, lastc, lastd, lasti )

      if (      ( lastc > 0 ) &
           .or. ( lastd > 0 ) &
           .or. ( lasti > 0 ) ) then

         call setmsg ( 'File # contains data; LASTC = #; LASTD = #; '// &
                       'LASTI = #.'                                  )
         call errhan ( '#',  handle                                  )
         call errint ( '#',  lastc                                   )
         call errint ( '#',  lastd                                   )
         call errint ( '#',  lasti                                   )
         call sigerr ( 'SPICE(DASNOTEMPTY)'                          )
         return

      end if

!
!     Initialize our fill buffers.
!
      call fillc ( ' ',   1,       cfill )
      call filld ( 0.0_wp,  pgsizd,  dfill )
      call filli ( 0,     pgsizi,  ifill )

!
!     Initialize enough integer addresses to hold the metadata area.
!
      call dasadi ( handle, pgbasi, ifill )

!
!     Set the architecture code.
!
      call dasudi ( handle, eparch, eparch, archid )

!
!     Set the page sizes and base addresses.
!
      call dasudi ( handle, eppszc, eppszc, pgsizc )
      call dasudi ( handle, eppszd, eppszd, pgsizd )
      call dasudi ( handle, eppszi, eppszi, pgsizi )

      call dasudi ( handle, epbasc, epbasc, pgbasc )
      call dasudi ( handle, epbasd, epbasd, pgbasd )
      call dasudi ( handle, epbasi, epbasi, pgbasi )

!
!     Since the integer fill value is zero, and since zero is
!     interpreted as null pointer, all pointers are initialized.
!
      return

!$Procedure  ZZEKPGAN ( Private: EK, allocate new page )

      entry zzekpgan ( handle, type, p, base )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Allocate a new page of a specified data type.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     PRIVATE
!
! $ Declarations
!
!     INTEGER               HANDLE
!     INTEGER               TYPE
!     INTEGER               P
!     INTEGER               BASE
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Paged EK file handle.
!     TYPE       I   Data type of page to allocate.
!     P          O   Page number.
!     BASE       O   DAS base address of page.
!
! $ Detailed_Input
!
!     HANDLE         is a handle of a paged EK file.  The file must
!                    be open for write access.
!
!     TYPE           is the data type of the page to allocate.  The
!                    type may be CHR, DP, or INT.  Values of these
!                    parameters are defined in ektype.inc.
!
! $ Detailed_Output
!
!     P              is the number of an allocated page.  The returned
!                    page is never taken from the free list; it is
!                    the lowest-addressed page of the specified type
!                    that has never been allocated.
!
!     BASE           is the base DAS address of the page.  This address
!                    is the predecessor of the first DAS word of the
!                    page.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the DAS file designated by HANDLE is not open for paged
!         write access, the error will be diagnosed by routines called
!         by this routine.
!
!     2)  Any read or write errors detected during reading or writing
!         the DAS file will be diagnosed by routines called by this
!         routine.
!
!     3)  If the requested data type is not recognized, the error
!         SPICE(INVALIDTYPE) is signaled.
!
! $ Files
!
!     This suite of routines provides paged access to DAS files.  Only
!     DAS files initialized via a call to ZZEKPGIN may be written or
!     read by these routines.
!
! $ Particulars
!
!     The pages returned by this routine lie on DAS record boundaries.
!     Successive requests for pages of the same data type will return
!     pages that are adjacent in the DAS address space of that type.
!     In fact, the main reason to call this routine rather than
!     ZZEKPGAL is to allocate adjacent pages.
!
!     Use ZZEKPGAL for normal allocation.
!
! $ Examples
!
!     See EKAPS.
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
!
! $ Version
!
!-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
!
!-&

!
!     Validate the file.
!
      call zzekpgch ( handle, 'WRITE' )

      if ( failed() ) return

      if ( type == chr ) then
!
!        The new page follows the last character address.
!
         call dasadc ( handle, pgsizc, 1, pgsizc, cfill )

!
!        Update the character page count.
!
         call dasrdi ( handle, epnpc, epnpc, npc   )
         call dasudi ( handle, epnpc, epnpc, npc+1 )

!
!        Set the page number and base address.
!
         p     =  npc + 1
         base  =  npc * pgsizc

      else if ( type == dp ) then

         call dasadd ( handle, pgsizd, dfill )

         call dasrdi ( handle, epnpd, epnpd, npd   )
         call dasudi ( handle, epnpd, epnpd, npd+1 )

         p     =  npd + 1
         base  =  npd * pgsizd

      else if ( type == int ) then

         call dasadi ( handle, pgsizi, ifill )

         call dasrdi ( handle, epnpi, epnpi, npi   )
         call dasudi ( handle, epnpi, epnpi, npi+1 )

         p     =  npi + 1
         base  =  npi * pgsizi  +  pgbasi

      else

         call setmsg ( 'The data type code # was not recognized.' )
         call errint ( '#', type                                  )
         call sigerr ( 'SPICE(INVALIDTYPE)'                       )
         return

      end if

      return

!$Procedure  ZZEKPGAL ( Private: EK, allocate page )

      entry zzekpgal ( handle, type, p, base )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Allocate a page of a specified data type.  The page need not
!     be new:  free pages are returned if possible.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     PRIVATE
!
! $ Declarations
!
!     INTEGER               HANDLE
!     INTEGER               TYPE
!     INTEGER               P
!     INTEGER               BASE
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Paged EK file handle.
!     TYPE       I   Data type of page to allocate.
!     P          O   Page number.
!     BASE       O   DAS base address of page.
!
! $ Detailed_Input
!
!     HANDLE         is a handle of a paged EK file.  The file must
!                    be open for write access.
!
!     TYPE           is the data type of the page to allocate.  The
!                    type may be CHR, DP, or INT.  Values of these
!                    parameters are defined in ektype.inc.
!
! $ Detailed_Output
!
!     P              is the number of an allocated page.  The returned
!                    page is taken from the free list if the free list
!                    is non-empty; otherwise, a new page is returned.
!
!     BASE           is the base DAS address of the page.  This address
!                    is the predecessor of the first DAS word of the
!                    page.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the DAS file designated by HANDLE is not open for paged
!         write access, the error will be diagnosed by routines called
!         by this routine.
!
!     2)  Any read or write errors detected during reading or writing
!         the DAS file will be diagnosed by routines called by this
!         routine.
!
!     3)  If the requested data type is not recognized, the error
!         SPICE(INVALIDTYPE) is signaled.
!
! $ Files
!
!     This suite of routines provides paged access to DAS files.  Only
!     DAS files initialized via a call to ZZEKPGIN may be written or
!     read by these routines.
!
! $ Particulars
!
!     This routine should be used for page allocation, except for
!     applications requiring allocation of contiguous pages.  If
!     contiguous pages are required, use ZZEKPGAN.
!
! $ Examples
!
!     See EKAPS.
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
!
! $ Version
!
!-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
!
!-&

!
!     Validate the file.
!
      call zzekpgch ( handle, 'WRITE' )

      if ( failed() ) return

      if ( type == chr ) then
!
!        If the character free list is non-empty, take a page from
!        that list.
!
         call dasrdi ( handle, epfpc, epfpc, freec )

         if ( freec > 0 ) then
!
!           We'll return the first free page.
!
            p       =     freec
!
!           The new head of the list is the successor of FREEC, if
!           any.  Obtain the forward pointer from the page.
!
            addr  =   ( freec - 1 ) * pgsizc  + 1

            call dasrdc ( handle,  addr,  addr+4,  1,  5,  encpag )
            call prtdec ( encpag,  forwrd  )

            freec =   forwrd

!
!           Decrement the free page count, and write the free pointer
!           back to the file.
!
            call dasrdi ( handle, epnfpc, epnfpc, nfreec   )
            call dasudi ( handle, epnfpc, epnfpc, nfreec-1 )
            call dasudi ( handle, epfpc,  epfpc,  freec    )

!
!           Set base address.
!
            base  =  (p-1) * pgsizc

         else
!
!           The new page follows the last character address.
!
            call dasadc ( handle, pgsizc, 1, pgsizc, cfill )

!
!           Update the character page count.
!
            call dasrdi ( handle, epnpc, epnpc, npc   )
            call dasudi ( handle, epnpc, epnpc, npc+1 )

!
!           Set the page number and base address.
!
            p     =  npc + 1
            base  =  npc * pgsizc

         end if

      else if ( type == dp ) then
!
!        If the d.p. free list is non-empty, take a page from
!        that list.
!
         call dasrdi ( handle, epfpd, epfpd, freed )

         if ( freed > 0 ) then
!
!           We'll return the first free page.
!
            p       =     freed
!
!           The new head of the list is the successor of FREED, if
!           any.  Obtain the forward pointer from the page.
!
            addr  =   ( freed - 1 ) * pgsizd   +  1

            call dasrdd ( handle, addr, addr, dpptr )
            freed =   nint(dpptr)

!
!           Decrement the free page count, and write the free pointer
!           back to the file.
!
            call dasrdi ( handle, epnfpd, epnfpd, nfreed   )
            call dasudi ( handle, epnfpd, epnfpd, nfreed-1 )
            call dasudi ( handle, epfpd,  epfpd,  freed    )

!
!           Set base address.
!
            base  =  (p-1) * pgsizd

         else
!
!           The new page follows the last d.p. address.
!
            call dasadd ( handle, pgsizd, dfill )

!
!           Update the d.p. page count.
!
            call dasrdi ( handle, epnpd, epnpd, npd   )
            call dasudi ( handle, epnpd, epnpd, npd+1 )

!
!           Set the page number and base address.
!
            p     =  npd + 1
            base  =  npd * pgsizd

         end if

      else if ( type == int ) then
!
!        If the integer free list is non-empty, take a page from
!        that list.
!
         call dasrdi ( handle, epfpi, epfpi, freei )

         if ( freei > 0 ) then
!
!           We'll return the first free page.
!
            p     =     freei
!
!           The new head of the list is the successor of FREEI, if
!           any.  Obtain the forward pointer from the page.
!
            addr  =   ( freei - 1 ) * pgsizi   +   pgbasi  +  1

            call dasrdi ( handle, addr, addr, freei )

!
!           Decrement the free page count, and write the free pointer
!           back to the file.
!
            call dasrdi ( handle, epnfpi, epnfpi, nfreei   )
            call dasudi ( handle, epnfpi, epnfpi, nfreei-1 )
            call dasudi ( handle, epfpi,  epfpi,  freei    )

!
!           Set base address.
!
            base  =  (p-1) * pgsizi  +  pgbasi

         else

            call dasadi ( handle, pgsizi, ifill )

            call dasrdi ( handle, epnpi, epnpi, npi   )
            call dasudi ( handle, epnpi, epnpi, npi+1 )

            p     =  npi + 1
            base  =  npi * pgsizi  +  pgbasi

         end if

      else

         call setmsg ( 'The data type code # was not recognized.' )
         call errint ( '#', type                                  )
         call sigerr ( 'SPICE(INVALIDTYPE)'                       )
         return

      end if

      return

!$Procedure  ZZEKPGFR ( Private: EK, free page )

      entry zzekpgfr ( handle, type, p )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Free a specified page.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     PRIVATE
!
! $ Declarations
!
!     INTEGER               HANDLE
!     INTEGER               TYPE
!     INTEGER               P
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Paged EK file handle.
!     TYPE       I   Data type of page to allocate.
!     P          I   Page number.
!
! $ Detailed_Input
!
!     HANDLE         is a handle of a paged EK file.  The file must
!                    be open for write access.
!
!     TYPE           is the data type of the page to allocate.  The
!                    type may be CHR, DP, or INT.  Values of these
!                    parameters are defined in ektype.inc.
!
!     P              is the number of an allocated page to be freed.
!
! $ Detailed_Output
!
!    None.  See $Particulars for a description of the effect of this
!    routine.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the DAS file designated by HANDLE is not open for paged
!         write access, the error will be diagnosed by routines called
!         by this routine.
!
!     2)  Any read or write errors detected during reading or writing
!         the DAS file will be diagnosed by routines called by this
!         routine.
!
!     3)  If the requested data type is not recognized, the error
!         SPICE(INVALIDTYPE) is signaled.
!
!     4)  If the number of the page to be freed is not that of an
!         allocated page of the specified type, the error
!         SPICE(INVALIDINDEX) is signaled.
!
! $ Files
!
!     This suite of routines provides paged access to DAS files.  Only
!     DAS files initialized via a call to ZZEKPGIN may be written or
!     read by these routines.
!
! $ Particulars
!
!     This routine should be used for page deallocation.  The input
!     page is placed at the head of the free list of the specified
!     data type.
!
! $ Examples
!
!     See EKDPS.
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
!
! $ Version
!
!-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
!
!-&

!
!     Check the file.
!
      call zzekpgch ( handle, 'WRITE' )

      if ( failed() ) return

      if ( type == chr ) then
!
!        Validate the page number.  Find out how many pages are
!        out there.
!
         call dasrdi ( handle, epnpc, epnpc, npc )

         if (  ( p < 1 ) .or. ( p > npc )  ) then

            call setmsg ( 'Attempt to free non-existent CHR page. ' // &
                          'Page number = #; valid range is 1:#'     )
            call errint ( '#',  p                                   )
            call errint ( '#',  npc                                 )
            call sigerr ( 'SPICE(INVALIDINDEX)'                     )
            return

         end if

!
!        Get the current character free pointer and free page count.
!
         call dasrdi ( handle, epfpc,  epfpc,  freec  )
         call dasrdi ( handle, epnfpc, epnfpc, nfreec )

!
!        Insert into the freed page a pointer to the head of the
!        free list.
!
         call prtenc ( freec, encpag )
         addr  =  (p-1)*pgsizc + 1

         call dasudc ( handle, addr, addr+4, 1, 5, encpag )

!
!        Update the current character free pointer and free page count.
!
         call dasudi ( handle, epfpc,  epfpc,  p        )
         call dasudi ( handle, epnfpc, epnfpc, nfreec+1 )

      else if ( type == dp ) then
!
!        Validate the page number.  Find out how many pages are
!        out there.
!
         call dasrdi ( handle, epnpd, epnpd, npd )

         if (  ( p < 1 ) .or. ( p > npd )  ) then

            call setmsg ( 'Attempt to free non-existent DP page. ' // &
                          'Page number = #; valid range is 1:#'     )
            call errint ( '#',  p                                   )
            call errint ( '#',  npd                                 )
            call sigerr ( 'SPICE(INVALIDINDEX)'                     )
            return

         end if

!
!        Get the current d.p. free pointer and free page count.
!
         call dasrdi ( handle, epfpd,  epfpd,  freed  )
         call dasrdi ( handle, epnfpd, epnfpd, nfreed )

!
!        Insert into the freed page a pointer to the head of the
!        free list.
!
         addr  =  (p-1)*pgsizd + 1

         call dasudd ( handle, addr, addr, real(freed, wp) )

!
!        Update the current d.p. free pointer and free page count.
!
         call dasudi ( handle, epfpd,  epfpd,  p        )
         call dasudi ( handle, epnfpd, epnfpd, nfreed+1 )

      else if ( type == int ) then
!
!        Validate the page number.  Find out how many pages are
!        out there.
!
         call dasrdi ( handle, epnpi, epnpi, npi )

         if (  ( p < 1 ) .or. ( p > npi )  ) then

            call setmsg ( 'Attempt to free non-existent INT page. ' // &
                          'Page number = #; valid range is 1:#'     )
            call errint ( '#',  p                                   )
            call errint ( '#',  npi                                 )
            call sigerr ( 'SPICE(INVALIDINDEX)'                     )
            return

         end if

!
!        Get the current integer free pointer and free page count.
!
         call dasrdi ( handle, epfpi,  epfpi,  freei  )
         call dasrdi ( handle, epnfpi, epnfpi, nfreei )

!
!        Insert into the freed page a pointer to the head of the
!        free list.
!
         addr  =  (p-1)*pgsizi  +  pgbasi  +  1

         call dasudi ( handle, addr, addr, freei )

!
!        Update the current integer free pointer and free page count.
!
         call dasudi ( handle, epfpi,  epfpi,  p        )
         call dasudi ( handle, epnfpi, epnfpi, nfreei+1 )

      else

         call setmsg ( 'The data type code # was not recognized.' )
         call errint ( '#', type                                  )
         call sigerr ( 'SPICE(INVALIDTYPE)'                       )
         return

      end if

      return

!$Procedure  ZZEKPGRC ( Private: EK, read character page )

      entry zzekpgrc ( handle, p, pagec )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Read a specified character page.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     PRIVATE
!
! $ Declarations
!
!     INTEGER               HANDLE
!     INTEGER               P
!     CHARACTER*(*)         PAGEC
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Paged EK file handle.
!     P          I   Page number.
!     PAGEC      O   Character page.
!
! $ Detailed_Input
!
!     HANDLE         is a handle of a paged EK file.  The file may
!                    be open for read or write access.
!
!     P              is the number of a character page to read.
!
! $ Detailed_Output
!
!     PAGEC          is a string containing the contents of the
!                    specified page.  PAGEC should be declared with
!                    length at PGSIZC characters.  This parameter is
!                    declared in the include file ekpage.inc.
!
!                    If PAGEC has length less than PGSIZC characters,
!                    the output will be truncated on the right.  If
!                    PAGEC is longer than PGSIZC characters, the output
!                    will be padded with trailing blanks.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  Any errors detected during reading the DAS file will be
!         diagnosed by routines called by this routine.
!
!     2)  If the number of the page to read is not that of an
!         allocated character page, the error SPICE(INVALIDINDEX) is
!         signaled.
!
! $ Files
!
!     This suite of routines provides paged access to DAS files.  Only
!     DAS files initialized via a call to ZZEKPGIN may be written or
!     read by these routines.
!
! $ Particulars
!
!     This routine should be used to read character pages.
!
! $ Examples
!
!     See ZZEKRD03.
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
!
! $ Version
!
!-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
!
!-&

!
!     Use discovery check-in.
!
!     Find out how many character pages are in use.
!
      call dasrdi ( handle, epnpc, epnpc, npc )

      if (  ( p < 1 )  .or.  ( p > npc )  ) then

         call setmsg ( 'CHR page = #; valid range is [1:#]' )
         call errint ( '#', p                               )
         call errint ( '#', npc                             )
         call sigerr ( 'SPICE(INVALIDINDEX)'                )
         return

      end if

      l      =   len( pagec )
      e      =   min( l, pgsizc )

      addr   =   (p-1)*pgsizc + 1

      call dasrdc ( handle, addr, addr+pgsizc-1, 1, e, pagec )

      if ( l > e ) then
         pagec ( e+1: ) = ' '
      end if

      return

!$Procedure  ZZEKPGRD ( Private: EK, read d.p. page )

      entry zzekpgrd ( handle, p, paged )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Read a specified double precision page.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     PRIVATE
!
! $ Declarations
!
!     INTEGER               HANDLE
!     INTEGER               P
!     DOUBLE PRECISION      PAGED ( * )
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Paged EK file handle.
!     P          I   Page number.
!     PAGED      O   Double precision page.
!
! $ Detailed_Input
!
!     HANDLE         is a handle of a paged EK file.  The file may
!                    be open for read or write access.
!
!     P              is the number of a double precision page to read.
!
! $ Detailed_Output
!
!     PAGED          is a double precision array containing the contents
!                    of the specified page.  PAGED should be declared
!                    with dimension PGSIZD.  This parameter is
!                    declared in the include file ekpage.inc.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  Any errors detected during reading the DAS file will be
!         diagnosed by routines called by this routine.
!
!     2)  If the number of the page to read is not that of an
!         allocated double precision page, the error SPICE(INVALIDINDEX)
!         is signaled.
!
! $ Files
!
!     This suite of routines provides paged access to DAS files.  Only
!     DAS files initialized via a call to ZZEKPGIN may be written or
!     read by these routines.
!
! $ Particulars
!
!     This routine should be used to read double precision pages.
!
! $ Examples
!
!     See ZZEKRD02.
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
!
! $ Version
!
!-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
!
!-&

!
!     Use discovery check-in.
!
!     Find out how many d.p. pages are in use.
!
      call dasrdi ( handle, epnpd, epnpd, npd )

      if (  ( p < 1 )  .or.  ( p > npd )  ) then

         call setmsg ( 'DP page = #; valid range is [1:#]'  )
         call errint ( '#', p                               )
         call errint ( '#', npd                             )
         call sigerr ( 'SPICE(INVALIDINDEX)'                )
         return

      end if

      addr   =   (p-1)*pgsizd + 1

      call dasrdd ( handle, addr, addr+pgsizd-1, paged )

      return

!$Procedure  ZZEKPGRI ( Private: EK, read integer page )

      entry zzekpgri ( handle, p, pagei )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Read a specified integer page.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     PRIVATE
!
! $ Declarations
!
!     INTEGER               HANDLE
!     INTEGER               P
!     INTEGER               PAGEI ( * )
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Paged EK file handle.
!     P          I   Page number.
!     PAGEI      O   Integer page.
!
! $ Detailed_Input
!
!     HANDLE         is a handle of a paged EK file.  The file may
!                    be open for read or write access.
!
!     P              is the number of an integer page to read.
!
! $ Detailed_Output
!
!     PAGEI          is an integer array containing the contents
!                    of the specified page.  PAGEI should be declared
!                    with dimension PGSIZI.  This parameter is
!                    declared in the include file ekpage.inc.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  Any errors detected during reading the DAS file will be
!         diagnosed by routines called by this routine.
!
!     2)  If the number of the page to read is not that of an
!         allocated double precision page, the error SPICE(INVALIDINDEX)
!         is signaled.
!
! $ Files
!
!     This suite of routines provides paged access to DAS files.  Only
!     DAS files initialized via a call to ZZEKPGIN may be written or
!     read by these routines.
!
! $ Particulars
!
!     This routine should be used to read integer pages.
!
! $ Examples
!
!     See ZZEKRD01.
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
!
! $ Version
!
!-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
!
!-&

!
!     Use discovery check-in.
!
!     Find out how many integer pages are in use.
!
      call dasrdi ( handle, epnpi, epnpi, npi )

      if (  ( p < 1 )  .or.  ( p > npi )  ) then

         call setmsg ( 'INT page = #; valid range is [1:#]' )
         call errint ( '#', p                               )
         call errint ( '#', npi                             )
         call sigerr ( 'SPICE(INVALIDINDEX)'                )
         return

      end if

      addr   =   pgbasi  +  (p-1)*pgsizi + 1

      call dasrdi ( handle, addr, addr+pgsizi-1, pagei )

      return

!$Procedure  ZZEKPGWC ( Private: EK, write character page )

      entry zzekpgwc ( handle, p, pagec )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Write a specified character page.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     PRIVATE
!
! $ Declarations
!
!     INTEGER               HANDLE
!     INTEGER               P
!     CHARACTER*(*)         PAGEC
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Paged EK file handle.
!     P          I   Page number.
!     PAGEC      I   Character page.
!
! $ Detailed_Input
!
!     HANDLE         is a handle of a paged EK file.  The file must
!                    be open for write access.
!
!     P              is the number of an allocated character page to
!                    write.
!
!     PAGEC          is a string to be written to the specified page.
!                    PAGEC must be declared with length at PGSIZC
!                    characters.  This parameter is declared in the
!                    include file ekpage.inc.
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
!     1)  Any errors detected during reading or writing the DAS file
!         will be diagnosed by routines called by this routine.
!
!     2)  If the number of the page to write is not that of an
!         allocated character page, the error SPICE(INVALIDINDEX) is
!         signaled.
!
!     3)  If the input string has length less than PGSIZC characters,
!         the error SPICE(STRINGTOOSHORT) is signaled.
!
! $ Files
!
!     This suite of routines provides paged access to DAS files.  Only
!     DAS files initialized via a call to ZZEKPGIN may be written or
!     read by these routines.
!
! $ Particulars
!
!     This routine writes the input string to the DAS address range
!     corresponding to the specified page.  The file must be closed
!     properly (via EKCLS) in order to make the change permanent.
!
! $ Examples
!
!     See ZZEKAD03.
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
!
! $ Version
!
!-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
!
!-&

!
!     Use discovery check-in.
!
!     Validate the file.
!
      call zzekpgch ( handle, 'WRITE' )

      if ( failed() ) return

!
!     Find out how many character pages are in use.
!
      call dasrdi ( handle, epnpc, epnpc, npc )

      if (  ( p < 1 )  .or.  ( p > npc )  ) then

         call setmsg ( 'CHR page = #; valid range is [1:#]' )
         call errint ( '#', p                               )
         call errint ( '#', npc                             )
         call sigerr ( 'SPICE(INVALIDINDEX)'                )
         return

      end if

      l  =  len( pagec )

      if ( l < pgsizc ) then

         call setmsg ( 'Input CHR page size = #; valid size is [#:]' )
         call errint ( '#', l                                        )
         call errint ( '#', pgsizc                                   )
         call sigerr ( 'SPICE(STRINGTOOSHORT)'                       )
         return

      end if

      addr   =   (p-1)*pgsizc + 1

      call dasudc ( handle, addr, addr+pgsizc-1, 1, pgsizc, pagec )

      return

!$Procedure  ZZEKPGWD ( Private: EK, write d.p. page )

      entry zzekpgwd ( handle, p, paged )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Write a specified double precision page.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     PRIVATE
!
! $ Declarations
!
!     INTEGER               HANDLE
!     INTEGER               P
!     DOUBLE PRECISION      PAGED ( * )
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Paged EK file handle.
!     P          I   Page number.
!     PAGED      I   Double precision page.
!
! $ Detailed_Input
!
!     HANDLE         is a handle of a paged EK file.  The file must
!                    be open for write access.
!
!     P              is the number of an allocated double precision
!                    page to write.
!
!     PAGED          is a double precision array to be written to
!                    the specified page.  PAGED must be declared with
!                    dimension at PGSIZD.  This parameter is
!                    declared in the include file ekpage.inc.
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
!     1)  Any errors detected during reading or writing the DAS file
!         will be diagnosed by routines called by this routine.
!
!     2)  If the number of the page to write is not that of an
!         allocated d.p. page, the error SPICE(INVALIDINDEX) is
!         signaled.
!
! $ Files
!
!     This suite of routines provides paged access to DAS files.  Only
!     DAS files initialized via a call to ZZEKPGIN may be written or
!     read by these routines.
!
! $ Particulars
!
!     This routine writes the input array to the DAS address range
!     corresponding to the specified page.  The file must be closed
!     properly (via EKCLS) in order to make the change permanent.
!
! $ Examples
!
!     See ZZEKAD02.
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
!
! $ Version
!
!-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
!
!-&

!
!     Use discovery check-in.
!
!     Validate the file.
!
      call zzekpgch ( handle, 'WRITE' )

      if ( failed() ) return

!
!     Find out how many d.p. pages are in use.
!
      call dasrdi ( handle, epnpd, epnpd, npd )

      if (  ( p < 1 )  .or.  ( p > npd )  ) then

         call setmsg ( 'DP page = #; valid range is [1:#]'  )
         call errint ( '#', p                               )
         call errint ( '#', npd                             )
         call sigerr ( 'SPICE(INVALIDINDEX)'                )
         return

      end if

      addr   =  (p-1)*pgsizd + 1

      call dasudd ( handle, addr, addr+pgsizd-1, paged )

      return

!$Procedure  ZZEKPGWI ( Private: EK, write integer page )

      entry zzekpgwi ( handle, p, pagei )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Write a specified integer page.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     PRIVATE
!
! $ Declarations
!
!     INTEGER               HANDLE
!     INTEGER               P
!     INTEGER               PAGEI ( * )
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Paged EK file handle.
!     P          I   Page number.
!     PAGEI      I   Integer page.
!
! $ Detailed_Input
!
!     HANDLE         is a handle of a paged EK file.  The file must
!                    be open for write access.
!
!     P              is the number of an allocated integer
!                    page to write.
!
!     PAGEI          is an integer array to be written to
!                    the specified page.  PAGEI must be declared with
!                    dimension at PGSIZI.  This parameter is
!                    declared in the include file ekpage.inc.
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
!     1)  Any errors detected during reading or writing the DAS file
!         will be diagnosed by routines called by this routine.
!
!     2)  If the number of the page to write is not that of an
!         allocated integer page, the error SPICE(INVALIDINDEX) is
!         signaled.
!
! $ Files
!
!     This suite of routines provides paged access to DAS files.  Only
!     DAS files initialized via a call to ZZEKPGIN may be written or
!     read by these routines.
!
! $ Particulars
!
!     This routine writes the input array to the DAS address range
!     corresponding to the specified page.  The file must be closed
!     properly (via EKCLS) in order to make the change permanent.
!
! $ Examples
!
!     See ZZEKAD01.
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
!
! $ Version
!
!-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
!
!-&

!
!     Use discovery check-in.
!
!     Validate the file.
!
      call zzekpgch ( handle, 'WRITE' )

      if ( failed() ) return

!
!     Find out how many integer pages are in use.
!
      call dasrdi ( handle, epnpi, epnpi, npi )

      if (  ( p < 1 )  .or.  ( p > npi )  ) then

         call setmsg ( 'INT page = #; valid range is [1:#]' )
         call errint ( '#', p                               )
         call errint ( '#', npi                             )
         call sigerr ( 'SPICE(INVALIDINDEX)'                )
         return

      end if

      addr   =   pgbasi + (p-1)*pgsizi + 1

      call dasudi ( handle, addr, addr+pgsizi-1, pagei )

      return

!$Procedure  ZZEKPGBS ( Private: EK, map page to base address )

      entry zzekpgbs ( type, p, base )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Map a page to its base address.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     PRIVATE
!
! $ Declarations
!
!     INTEGER               TYPE
!     INTEGER               P
!     INTEGER               BASE
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     TYPE       I   Data type of page.
!     P          I   Page number.
!     BASE       O   DAS base address of page.
!
! $ Detailed_Input
!
!     TYPE           is the data type of the page whose base address
!                    is requested.  The type may be CHR, DP, or INT.
!                    Values of these parameters are defined in
!                    ektype.inc.
!
!     P              is the number of the page of interest.
!
! $ Detailed_Output
!
!     BASE           is the base DAS address of the page.  This address
!                    is the predecessor of the first DAS word of the
!                    page.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the requested data type is not recognized, the error
!         SPICE(INVALIDTYPE) is signaled.
!
!     2)  Range checking is not performed on the input page number P.
!
! $ Files
!
!     This suite of routines provides paged access to DAS files.  Only
!     DAS files initialized via a call to ZZEKPGIN may be written or
!     read by these routines.
!
! $ Particulars
!
!     This routine provides translation from page numbers to DAS
!     addresses.
!
! $ Examples
!
!     See EKDELR.
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
!
! $ Version
!
!-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
!
!-&

      if ( type == chr ) then

         base  =   ( p - 1 ) * pgsizc

      else if ( type == dp ) then

         base  =   ( p - 1 ) * pgsizd

      else if ( type == int ) then

         base  =   ( p - 1 ) * pgsizi   +   pgbasi

      else

         call setmsg ( 'The data type code # was not recognized.' )
         call errint ( '#', type                                  )
         call sigerr ( 'SPICE(INVALIDTYPE)'                       )
         return

      end if

      return

!$Procedure  ZZEKPGPG ( Private: EK, map address to page )

      entry zzekpgpg ( type, addrss, p, base )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Map a DAS address to the number of the page containing it.  Also
!     return the base address of the page.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     PRIVATE
!
! $ Declarations
!
!     INTEGER               TYPE
!     INTEGER               ADDRSS
!     INTEGER               P
!     INTEGER               BASE
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     TYPE       I   Data type of address.
!     ADDRSS     I   DAS address to be mapped.
!     P          O   Page number.
!     BASE       O   DAS base address of page.
!
! $ Detailed_Input
!
!     TYPE           is the data type of a DAS address to be mapped to
!                    a page number.  The type may be CHR, DP, or INT.
!                    Values of these parameters are defined in
!                    ektype.inc.
!
!     ADDRSS         is a DAS address to be mapped to a page number.
!
! $ Detailed_Output
!
!     P              is the number of the page containing the input
!                    address.
!
!     BASE           is the base DAS address of the page.  This address
!                    is the predecessor of the first DAS word of the
!                    page.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the requested data type is not recognized, the error
!         SPICE(INVALIDTYPE) is signaled.
!
!     2)  Range checking is not performed on the input address.
!
! $ Files
!
!     This suite of routines provides paged access to DAS files.  Only
!     DAS files initialized via a call to ZZEKPGIN may be written or
!     read by these routines.
!
! $ Particulars
!
!     This routine provides translation from DAS addresses to page
!     numbers.
!
! $ Examples
!
!     See ZZEKAD01.
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
!     B.V. Semenov       (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 14-OCT-2021 (BVS)
!
!        Bug fix: fixed routine name in CHKIN/CHKOUT calls (ZZEKPGBS
!        -> ZZEKPGPG).
!
!-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
!
!-&

      if ( type == chr ) then

         p     =   ( addrss  + pgsizc - 1 ) / pgsizc
         base  =   ( p - 1 ) * pgsizc

      else if ( type == dp ) then

         p     =   ( addrss  + pgsizd - 1 ) / pgsizd
         base  =   ( p - 1 ) * pgsizd

      else if ( type == int ) then

         p     =   ( ( addrss-pgbasi )  +   pgsizi - 1 ) / pgsizi
         base  =   ( p - 1 ) * pgsizi   +   pgbasi

      else

         call setmsg ( 'The data type code # was not recognized.' )
         call errint ( '#', type                                  )
         call sigerr ( 'SPICE(INVALIDTYPE)'                       )
         return

      end if

      return

!$Procedure  ZZEKPGST ( Private: EK, return paging statistics )

      entry zzekpgst ( handle, stat, value )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Return paging statistics.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     PRIVATE
!
! $ Declarations
!
!     INTEGER               HANDLE
!     CHARACTER*(*)         STAT
!     INTEGER               VALUE
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Paged EK file handle.
!     STAT       I   Name of requested statistic.
!     VALUE      O   Value of requested statistic.
!
! $ Detailed_Input
!
!     HANDLE         is a handle of a paged EK file.  The file may
!                    be open for read or write access.
!
!     STAT           is the name of the requested statistic.  Possible
!                    values and meanings of STAT are:
!
!                      'N_C_ALLOC'     Number of character pages
!                                      allocated.  Pages on the free
!                                      list are not included.
!
!                      'N_D_ALLOC'     Number of d.p. pages allocated.
!
!                      'N_I_ALLOC'     Number of integer pages
!                                      allocated.
!
!                      'N_C_FREE'      Number of pages in character free
!                                      list.
!
!                      'N_D_FREE'      Number of pages in d.p. free
!                                      list.
!
!                      'N_I_FREE'      Number of pages in integer free
!                                      list.
!
! $ Detailed_Output
!
!     VALUE          is the value of the requested statistic.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the requested statistic is not recognized, the error
!         SPICE(INVALIDOPTION) is signaled.
!
! $ Files
!
!     This suite of routines provides paged access to DAS files.  Only
!     DAS files initialized via a call to ZZEKPGIN may be written or
!     read by these routines.
!
! $ Particulars
!
!     This routine provides translation from DAS addresses to page
!     numbers.
!
! $ Examples
!
!     1)  Find the number of pages on the integer free list of the
!         paged EK designated by HANDLE:
!
!            CALL ZZEKPGST ( HANDLE, 'N_I_FREE', NFREE )
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
!
! $ Version
!
!-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
!
!-&

      if (  eqstr( stat, 'N_C_ALLOC' )  ) then

         call dasrdi ( handle, epnpc, epnpc, value   )

      else if (  eqstr( stat, 'N_D_ALLOC' )  ) then

         call dasrdi ( handle, epnpd, epnpd, value   )

      else if (  eqstr( stat, 'N_I_ALLOC' )  ) then

         call dasrdi ( handle, epnpi, epnpi, value   )

      else if (  eqstr( stat, 'N_C_FREE' )  ) then

         call dasrdi ( handle, epnfpc, epnfpc, value   )

      else if (  eqstr( stat, 'N_D_FREE' )  ) then

         call dasrdi ( handle, epnfpd, epnfpd, value   )

      else if (  eqstr( stat, 'N_I_FREE' )  ) then

         call dasrdi ( handle, epnfpi, epnfpi, value   )

      else

         call setmsg ( 'Statistic # is not supported.' )
         call errch  ( '#',  stat                      )
         call sigerr ( 'SPICE(INVALIDOPTION)'          )
         return

      end if

      return

      end
