!$Procedure  ZZEKPAGE ( Private: Manage EK DAS paging system )

      SUBROUTINE ZZEKPAGE (  HANDLE,  TYPE,   ADDRSS,  STAT,  P, &
                             PAGEC,   PAGED,  PAGEI,   BASE,  VALUE  )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Manage EK DAS paging system.
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
!     PRIVATE
!
! $ Declarations

      INCLUDE 'ekarch.inc'
      INCLUDE 'ekpage.inc'
      INCLUDE 'ektype.inc'

      INTEGER               HANDLE
      INTEGER               TYPE
      INTEGER               ADDRSS
      CHARACTER*(*)         STAT
      INTEGER               P
      CHARACTER*(*)         PAGEC
      DOUBLE PRECISION      PAGED ( * )
      INTEGER               PAGEI ( * )
      INTEGER               BASE
      INTEGER               VALUE

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
      LOGICAL               EQSTR
      LOGICAL               FAILED

!
!     Local variables
!
!
!     Note:  the integer fill buffer should be as large as the maximum
!     of the integer page size and the metadata area size.
!
      CHARACTER*(PGSIZC)    CFILL
      CHARACTER*(5)         ENCPAG

      DOUBLE PRECISION      DFILL  ( PGSIZD )
      DOUBLE PRECISION      DPPTR

      INTEGER               ADDR
      INTEGER               E
      INTEGER               FORWRD
      INTEGER               FREEC
      INTEGER               FREED
      INTEGER               FREEI
      INTEGER               IFILL  ( PGSIZI )
      INTEGER               L
      INTEGER               LASTC
      INTEGER               LASTD
      INTEGER               LASTI
      INTEGER               NFREEC
      INTEGER               NFREED
      INTEGER               NFREEI
      INTEGER               NPC
      INTEGER               NPD
      INTEGER               NPI

!
!     Saved variables
!
      SAVE

      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
      RETURN






!$Procedure  ZZEKPGIN ( Private: Initialize DAS for paged access )

      ENTRY ZZEKPGIN ( HANDLE )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Initialize an open DAS file for paged access.
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

      CALL CHKIN ( 'ZZEKPGIN' )

!
!     The file must be open for write access.
!
      CALL DASSIH ( HANDLE, 'WRITE' )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZEKPGIN' )
         RETURN
      END IF

!
!     Find out which addresses are already in use.  A file containing
!     data cannot be initialized.
!
      CALL DASLLA ( HANDLE, LASTC, LASTD, LASTI )

      IF (      ( LASTC .GT. 0 ) &
           .OR. ( LASTD .GT. 0 ) &
           .OR. ( LASTI .GT. 0 ) ) THEN

         CALL SETMSG ( 'File # contains data; LASTC = #; LASTD = #; '// &
                       'LASTI = #.'                                  )
         CALL ERRHAN ( '#',  HANDLE                                  )
         CALL ERRINT ( '#',  LASTC                                   )
         CALL ERRINT ( '#',  LASTD                                   )
         CALL ERRINT ( '#',  LASTI                                   )
         CALL SIGERR ( 'SPICE(DASNOTEMPTY)'                          )
         CALL CHKOUT ( 'ZZEKPGIN'                                    )
         RETURN

      END IF

!
!     Initialize our fill buffers.
!
      CALL FILLC ( ' ',   1,       CFILL )
      CALL FILLD ( 0.D0,  PGSIZD,  DFILL )
      CALL FILLI ( 0,     PGSIZI,  IFILL )

!
!     Initialize enough integer addresses to hold the metadata area.
!
      CALL DASADI ( HANDLE, PGBASI, IFILL )

!
!     Set the architecture code.
!
      CALL DASUDI ( HANDLE, EPARCH, EPARCH, ARCHID )

!
!     Set the page sizes and base addresses.
!
      CALL DASUDI ( HANDLE, EPPSZC, EPPSZC, PGSIZC )
      CALL DASUDI ( HANDLE, EPPSZD, EPPSZD, PGSIZD )
      CALL DASUDI ( HANDLE, EPPSZI, EPPSZI, PGSIZI )

      CALL DASUDI ( HANDLE, EPBASC, EPBASC, PGBASC )
      CALL DASUDI ( HANDLE, EPBASD, EPBASD, PGBASD )
      CALL DASUDI ( HANDLE, EPBASI, EPBASI, PGBASI )

!
!     Since the integer fill value is zero, and since zero is
!     interpreted as null pointer, all pointers are initialized.
!
      CALL CHKOUT ( 'ZZEKPGIN' )
      RETURN





!$Procedure  ZZEKPGAN ( Private: EK, allocate new page )

      ENTRY ZZEKPGAN ( HANDLE, TYPE, P, BASE )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Allocate a new page of a specified data type.
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

      CALL CHKIN ( 'ZZEKPGAN' )

!
!     Validate the file.
!
      CALL ZZEKPGCH ( HANDLE, 'WRITE' )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZEKPGAN' )
         RETURN
      END IF


      IF ( TYPE .EQ. CHR ) THEN
!
!        The new page follows the last character address.
!
         CALL DASADC ( HANDLE, PGSIZC, 1, PGSIZC, CFILL )

!
!        Update the character page count.
!
         CALL DASRDI ( HANDLE, EPNPC, EPNPC, NPC   )
         CALL DASUDI ( HANDLE, EPNPC, EPNPC, NPC+1 )

!
!        Set the page number and base address.
!
         P     =  NPC + 1
         BASE  =  NPC * PGSIZC


      ELSE IF ( TYPE .EQ. DP ) THEN

         CALL DASADD ( HANDLE, PGSIZD, DFILL )

         CALL DASRDI ( HANDLE, EPNPD, EPNPD, NPD   )
         CALL DASUDI ( HANDLE, EPNPD, EPNPD, NPD+1 )

         P     =  NPD + 1
         BASE  =  NPD * PGSIZD


      ELSE IF ( TYPE .EQ. INT ) THEN

         CALL DASADI ( HANDLE, PGSIZI, IFILL )

         CALL DASRDI ( HANDLE, EPNPI, EPNPI, NPI   )
         CALL DASUDI ( HANDLE, EPNPI, EPNPI, NPI+1 )

         P     =  NPI + 1
         BASE  =  NPI * PGSIZI  +  PGBASI


      ELSE

         CALL SETMSG ( 'The data type code # was not recognized.' )
         CALL ERRINT ( '#', TYPE                                  )
         CALL SIGERR ( 'SPICE(INVALIDTYPE)'                       )
         CALL CHKOUT ( 'ZZEKPGAN'                                 )
         RETURN

      END IF

      CALL CHKOUT ( 'ZZEKPGAN' )
      RETURN




!$Procedure  ZZEKPGAL ( Private: EK, allocate page )

      ENTRY ZZEKPGAL ( HANDLE, TYPE, P, BASE )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Allocate a page of a specified data type.  The page need not
!     be new:  free pages are returned if possible.
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
      CALL CHKIN ( 'ZZEKPGAL' )

!
!     Validate the file.
!
      CALL ZZEKPGCH ( HANDLE, 'WRITE' )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZEKPGAL' )
         RETURN
      END IF


      IF ( TYPE .EQ. CHR ) THEN
!
!        If the character free list is non-empty, take a page from
!        that list.
!
         CALL DASRDI ( HANDLE, EPFPC, EPFPC, FREEC )


         IF ( FREEC .GT. 0 ) THEN
!
!           We'll return the first free page.
!
            P       =     FREEC
!
!           The new head of the list is the successor of FREEC, if
!           any.  Obtain the forward pointer from the page.
!
            ADDR  =   ( FREEC - 1 ) * PGSIZC  + 1

            CALL DASRDC ( HANDLE,  ADDR,  ADDR+4,  1,  5,  ENCPAG )
            CALL PRTDEC ( ENCPAG,  FORWRD  )

            FREEC =   FORWRD

!
!           Decrement the free page count, and write the free pointer
!           back to the file.
!
            CALL DASRDI ( HANDLE, EPNFPC, EPNFPC, NFREEC   )
            CALL DASUDI ( HANDLE, EPNFPC, EPNFPC, NFREEC-1 )
            CALL DASUDI ( HANDLE, EPFPC,  EPFPC,  FREEC    )

!
!           Set base address.
!
            BASE  =  (P-1) * PGSIZC


         ELSE
!
!           The new page follows the last character address.
!
            CALL DASADC ( HANDLE, PGSIZC, 1, PGSIZC, CFILL )

!
!           Update the character page count.
!
            CALL DASRDI ( HANDLE, EPNPC, EPNPC, NPC   )
            CALL DASUDI ( HANDLE, EPNPC, EPNPC, NPC+1 )

!
!           Set the page number and base address.
!
            P     =  NPC + 1
            BASE  =  NPC * PGSIZC

         END IF



      ELSE IF ( TYPE .EQ. DP ) THEN
!
!        If the d.p. free list is non-empty, take a page from
!        that list.
!
         CALL DASRDI ( HANDLE, EPFPD, EPFPD, FREED )


         IF ( FREED .GT. 0 ) THEN
!
!           We'll return the first free page.
!
            P       =     FREED
!
!           The new head of the list is the successor of FREED, if
!           any.  Obtain the forward pointer from the page.
!
            ADDR  =   ( FREED - 1 ) * PGSIZD   +  1

            CALL DASRDD ( HANDLE, ADDR, ADDR, DPPTR )
            FREED =   NINT(DPPTR)

!
!           Decrement the free page count, and write the free pointer
!           back to the file.
!
            CALL DASRDI ( HANDLE, EPNFPD, EPNFPD, NFREED   )
            CALL DASUDI ( HANDLE, EPNFPD, EPNFPD, NFREED-1 )
            CALL DASUDI ( HANDLE, EPFPD,  EPFPD,  FREED    )

!
!           Set base address.
!
            BASE  =  (P-1) * PGSIZD


         ELSE
!
!           The new page follows the last d.p. address.
!
            CALL DASADD ( HANDLE, PGSIZD, DFILL )

!
!           Update the d.p. page count.
!
            CALL DASRDI ( HANDLE, EPNPD, EPNPD, NPD   )
            CALL DASUDI ( HANDLE, EPNPD, EPNPD, NPD+1 )

!
!           Set the page number and base address.
!
            P     =  NPD + 1
            BASE  =  NPD * PGSIZD

         END IF


      ELSE IF ( TYPE .EQ. INT ) THEN
!
!        If the integer free list is non-empty, take a page from
!        that list.
!
         CALL DASRDI ( HANDLE, EPFPI, EPFPI, FREEI )


         IF ( FREEI .GT. 0 ) THEN
!
!           We'll return the first free page.
!
            P     =     FREEI
!
!           The new head of the list is the successor of FREEI, if
!           any.  Obtain the forward pointer from the page.
!
            ADDR  =   ( FREEI - 1 ) * PGSIZI   +   PGBASI  +  1

            CALL DASRDI ( HANDLE, ADDR, ADDR, FREEI )

!
!           Decrement the free page count, and write the free pointer
!           back to the file.
!
            CALL DASRDI ( HANDLE, EPNFPI, EPNFPI, NFREEI   )
            CALL DASUDI ( HANDLE, EPNFPI, EPNFPI, NFREEI-1 )
            CALL DASUDI ( HANDLE, EPFPI,  EPFPI,  FREEI    )

!
!           Set base address.
!
            BASE  =  (P-1) * PGSIZI  +  PGBASI


         ELSE

            CALL DASADI ( HANDLE, PGSIZI, IFILL )

            CALL DASRDI ( HANDLE, EPNPI, EPNPI, NPI   )
            CALL DASUDI ( HANDLE, EPNPI, EPNPI, NPI+1 )

            P     =  NPI + 1
            BASE  =  NPI * PGSIZI  +  PGBASI

         END IF


      ELSE

         CALL SETMSG ( 'The data type code # was not recognized.' )
         CALL ERRINT ( '#', TYPE                                  )
         CALL SIGERR ( 'SPICE(INVALIDTYPE)'                       )
         CALL CHKOUT ( 'ZZEKPGAL'                                 )
         RETURN

      END IF

      CALL CHKOUT ( 'ZZEKPGAL' )
      RETURN






!$Procedure  ZZEKPGFR ( Private: EK, free page )

      ENTRY ZZEKPGFR ( HANDLE, TYPE, P )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Free a specified page.
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
      CALL CHKIN ( 'ZZEKPGFR' )

!
!     Check the file.
!
      CALL ZZEKPGCH ( HANDLE, 'WRITE' )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZEKPGFR' )
         RETURN
      END IF


      IF ( TYPE .EQ. CHR ) THEN
!
!        Validate the page number.  Find out how many pages are
!        out there.
!
         CALL DASRDI ( HANDLE, EPNPC, EPNPC, NPC )

         IF (  ( P .LT. 1 ) .OR. ( P .GT. NPC )  ) THEN

            CALL SETMSG ( 'Attempt to free non-existent CHR page. ' // &
                          'Page number = #; valid range is 1:#'     )
            CALL ERRINT ( '#',  P                                   )
            CALL ERRINT ( '#',  NPC                                 )
            CALL SIGERR ( 'SPICE(INVALIDINDEX)'                     )
            CALL CHKOUT ( 'ZZEKPGFR'                                )
            RETURN

         END IF

!
!        Get the current character free pointer and free page count.
!
         CALL DASRDI ( HANDLE, EPFPC,  EPFPC,  FREEC  )
         CALL DASRDI ( HANDLE, EPNFPC, EPNFPC, NFREEC )

!
!        Insert into the freed page a pointer to the head of the
!        free list.
!
         CALL PRTENC ( FREEC, ENCPAG )
         ADDR  =  (P-1)*PGSIZC + 1

         CALL DASUDC ( HANDLE, ADDR, ADDR+4, 1, 5, ENCPAG )

!
!        Update the current character free pointer and free page count.
!
         CALL DASUDI ( HANDLE, EPFPC,  EPFPC,  P        )
         CALL DASUDI ( HANDLE, EPNFPC, EPNFPC, NFREEC+1 )


      ELSE IF ( TYPE .EQ. DP ) THEN
!
!        Validate the page number.  Find out how many pages are
!        out there.
!
         CALL DASRDI ( HANDLE, EPNPD, EPNPD, NPD )

         IF (  ( P .LT. 1 ) .OR. ( P .GT. NPD )  ) THEN

            CALL SETMSG ( 'Attempt to free non-existent DP page. ' // &
                          'Page number = #; valid range is 1:#'     )
            CALL ERRINT ( '#',  P                                   )
            CALL ERRINT ( '#',  NPD                                 )
            CALL SIGERR ( 'SPICE(INVALIDINDEX)'                     )
            CALL CHKOUT ( 'ZZEKPGFR'                                )
            RETURN

         END IF

!
!        Get the current d.p. free pointer and free page count.
!
         CALL DASRDI ( HANDLE, EPFPD,  EPFPD,  FREED  )
         CALL DASRDI ( HANDLE, EPNFPD, EPNFPD, NFREED )

!
!        Insert into the freed page a pointer to the head of the
!        free list.
!
         ADDR  =  (P-1)*PGSIZD + 1

         CALL DASUDD ( HANDLE, ADDR, ADDR, DBLE(FREED) )

!
!        Update the current d.p. free pointer and free page count.
!
         CALL DASUDI ( HANDLE, EPFPD,  EPFPD,  P        )
         CALL DASUDI ( HANDLE, EPNFPD, EPNFPD, NFREED+1 )




      ELSE IF ( TYPE .EQ. INT ) THEN
!
!        Validate the page number.  Find out how many pages are
!        out there.
!
         CALL DASRDI ( HANDLE, EPNPI, EPNPI, NPI )

         IF (  ( P .LT. 1 ) .OR. ( P .GT. NPI )  ) THEN

            CALL SETMSG ( 'Attempt to free non-existent INT page. ' // &
                          'Page number = #; valid range is 1:#'     )
            CALL ERRINT ( '#',  P                                   )
            CALL ERRINT ( '#',  NPI                                 )
            CALL SIGERR ( 'SPICE(INVALIDINDEX)'                     )
            CALL CHKOUT ( 'ZZEKPGFR'                                )
            RETURN

         END IF

!
!        Get the current integer free pointer and free page count.
!
         CALL DASRDI ( HANDLE, EPFPI,  EPFPI,  FREEI  )
         CALL DASRDI ( HANDLE, EPNFPI, EPNFPI, NFREEI )

!
!        Insert into the freed page a pointer to the head of the
!        free list.
!
         ADDR  =  (P-1)*PGSIZI  +  PGBASI  +  1

         CALL DASUDI ( HANDLE, ADDR, ADDR, FREEI )

!
!        Update the current integer free pointer and free page count.
!
         CALL DASUDI ( HANDLE, EPFPI,  EPFPI,  P        )
         CALL DASUDI ( HANDLE, EPNFPI, EPNFPI, NFREEI+1 )


      ELSE

         CALL SETMSG ( 'The data type code # was not recognized.' )
         CALL ERRINT ( '#', TYPE                                  )
         CALL SIGERR ( 'SPICE(INVALIDTYPE)'                       )
         CALL CHKOUT ( 'ZZEKPGFR'                                 )
         RETURN

      END IF

      CALL CHKOUT ( 'ZZEKPGFR' )
      RETURN




!$Procedure  ZZEKPGRC ( Private: EK, read character page )

      ENTRY ZZEKPGRC ( HANDLE, P, PAGEC )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Read a specified character page.
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
!
!     Find out how many character pages are in use.
!
      CALL DASRDI ( HANDLE, EPNPC, EPNPC, NPC )

      IF (  ( P .LT. 1 )  .OR.  ( P .GT. NPC )  ) THEN

         CALL CHKIN  ( 'ZZEKPGRC'                           )
         CALL SETMSG ( 'CHR page = #; valid range is [1:#]' )
         CALL ERRINT ( '#', P                               )
         CALL ERRINT ( '#', NPC                             )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                )
         CALL CHKOUT ( 'ZZEKPGRC'                           )
         RETURN

      END IF

      L      =   LEN( PAGEC )
      E      =   MIN( L, PGSIZC )

      ADDR   =   (P-1)*PGSIZC + 1

      CALL DASRDC ( HANDLE, ADDR, ADDR+PGSIZC-1, 1, E, PAGEC )

      IF ( L .GT. E ) THEN
         PAGEC ( E+1: ) = ' '
      END IF

      RETURN




!$Procedure  ZZEKPGRD ( Private: EK, read d.p. page )

      ENTRY ZZEKPGRD ( HANDLE, P, PAGED )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Read a specified double precision page.
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
!
!     Find out how many d.p. pages are in use.
!
      CALL DASRDI ( HANDLE, EPNPD, EPNPD, NPD )

      IF (  ( P .LT. 1 )  .OR.  ( P .GT. NPD )  ) THEN

         CALL CHKIN  ( 'ZZEKPGRD'                           )
         CALL SETMSG ( 'DP page = #; valid range is [1:#]'  )
         CALL ERRINT ( '#', P                               )
         CALL ERRINT ( '#', NPD                             )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                )
         CALL CHKOUT ( 'ZZEKPGRD'                           )
         RETURN

      END IF

      ADDR   =   (P-1)*PGSIZD + 1

      CALL DASRDD ( HANDLE, ADDR, ADDR+PGSIZD-1, PAGED )

      RETURN





!$Procedure  ZZEKPGRI ( Private: EK, read integer page )

      ENTRY ZZEKPGRI ( HANDLE, P, PAGEI )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Read a specified integer page.
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
!
!     Find out how many integer pages are in use.
!
      CALL DASRDI ( HANDLE, EPNPI, EPNPI, NPI )

      IF (  ( P .LT. 1 )  .OR.  ( P .GT. NPI )  ) THEN

         CALL CHKIN  ( 'ZZEKPGRI'                           )
         CALL SETMSG ( 'INT page = #; valid range is [1:#]' )
         CALL ERRINT ( '#', P                               )
         CALL ERRINT ( '#', NPI                             )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                )
         CALL CHKOUT ( 'ZZEKPGRI'                           )
         RETURN

      END IF

      ADDR   =   PGBASI  +  (P-1)*PGSIZI + 1

      CALL DASRDI ( HANDLE, ADDR, ADDR+PGSIZI-1, PAGEI )

      RETURN






!$Procedure  ZZEKPGWC ( Private: EK, write character page )

      ENTRY ZZEKPGWC ( HANDLE, P, PAGEC )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Write a specified character page.
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
      CALL ZZEKPGCH ( HANDLE, 'WRITE' )

      IF ( FAILED() ) THEN
         RETURN
      END IF

!
!     Find out how many character pages are in use.
!
      CALL DASRDI ( HANDLE, EPNPC, EPNPC, NPC )

      IF (  ( P .LT. 1 )  .OR.  ( P .GT. NPC )  ) THEN

         CALL CHKIN  ( 'ZZEKPGWC'                           )
         CALL SETMSG ( 'CHR page = #; valid range is [1:#]' )
         CALL ERRINT ( '#', P                               )
         CALL ERRINT ( '#', NPC                             )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                )
         CALL CHKOUT ( 'ZZEKPGWC'                           )
         RETURN

      END IF


      L  =  LEN( PAGEC )

      IF ( L .LT. PGSIZC ) THEN

         CALL CHKIN  ( 'ZZEKPGWC'                                    )
         CALL SETMSG ( 'Input CHR page size = #; valid size is [#:]' )
         CALL ERRINT ( '#', L                                        )
         CALL ERRINT ( '#', PGSIZC                                   )
         CALL SIGERR ( 'SPICE(STRINGTOOSHORT)'                       )
         CALL CHKOUT ( 'ZZEKPGWC'                                    )
         RETURN

      END IF


      ADDR   =   (P-1)*PGSIZC + 1

      CALL DASUDC ( HANDLE, ADDR, ADDR+PGSIZC-1, 1, PGSIZC, PAGEC )

      RETURN





!$Procedure  ZZEKPGWD ( Private: EK, write d.p. page )

      ENTRY ZZEKPGWD ( HANDLE, P, PAGED )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Write a specified double precision page.
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
!
!     Validate the file.
!
      CALL ZZEKPGCH ( HANDLE, 'WRITE' )

      IF ( FAILED() ) THEN
         RETURN
      END IF

!
!     Find out how many d.p. pages are in use.
!
      CALL DASRDI ( HANDLE, EPNPD, EPNPD, NPD )

      IF (  ( P .LT. 1 )  .OR.  ( P .GT. NPD )  ) THEN

         CALL CHKIN  ( 'ZZEKPGWD'                           )
         CALL SETMSG ( 'DP page = #; valid range is [1:#]'  )
         CALL ERRINT ( '#', P                               )
         CALL ERRINT ( '#', NPD                             )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                )
         CALL CHKOUT ( 'ZZEKPGWD'                           )
         RETURN

      END IF


      ADDR   =  (P-1)*PGSIZD + 1

      CALL DASUDD ( HANDLE, ADDR, ADDR+PGSIZD-1, PAGED )

      RETURN






!$Procedure  ZZEKPGWI ( Private: EK, write integer page )

      ENTRY ZZEKPGWI ( HANDLE, P, PAGEI )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Write a specified integer page.
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
      CALL ZZEKPGCH ( HANDLE, 'WRITE' )

      IF ( FAILED() ) THEN
         RETURN
      END IF

!
!     Find out how many integer pages are in use.
!
      CALL DASRDI ( HANDLE, EPNPI, EPNPI, NPI )

      IF (  ( P .LT. 1 )  .OR.  ( P .GT. NPI )  ) THEN

         CALL CHKIN  ( 'ZZEKPGWI'                           )
         CALL SETMSG ( 'INT page = #; valid range is [1:#]' )
         CALL ERRINT ( '#', P                               )
         CALL ERRINT ( '#', NPI                             )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                )
         CALL CHKOUT ( 'ZZEKPGWI'                           )
         RETURN

      END IF


      ADDR   =   PGBASI + (P-1)*PGSIZI + 1

      CALL DASUDI ( HANDLE, ADDR, ADDR+PGSIZI-1, PAGEI )

      RETURN






!$Procedure  ZZEKPGBS ( Private: EK, map page to base address )

      ENTRY ZZEKPGBS ( TYPE, P, BASE )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Map a page to its base address.
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

      IF ( TYPE .EQ. CHR ) THEN

         BASE  =   ( P - 1 ) * PGSIZC


      ELSE IF ( TYPE .EQ. DP ) THEN

         BASE  =   ( P - 1 ) * PGSIZD


      ELSE IF ( TYPE .EQ. INT ) THEN

         BASE  =   ( P - 1 ) * PGSIZI   +   PGBASI

      ELSE

         CALL CHKIN  ( 'ZZEKPGBS'                                 )
         CALL SETMSG ( 'The data type code # was not recognized.' )
         CALL ERRINT ( '#', TYPE                                  )
         CALL SIGERR ( 'SPICE(INVALIDTYPE)'                       )
         CALL CHKOUT ( 'ZZEKPGBS'                                 )
         RETURN

      END IF

      RETURN






!$Procedure  ZZEKPGPG ( Private: EK, map address to page )

      ENTRY ZZEKPGPG ( TYPE, ADDRSS, P, BASE )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Map a DAS address to the number of the page containing it.  Also
!     return the base address of the page.
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



      IF ( TYPE .EQ. CHR ) THEN

         P     =   ( ADDRSS  + PGSIZC - 1 ) / PGSIZC
         BASE  =   ( P - 1 ) * PGSIZC


      ELSE IF ( TYPE .EQ. DP ) THEN

         P     =   ( ADDRSS  + PGSIZD - 1 ) / PGSIZD
         BASE  =   ( P - 1 ) * PGSIZD


      ELSE IF ( TYPE .EQ. INT ) THEN

         P     =   ( ( ADDRSS-PGBASI )  +   PGSIZI - 1 ) / PGSIZI
         BASE  =   ( P - 1 ) * PGSIZI   +   PGBASI

      ELSE

         CALL CHKIN  ( 'ZZEKPGPG'                                 )
         CALL SETMSG ( 'The data type code # was not recognized.' )
         CALL ERRINT ( '#', TYPE                                  )
         CALL SIGERR ( 'SPICE(INVALIDTYPE)'                       )
         CALL CHKOUT ( 'ZZEKPGPG'                                 )
         RETURN

      END IF

      RETURN




!$Procedure  ZZEKPGST ( Private: EK, return paging statistics )

      ENTRY ZZEKPGST ( HANDLE, STAT, VALUE )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Return paging statistics.
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


      CALL CHKIN ( 'ZZEKPGST' )


      IF (  EQSTR( STAT, 'N_C_ALLOC' )  ) THEN

         CALL DASRDI ( HANDLE, EPNPC, EPNPC, VALUE   )


      ELSE IF (  EQSTR( STAT, 'N_D_ALLOC' )  ) THEN

         CALL DASRDI ( HANDLE, EPNPD, EPNPD, VALUE   )


      ELSE IF (  EQSTR( STAT, 'N_I_ALLOC' )  ) THEN

         CALL DASRDI ( HANDLE, EPNPI, EPNPI, VALUE   )


      ELSE IF (  EQSTR( STAT, 'N_C_FREE' )  ) THEN

         CALL DASRDI ( HANDLE, EPNFPC, EPNFPC, VALUE   )


      ELSE IF (  EQSTR( STAT, 'N_D_FREE' )  ) THEN

         CALL DASRDI ( HANDLE, EPNFPD, EPNFPD, VALUE   )


      ELSE IF (  EQSTR( STAT, 'N_I_FREE' )  ) THEN

         CALL DASRDI ( HANDLE, EPNFPI, EPNFPI, VALUE   )

      ELSE

         CALL SETMSG ( 'Statistic # is not supported.' )
         CALL ERRCH  ( '#',  STAT                      )
         CALL SIGERR ( 'SPICE(INVALIDOPTION)'          )
         CALL CHKOUT ( 'ZZEKPGST'                      )
         RETURN

      END IF

      CALL CHKOUT ( 'ZZEKPGST' )
      RETURN

      END
