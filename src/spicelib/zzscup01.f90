!$Procedure ZZSCUP01 ( Private, SCLK database update, type 01 )

      subroutine zzscup01 ( sc,     polctr, hdsclk, scpool, clklst, &
                            dpfree, dpbuff, ifree,  intbuf, scbase, &
                            prvsc,  nfield, delcde, timsys, ncoeff, &
                            npart,  cofbas, strbas, endbas, modbas, &
                            offbas                                 )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     If necessary, update the SCLK type 01 database and associated
!     SCLK parameters.
!
! $ Required_Reading
!
!     SCLK
!     TIME
!
! $ Keywords
!
!     SCLK
!
! $ Declarations

      implicit none

      include 'zzsc01.inc'

      integer(ip)           sc
      integer(ip)           polctr
      integer(ip)           hdsclk ( * )
      integer(ip)           scpool ( lbsngl : * )
      integer(ip)           clklst ( * )
      integer(ip)           dpfree
      real(wp)              dpbuff ( * )
      integer(ip)           ifree
      integer(ip)           intbuf ( * )
      integer(ip)           scbase ( * )
      integer(ip)           prvsc
      integer(ip)           nfield
      integer(ip)           delcde
      integer(ip)           timsys
      integer(ip)           ncoeff
      integer(ip)           npart
      integer(ip)           cofbas
      integer(ip)           strbas
      integer(ip)           endbas
      integer(ip)           modbas
      integer(ip)           offbas

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     SC         I   SCLK ID code.
!     POLCTR    I-O  Kernel pool update counter.
!     HDSCLK    I-O  Array of heads of hash collision lists.
!     SCPOOL    I-O  Singly linked collision list pool.
!     CLKLST    I-O  Array of SCLK codes.
!     DPFREE    I-O  Index of first free entry in d.p. data buffer.
!     DPBUFF    I-O  Double precision SCLK data buffer.
!     IFREE     I-O  Index of first free entry in integer data buffer.
!     INTBUF    I-O  Integer SCLK data buffer.
!     SCBASE    I-O  Array of SCLK integer buffer base indices.
!     PRVSC     I-O  Previous SCLK ID code.
!     NFIELD    I-O  Number of fields of current SCLK.
!     DELCDE    I-O  Delimiter code of current SCLK.
!     TIMSYS    I-O  Time system code of current SCLK.
!     NCOEFF    I-O  Number of coefficients current SCLK.
!     NPART     I-O  Number of partitions of current SCLK.
!     COFBAS    I-O  Base index of coefficients in d.p. buffer.
!     STRBAS    I-O  Base index of partition starts in d.p. buffer.
!     ENDBAS    I-O  Base index of partition stops in d.p. buffer.
!     MODBAS    I-O  Base index of SCLK moduli in d.p. buffer.
!     OFFBAS    I-O  Base index of SCLK offsets in d.p. buffer.
!
! $ Detailed_Input
!
!     SC          is the SCLK ID of a type 1 clock. Data for this clock
!                 are to be added to the type 1 database.
!
!     POLCTR      is the type 1 SCLK database's kernel pool tracking
!                 counter.
!
!     HDSCLK      is an array containing the head node indices of the
!                 SCLK ID collision lists stored in SCPOOL.
!
!     SCPOOL      is a singly linked list pool containing collision
!                 lists for sets of SCLK IDs that have the same hash
!                 values.
!
!     CLKLST      is an array of SCLK IDs, which is parallel to the
!                 data portion of SCPOOL. Each allocated node of
!                 SCPOOL is the index in CLKLST of the associated
!                 SCLK ID.
!
!     DPFREE      is index in the database's double precision buffer of
!                 the first free element.
!
!     DPBUFF      is the double precision buffer of the type 1
!                 database.
!
!     IFREE       is index in the database's integer buffer of the
!                 first free element.
!
!     INTBUF      is the integer buffer of the type 1 database.
!
!     SCBASE      is an array of base addresses in the array INTBUF of
!                 the integer data sets associated with the clocks
!                 in the type 1 database. The Ith element of SCBASE is
!                 the base address of the clock at index I in the
!                 array CLKLST.
!
!                 The "base" address of an integer data set immediately
!                 precedes the first index of that data set: the index
!                 in INTBUF of the first element of the integer data of
!                 CLKLST(I) is SCBASE(I)+1.
!
!     PRVSC       is the SCLK ID code of the type 1 clock seen on the
!                 previous, successful call to any SC01 entry point. If
!                 the last call was unsuccessful, or if the current
!                 call is the first, PRVSC will be 0, which is never a
!                 valid SCLK ID code. If a clock of type other than 1
!                 was seen by SCTY01 on the previous call, PRVSC will
!                 be 0.
!
!     NFIELD      is the number of fields of the type 1 SCLK seen on
!                 the previous, successful call to any SC01 entry
!                 point. NFIELD is valid only if PRVSC is non-zero.
!
!     DELCDE      is the delimiter code of the type 1 SCLK seen on the
!                 previous, successful call to any SC01 entry point.
!                 DELCDE is valid only if PRVSC is non-zero.
!
!     TIMSYS      is the parallel time system of the type 1 SCLK seen
!                 on the previous, successful call to any SC01 entry
!                 point. TIMSYS is valid only if PRVSC is non-zero.
!
!     NCOEFF      is the number of individual coefficients of the type
!                 1 SCLK seen on the previous, successful call to any
!                 SC01 entry point. NCOEFF is valid only if PRVSC is
!                 non-zero.
!
!     NPART       is the number partitions of the type 1 SCLK seen on
!                 the previous, successful call to any SC01 entry
!                 point. NPART is valid only if PRVSC is non-zero.
!
!     COFBAS      is the base index in the double precision buffer of
!                 the set of SCLK coefficients of the type 1 SCLK seen
!                 on the previous, successful call to any SC01 entry
!                 point. COFBAS is valid only if PRVSC is non-zero.
!
!                 The first coefficient associated with PRVSC is
!                 located at index COFBAS+1.
!
!     STRBAS      is the base index in the double precision buffer of
!                 the set of partition start times of the type 1 SCLK
!                 seen on the previous, successful call to any SC01
!                 entry point. STRBAS is valid only if PRVSC is
!                 non-zero.
!
!                 The first partition start time associated with PRVSC
!                 is located at index STRBAS+1.
!
!     ENDBAS      is the base index in the double precision buffer of
!                 the set of partition end times of the type 1 SCLK
!                 seen on the previous, successful call to any SC01
!                 entry point. ENDBAS is valid only if PRVSC is
!                 non-zero.
!
!                 The first partition end time associated with PRVSC is
!                 located at index ENDBAS+1.
!
!     MODBAS      is the base index in the double precision buffer of
!                 the set of SCLK moduli of the type 1 SCLK seen on the
!                 previous, successful call to any SC01 entry point.
!                 MODBAS is valid only if PRVSC is non-zero.
!
!                 The first SCLK modulus associated with PRVSC is
!                 located at index MODBAS+1.
!
!     OFFBAS      is the base index in the double precision buffer of
!                 the set of SCLK offsets of the type 1 SCLK seen on
!                 the previous, successful call to any SC01 entry
!                 point. OFFBAS is valid only if PRVSC is non-zero.
!
!                 The first SCLK offset associated with PRVSC is
!                 located at index OFFBAS+1.
!
! $ Detailed_Output
!
!     POLCTR      is the input kernel pool tracking counter, updated if
!                 necessary to reflect a kernel pool update.
!
!     HDSCLK      is the input list head node array, updated if
!                 necessary to reflect addition to the type 1 database
!                 of data for a new clock.
!
!     SCPOOL      is the input singly linked list pool, updated if
!                 necessary to reflect addition to the type 1 database
!                 of data for a new clock.
!
!     CLKLST      is the input array of SCLK IDs, updated if necessary
!                 to reflect addition to the type 1 database of data
!                 for a new clock.
!
!     DPFREE      is the input index of the first free element of the
!                 type 1 database's double precision buffer, updated if
!                 necessary to reflect addition to the type 1 database
!                 of data for a new clock.
!
!     DPBUFF      is the input double precision buffer of the type 1
!                 database, updated if necessary to reflect addition to
!                 the type 1 database of data for a new clock.
!
!     IFREE       is input index in the database's integer buffer of
!                 the first free element, updated if necessary to
!                 reflect addition to the type 1 database of data for a
!                 new clock.
!
!     INTBUF      is the input integer buffer of the type 1 database,
!                 updated if necessary to reflect addition to the type
!                 1 database of data for a new clock.
!
!     SCBASE      is the input array of base addresses in the array
!                 INTBUF, updated if necessary to reflect addition to
!                 the type 1 database of data for a new clock.
!
!     PRVSC       is set to the input SC if the call to this routine
!                 was successful. Otherwise PRVSC is set to zero.
!
!     NFIELD      is the number of fields of the type 1 SCLK designated
!                 by SC.
!
!     DELCDE      is the delimiter code of the type 1 SCLK designated
!                 by SC.
!
!     TIMSYS      is the parallel time system of the type 1 SCLK
!                 designated by SC.
!
!     NCOEFF      is the number of individual coefficients of the type
!                 1 SCLK designated by SC.
!
!     NPART       is the number partitions of the type 1 SCLK
!                 designated by SC.
!
!     COFBAS      is the base index in the double precision buffer of
!                 the set of SCLK coefficients of the type 1 SCLK
!                 designated by SC.
!
!     STRBAS      is the base index in the double precision buffer of
!                 the set of partition start times of the type 1 SCLK
!                 designated by SC.
!
!     ENDBAS      is the base index in the double precision buffer of
!                 the set of partition end times of the type 1 SCLK
!                 designated by SC.
!
!     MODBAS      is the base index in the double precision buffer of
!                 the set of SCLK moduli of the type 1 SCLK designated
!                 by SC.
!
!     OFFBAS      is the base index in the double precision buffer of
!                 the set of SCLK offsets of the type 1 SCLK
!                 designated by SC.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the total number of double precision values associated
!         with the input clock is greater than the double precision
!         buffer size, the error will be signaled by a routine in the
!         call tree of this routine.
!
!     2)  If the database contains entries for the maximum number
!         of clocks and an addition is requested, the database will
!         be re-initialized, and an entry for the new clock will be
!         created. This case is not an error.
!
!     3)  If an error occurs while this routine looks up data from the
!         kernel pool, the error will signaled by a routine in the call
!         tree of this routine.
!
! $ Files
!
!     Appropriate kernels must be loaded by the calling program before
!     this routine is called.
!
!     The following data are required:
!
!        - An SCLK kernel providing data for the SCLK designated by
!          the input ID code SC.
!
!     In all cases, kernel data are normally loaded once per program
!     run, NOT every time this routine is called.
!
! $ Particulars
!
!     This routine determines whether an update to the SCLK type 01
!     database is needed and updates the database and associated
!     SCLK parameter output arguments if so. Any updates to the kernel
!     pool trigger SCLK database updates. If the database doesn't
!     need to be updated, but the input SCLK ID doesn't match the
!     SCLK ID from the previous, successful call to any SC01 entry
!     point, as indicated by the condition
!
!         SC .EQ. PRVSC
!
!     then the output SCLK parameters are updated. If the input SCLK ID
!     matches that of the previous, successful call to an entry point
!     of SC01, and if the kernel pool has not been modified since that
!     call, the database is left unchanged, and the output SCLK
!     parameters are left untouched.
!
!     Note that a successful call to SCTY01 for a non-type 1 SCLK will
!     leave PRVSC set to 0, so the condition above won't be met.
!
!     If the input SCLK ID doesn't match the previous one and the
!     input ID is not present in the database, data for the input SCLK
!     will be looked up from the kernel pool, if possible. If the
!     required kernel pool data are found, the database is updated to
!     reflect addition of the new clock, and the integer SCLK
!     parameter output arguments are updated. If the data are not
!     found, an error will be signaled.
!
!     The type 1 SCLK database uses an add-or-clear policy for data
!     additions. If there is room in the SCLK ID hash and the data
!     buffers, data for a new clock are added to the database. If there
!     is not enough room, the database is re-initialized, after which
!     data for the new clock are added. If a new clock has too much
!     data to be buffered, an error is signaled.
!
!     Note that the database entry for a given SCLK is never updated
!     with new data for that clock: if the data change, that means the
!     kernel pool state has changed, so the type 01 SCLK database is
!     re-initialized before data for the SCLK are fetched.
!
! $ Examples
!
!     None. See usage in SC01.
!
! $ Restrictions
!
!     This is a SPICE-private routine. It should not be called directly
!     by user application code.
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
!-    SPICELIB Version 1.0.0, 01-DEC-2021 (NJB)
!
!-&

! $ Index_Entries
!
!     update current sclk data and type 1 sclk database
!
!-&


! $ Revisions
!
!     None.
!
!-&

!
!     SPICELIB functions
!
      logical               failed
      logical               return

!
!     Local variables
!
      integer(ip)           ibase
      integer(ip)           sclkat

      logical               samclk
      logical               update


      if ( return() ) return

!
!     Reinitialize local database if the kernel pool has been
!     updated. The call below syncs POLCTR.
!
      call zzpctrck ( polctr, update )

      if ( update ) then
!
!        Initialize local data structures.
!
         call zzscin01 ( hdsclk, scpool, clklst, &
                         dpfree, ifree,  prvsc )

         samclk = .false.
      else
         samclk = ( sc /= 0 ) .and. ( sc == prvsc )
      end if

!
!     Set the output arguments only if the clock has changed or if
!     a kernel pool update has occurred.
!
      if ( .not. samclk )  then
!
!        Look up the input clock. If the clock's information is not
!        buffered, try to look up the information from the kernel pool.
!
         call zzhsichk ( hdsclk, scpool, clklst, sc, sclkat )

         if ( sclkat == 0 ) then
!
!           This clock is unknown to the SC01 subsystem. Try to look up
!           the clock's specification from the kernel pool.
!
            call zzscad01 ( sc,     hdsclk, scpool, clklst, dpfree, &
                            dpbuff, ifree,  intbuf, scbase, sclkat )

            if ( failed() ) then
!
!              ZZSCAD01 will have initialized the type 01 SCLK
!              database; there's no need to do it again. Initialize
!              the SCLK integer parameters.
!
               nfield = 0
               delcde = 0
               timsys = 0
               ncoeff = 0
               npart  = 0

!
!              Indicate valid data for the previous were not obtained.
!
               prvsc = 0

               return

            end if

         end if

!
!        IBASE is the base address in the integer buffer of the integer
!        data for this clock.
!
         ibase = scbase( sclkat )

!
!        Set integer SCLK parameters to the correct values for
!        the clock designated by SC.
!
         nfield = intbuf( ibase+ixnfld )
         delcde = intbuf( ibase+ixdlim )
         timsys = intbuf( ibase+ixtsys )
         ncoeff = intbuf( ibase+ixncof )
         npart  = intbuf( ibase+ixnprt )
         cofbas = intbuf( ibase+ixbcof )
         strbas = intbuf( ibase+ixbstr )
         endbas = intbuf( ibase+ixbend )
         modbas = intbuf( ibase+ixbmod )
         offbas = intbuf( ibase+ixboff )

      end if

      return
      end
