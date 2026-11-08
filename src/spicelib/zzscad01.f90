!$Procedure ZZSCAD01 ( SPICE private, add clock to database, type 01 )

      subroutine zzscad01 ( sc,     hdsclk, scpool, clklst, dpfree, &
                            dpbuff, ifree,  intbuf, scbase, sclkat )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Add to the SCLK type 01 database data for a specified SCLK.
!
! $ Required_Reading
!
!     SCLK
!
! $ Keywords
!
!     SCLK
!     TIME
!
! $ Declarations

      implicit none

      include 'errhnd.inc'
      include 'zzsc01.inc'

      integer(ip)           sc
      integer(ip)           hdsclk ( * )
      integer(ip)           scpool ( lbsngl : * )
      integer(ip)           clklst ( * )
      integer(ip)           dpfree
      real(wp)              dpbuff ( * )
      integer(ip)           ifree
      integer(ip)           intbuf ( * )
      integer(ip)           scbase ( * )
      integer(ip)           sclkat

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     SC         I   SCLK ID code.
!     HDSCLK    I-O  Array of heads of hash collision lists.
!     SCPOOL    I-O  Singly linked collision list pool.
!     CLKLST    I-O  Array of SCLK codes.
!     DPFREE    I-O  Index of first free entry in d.p. data buffer.
!     DPBUFF    I-O  D.p. (double precision) data buffer
!     IFREE     I-O  Index of first free entry in integer data buffer.
!     INTBUF    I-O  Integer data buffer.
!     SCBASE    I-O  Base index in integer buffer corresponding to SC.
!     SCLKAT     O   Index in CLKLST of SC.
!
! $ Detailed_Input
!
!     SC          is the SCLK ID of a type 1 clock. Data for this clock
!                 are to be added to the type 1 database.
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
! $ Detailed_Output
!
!     HDSCLK      is the input list head array, updated as needed to
!                 reflect the addition of the new clock. Note that the
!                 array won't be updated if the clock is added to a
!                 pre-existing collision list.
!
!     SCPOOL      is the input singly linked list pool, updated to
!                 reflect the addition of the new clock.
!
!     CLKLST      is an array of SCLK IDs, updated to reflect the
!                 addition of the new clock.
!
!     DPFREE      is index in the database's double precision buffer of
!                 the first free element after addition of data for the
!                 new clock.
!
!     DPBUFF      is the double precision buffer of the type 1
!                 database, updated to reflect the addition of the new
!                 clock.
!
!     IFREE       is index in the database's integer buffer of the
!                 first free element after addition of data for the new
!                 clock.
!
!     INTBUF      is the integer buffer of the type 1 database, updated
!                 to reflect the addition of the new clock.
!
!     SCBASE      is an array of base addresses in the array INTBUF,
!                 updated to reflect the addition of the new clock.
!
!     SCLKAT      is the index in CLKLST at which the input clock ID
!                 SC is stored.
!
! $ Parameters
!
!     See the include files zzsc01.inc and zzctr.inc.
!
! $ Exceptions
!
!     1)  If any of the kernel variables containing SCLK coefficients,
!         partition start or stop times, moduli, or offsets are not
!         found, the error SPICE(KERNELVARNOTFOUND) will be signaled.
!
!     2)  If any integer kernel variables are not found, the error will
!         be signaled by a routine in the call tree of this routine.
!
!     3)  If any kernel variable used by this routine does not have
!         numeric type, the error will be signaled by a routine in the
!         call tree of this routine.
!
!     4)  If the counts of partition start or stop times do not match,
!         the error SPICE(NUMPARTSUNEQUAL) will be signaled.
!
!     5)  If the count of partitions exceeds the limit MXPART,
!         the error SPICE(TOOMANYPARTITIONS) will be signaled.
!
!     6)  If the count of coefficients exceeds the limit 3*MXCOEF,
!         the error SPICE(TOOMANYCOEFFS) will be signaled.
!
!     7)  If any kernel variable other than the coefficient set or
!         the partition bounds has size that exceeds the applicable
!         limit, the error SPICE(KERNELVARTOOLARGE) will be signaled.
!
!     8)  If an error occurs while this routine looks up data from the
!         kernel pool, the error will be signaled by a routine in the
!         call tree of this routine.
!
!     9)  If an error occurs while this routine adds data to a hash,
!         the error will be signaled by a routine in the call tree of
!         this routine.
!
!     10) If the database contains entries for the maximum number
!         of clocks and an addition is requested, the database will
!         be re-initialized, and an entry for the new clock will be
!         created. This case is not an error.
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
!     None.
!
! $ Examples
!
!     None. See usage in ZZSCUP01.
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
!     add sclk to type 1 sclk database
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
!     Local parameters
!
      integer(ip), parameter :: namlen = 60

!
!     Following are parameters for the indices within the
!     array NAMLST of the kernel variable names used by the
!     SC01 entry points.
!
!     NOTE: indices for the d.p. variables must be listed first.
!
      integer(ip), parameter :: cofidx = 1

      integer(ip), parameter :: prsidx = cofidx + 1

      integer(ip), parameter :: preidx = prsidx + 1

      integer(ip), parameter :: offidx = preidx + 1

      integer(ip), parameter :: modidx = offidx + 1

      integer(ip), parameter :: nflidx = modidx + 1

      integer(ip), parameter :: delidx = nflidx + 1

      integer(ip), parameter :: sysidx = delidx + 1

      integer(ip), parameter :: nkitem = sysidx

      integer(ip), parameter :: ndp = modidx

!
!     Maximum number of elements in coefficient array:
!
      integer(ip), parameter :: mxcfsz = 3 * mxcoef

!
!     Local variables
!
      character(1)          kvtype ( nkitem )
      character(namlen)     kvname ( nkitem )
      character(namlen)     namlst ( nkitem )
      character(smsgln)     shrtms

      integer(ip)           dpbase
      integer(ip)           dproom
      integer(ip)           i
      integer(ip)           ibase
      integer(ip)           ibix   ( nkitem )
      integer(ip)           iroom
      integer(ip)           kvmaxn ( nkitem )
      integer(ip)           kvsize ( nkitem )
      integer(ip)           n
      integer(ip)           ncoeff
      integer(ip)           ndpval
      integer(ip)           nmod
      integer(ip)           noff
      integer(ip)           npart
      integer(ip)           npartb
      integer(ip)           nparte
      integer(ip)           nscavl
      integer(ip)           nsys
      integer(ip)           prvsc

      logical               found
      logical               new

!
!     Saved values
!
      save                 ibix
      save                 kvmaxn
      save                 namlst

!
!     Initial values
!
      data ( namlst(i), kvmaxn(i), ibix(i), i = 1, nkitem ) / &
        'SCLK01_COEFFICIENTS',    mxcfsz,    ixbcof, &
        'SCLK_PARTITION_START',   mxpart,    ixbstr, &
        'SCLK_PARTITION_END',     mxpart,    ixbend, &
        'SCLK01_OFFSETS',         mxnfld,    ixboff, &
        'SCLK01_MODULI',          mxnfld,    ixbmod, &
        'SCLK01_N_FIELDS',        1,         0, &
        'SCLK01_OUTPUT_DELIM',    1,         0, &
        'SCLK01_TIME_SYSTEM',     1,         0              /

      if ( return () ) return

!
!     Make up a list of names of kernel variables that we'll use.
!     The first name in the list is SCLK_KERNEL_ID, which does not
!     require the addition of a spacecraft code suffix.  For the
!     rest of the names, we'll have to add the suffix.
!
      call movec ( namlst, nkitem, kvname )

      do i = 1, nkitem
         call suffix ( '_#',        0,         kvname(i) )
         call repmi  ( kvname(i),  '#',  -sc,  kvname(i) )
      end do

!
!     Check available room in the clock hash and in the numeric data
!     buffers. If there's no room, re-initialize the entire data
!     structure.
!
      call zzhsiavl ( scpool, nscavl )

      if ( nscavl == 0 ) then

         call zzscin01 ( hdsclk, scpool, clklst, dpfree, ifree, prvsc )

      end if

!
!     Add this clock to the hash.
!
      call zzhsiadd ( hdsclk, scpool, clklst, sc, sclkat, new )

      if ( failed() ) then
!
!        This code should be unreachable but is provided for safety.
!
         call zzscin01 ( hdsclk, scpool, clklst, dpfree, ifree, prvsc )
         return

      end if

!
!     Check room in the integer buffer.
!
      iroom = ibufsz + 1 - ifree

      if (  ( iroom < nivals ) .or. ( iroom > ibufsz )  ) then
!
!        This code should be unreachable but is provided for safety.
!
         i = ifree

         call zzscin01 ( hdsclk, scpool, clklst, dpfree, ifree, prvsc )

         call setmsg ( 'IROOM was #; must be in range #:#. ' &
         //            'IFREE was #; must be in range 1:#.' )
         call errint ( '#', iroom    )
         call errint ( '#', nivals   )
         call errint ( '#', ibufsz   )
         call errint ( '#', i        )
         call errint ( '#', ibufsz+1 )
         call sigerr ( 'SPICE(BUG)'  )
         return

      end if

!
!     Compute available room for double precision data.
!
      dproom = dbufsz + 1 - dpfree

      if (  ( dproom < 0 ) .or. ( dproom > dbufsz )  ) then
!
!        This code should be unreachable but is provided for safety.
!
         i = dpfree

         call zzscin01 ( hdsclk, scpool, clklst, dpfree, ifree, prvsc )

         call setmsg ( 'DPROOM was #; must be in range 0:#. ' &
         //            'DPFREE was #; must be in range 1:#.' )
         call errint ( '#', dproom   )
         call errint ( '#', dbufsz   )
         call errint ( '#', i        )
         call errint ( '#', dbufsz+1 )
         call sigerr ( 'SPICE(BUG)'  )
         return

      end if

!
!     Set the base address for the next item to be added to the
!     integer buffer. The index of the first element of the item
!     is the successor of IBASE.
!
      ibase            = ifree-1
      scbase( sclkat ) = ibase

!
!     Fetch from the kernel pool the integer items we need:
!
!        -  The number of fields in an (unabridged) SCLK string
!        -  The output delimiter code
!        -  The parallel time system code (optional; defaults to TDB)
!
      call scli01 ( namlst(nflidx), sc, 1, n,    intbuf(ibase+ixnfld) )
      call scli01 ( namlst(delidx), sc, 1, n,    intbuf(ibase+ixdlim) )
      call scli01 ( namlst(sysidx), sc, 1, nsys, intbuf(ibase+ixtsys) )

      if ( failed() ) then

         call zzscin01 ( hdsclk, scpool, clklst, dpfree, ifree, prvsc )
         return

      end if

!
!     The time system code need not be provided in the kernel pool.
!     Set it to the default value if it's not present.
!
      if ( nsys == 0 ) then
         intbuf(ibase+ixtsys) = tdb
      end if

!
!     Check for presence in the kernel pool of all required d.p. kernel
!     variables.
!
!     Get and check the sizes of the d.p. arrays: coefficients,
!     partitions, moduli, and offsets.
!
!     We obtain sizes of d.p. kernel variables rather than using sizes
!     returned by SCLD01 because we'll read data directly into our
!     passed-in buffers. We need to know in advance how much room is
!     needed.
!
      do i = 1, ndp

         call dtpool ( kvname(i), found, kvsize(i), kvtype(i) )

         if ( failed() ) then
!
!           This code should be unreachable but is provided for
!           safety. Initialize local data structures. ZZSCIN01 sets
!           PRVSC to 0.
!
            call zzscin01 ( hdsclk, scpool, clklst, &
                            dpfree, ifree,  prvsc  )

            return

         end if

!
!        Check that each required d.p. kernel variable was found.
!
         if ( .not. found ) then

            call zzscin01 ( hdsclk, scpool, clklst, &
                            dpfree, ifree,  prvsc  )

            call setmsg ( 'Kernel variable # for spacecraft clock ' &
            //            '# was not found. An SCLK kernel for ' &
            //            'this clock may not have been loaded.'   )
            call errch  ( '#', kvname(i)                           )
            call errint ( '#', sc                                  )
            call sigerr ( 'SPICE(KERNELVARNOTFOUND)'               )
            return

         end if

!
!        Check the number of values associated with the kernel
!        variable.
!
         if ( kvsize(i) > kvmaxn(i) ) then

            call zzscin01 ( hdsclk, scpool, clklst, &
                            dpfree, ifree,  prvsc  )

            call setmsg ( 'The number of values associated with ' &
            //            'the kernel variable # for clock # ' &
            //            'is #, which exceeds the limit #.'     )
            call errch  ( '#', kvname(i)                         )
            call errint ( '#', sc                                )
            call errint ( '#', kvsize(i)                         )
            call errint ( '#', kvmaxn(i)                         )
!
!           Customize the short error message a bit.
!
            if ( i == cofidx ) then

               shrtms = 'SPICE(TOOMANYCOEFFS)'

            else if (     ( i == prsidx ) &
                     .or. ( i == preidx ) ) then

               shrtms = 'SPICE(TOOMANYPARTITIONS)'
            else
               shrtms = 'SPICE(KERNELVARTOOLARGE)'
            end if

            call sigerr (  shrtms    )
            return

         end if

      end do

!
!     Store kernel variable sizes as scalars for convenience.
!
      ncoeff = kvsize(cofidx)
      npartb = kvsize(prsidx)
      nparte = kvsize(preidx)
      nmod   = kvsize(modidx)
      noff   = kvsize(offidx)

!
!     NDPVAL is the number of d.p. values that must be buffered to
!     support this clock.
!
      ndpval = ncoeff + npartb + nparte + nmod + noff

!
!     Check whether sizes of partition start and end kernel variables
!     match.
!
      if ( npartb /= nparte  ) then

         call zzscin01 ( hdsclk, scpool, clklst, dpfree, ifree, prvsc )

         call setmsg ( 'The numbers of partition start times # and ' &
         //            'stop times # are unequal for spacecraft ' &
         //            'clock #.'                                  )
         call errint ( '#', npartb                                 )
         call errint ( '#', nparte                                 )
         call errint ( '#', sc                                     )
         call sigerr ( 'SPICE(NUMPARTSUNEQUAL)'                    )
         return

      else
         npart = npartb
      end if

      if ( ndpval > dbufsz ) then
!
!        We couldn't make enough room even if we dumped all buffered
!        data.
!
!        This code should be unreachable but is provided for safety.
!        We've already checked the existence, types, and sizes of the
!        d.p. kernel variables.
!
         call zzscin01 ( hdsclk, scpool, clklst, dpfree, ifree, prvsc )

         call setmsg ( 'Total number of double precision data ' &
         //            'values for SCLK # is #; this count exceeds ' &
         //            'the maximum supported count #.' )
         call errint ( '#', sc                          )
         call errint ( '#', ndpval                      )
         call errint ( '#', dbufsz                      )
         call sigerr ( 'SPICE(BUG)'                     )
         return

      else if ( ndpval > dproom ) then
!
!        We don't have room for this clock, but we will if we dump
!        the existing buffered data.
!
         call zzscin01 ( hdsclk, scpool, clklst, dpfree, ifree, prvsc )

      end if

!
!     Store in the integer buffer the sizes of the coefficient array
!     and the number of partitions.
!
      intbuf( ibase+ixncof ) = ncoeff
      intbuf( ibase+ixnprt ) = npart

!
!     Fetch from the kernel pool the d.p. items we need:
!
!        -  The SCLK coefficients array
!        -  The partition start times
!        -  The partition end times
!        -  The moduli of the fields of an SCLK string
!        -  The offsets for each clock field.
!
!     Set the base index in the d.p. buffer of the next d.p. item
!     to store. The first element of the item has index DPBASE+1.
!
!     We don't update DPFREE until all d.p. data have been fetched
!     successfully.
!
      dpbase = dpfree - 1

      do i = 1, ndp
!
!        Store in the integer buffer the base index in the d.p.
!        buffer of the kernel variable's values.
!
         intbuf( ibase + ibix(i) ) = dpbase
!
!        Fetch d.p. values into buffer.
!
         call scld01 ( namlst(i), sc, &
                       kvmaxn(i), n,  dpbuff(dpbase + 1) )

         if ( failed() ) then
!
!           This code could be reached if the kernel variable has
!           character type.
!
            call zzscin01 ( hdsclk, scpool, clklst, &
                            dpfree, ifree,  prvsc  )

            return

         end if
!
!        Update the d.p. buffer base index.
!
         dpbase = dpbase + n

      end do

!
!     Account for buffer usage for the new clock.
!
      dpfree = dpfree + ndpval
      ifree  = ifree  + nivals

      return
      end
