!$Procedure      ZZDSKBSR ( DSK, buffer segments for readers )

      subroutine zzdskbsr ( fname, &
                            bodyid, &
                            handle, &
                            cmpfun, &
                            usrctr, &
                            update, &
                            dladsc, &
                            dskdsc, &
                            found   )
      use pepper_kinds, only: ip, wp

      implicit none

! $ Abstract
!
!     Load and unload DSK files for use by the readers. Buffer segments
!     for readers.
!
! $ Required_Reading
!
!     DSK
!     DAS
!
! $ Keywords
!
!     TOPOGRAPHY
!
! $ Declarations

      include 'dla.inc'
      include 'dskdsc.inc'
      include 'zzctr.inc'

      character(*)          fname
      integer(ip)           bodyid
      integer(ip)           handle
      logical               cmpfun
      external              cmpfun
      integer(ip)           usrctr ( * )
      logical               update
      integer(ip)           dladsc ( * )
      real(wp)              dskdsc ( * )
      logical               found


      integer(ip)           ftsize
      parameter           ( ftsize =  5000 )

      integer(ip)           btsize
      parameter           ( btsize =  100 )

      integer(ip)           lbpool
      parameter           ( lbpool =   -5 )

      integer(ip)           stsize
      parameter           ( stsize = 10000 )

! $ Brief_I/O
!
!     Variable  I/O  Entry points
!     --------  ---  --------------------------------------------------
!     FNAME      I   ZZDSKLSF
!     BODYID     I   ZZDSKBSS
!     HANDLE    I,O  ZZDSKLSF, ZZDSKUPF, ZZDSKSNS
!     USRCTR    I,O  ZZDSKCHK
!     UPDATE     O   ZZDSKCHK
!     DESCR      O   ZZDSKSNS
!     FOUND      O   ZZDSKSNS
!
! $ Detailed_Input
!
!     FNAME      is the name of a binary DSK file to be loaded.
!
!     HANDLE     on input is the handle of a binary DSK file to be
!                unloaded.
!
!
!     The purpose of entry points ZZDSKBSS and ZZDSKSNS is to search for
!     segments in DSK files matching certain criteria.
!
!     USRCTR     on input is the value of a DSK loaded kernel counter
!                maintained by a DSK routine or subsystem.
!
!
! $ Detailed_Output
!
!     HANDLE     on output is the handle of the DSK file
!                containing a located segment.
!
!     USRCTR     on output is the value of a DSK loaded kernel counter
!                maintained by the ZZDSKBSR subsystem.
!
!     UPDATE     is a logical flag indicating whether the state of
!                the loaded DSK set has changed since the ZZDSKBSR
!                state counter was equal to a given user counter.
!
!     DESCR      is the packed descriptor of a located segment.
!
!     FOUND      indicates whether a requested segment was found or not.
!
! $ Parameters
!
!     FTSIZE     is the maximum number of shape files that can
!                be loaded by ZZDSKLSF at any given time for use by the
!                readers.
!
!     BTSIZE     is the maximum number of bodies whose segments
!                are buffered by ZZDSKSNS.
!
!     STSIZE     is the maximum number of segments that can be buffered
!                at any given time by ZZDSKSNS.
!
! $ Exceptions
!
!     1) If ZZDSKBSR is called directly, the error SPICE(DSKBOGUSENTRY)
!        is signaled.
!
!     2) See entry points ZZDSKLSF, ZZDSKUPF, ZZDSKBSS, and ZZDSKSNS
!        for exceptions specific to them.
!
! $ Files
!
!     DSK shape files are indicated by filename before loading
!     (see ZZDSKLSF) and handle after loading (all other places).
!
! $ Particulars
!
!     ZZDSKBSR serves as an umbrella, allowing data to be shared by its
!     entry points:
!
!        ZZDSKLSF       Load shape file.
!        ZZDSKUPF       Unload shape file.
!        ZZDSKBSS       Begin search for segment.
!        ZZDSKSNS       Select next segment.
!        ZZDSKCHK       Check for change in loaded kernel set.
!
!     Before a file can be read by the DSK readers, it must be
!     loaded by ZZDSKLSF, which among other things load the file into
!     the DAS subsystem.
!
!     Up to FTSIZE files may be loaded for use simultaneously, and a
!     file only has to be loaded once to become a potential search
!     target for any number of subsequent reads.
!
!     Once a DSK has been loaded, it is assigned a file
!     handle, which is used to keep track of the file internally, and
!     which is used by the calling program to refer to the file in all
!     subsequent calls to DSK routines.
!
!     A file may be removed from the list of files for potential
!     searching by unloading it via a call to ZZDSKUPF.
!
!     ZZDSKBSS and ZZDSKSNS are used together to search through loaded
!     files for segments.
!
!     ZZDSKBSS sets up the search.
!
!     ZZDSKSNS finds segments matching the search criteria set up by
!     ZZDSKBSS. Last-loaded files get searched first, and individual
!     files are searched backwards.
!
!     When an applicable segment is found, ZZDSKSNS returns that
!     segment's descriptor and identifier, along with the handle of the
!     file containing the segment.
!
!     Subsequent calls to ZZDSKSNS continue the search, picking up
!     where the previous call to this routine left off.
!
!     ZZDSKSNS uses information on loaded files to manage a buffer of
!     saved segment descriptors and identifiers. The buffer is used to
!     speed up access time by minimizing file reads.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     1) If Fortran I/O errors occur while searching a loaded DSK
!        file, the internal state of this suite of routines may
!        be corrupted.  It may be possible to correct the state
!        by unloading the pertinent DSK files and then re-loading
!        them.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!     R.E. Thurman   (JPL)
!     I.M. Underwood (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 13-OCT-2021 (NJB) (BVS)
!
!        Updated entry point ZZDSKSNS to always initialize FOUND.
!
!        Corrected typo in long error message in entry point ZZDSKBSS.
!        Corrected comments in ZZDSKBSS and in ZZDSKSNS
!
!-    SPICELIB Version 1.0.0, 08-FEB-2017 (NJB)
!
!        Updated version info.
!
!        31-MAR-2016 (NJB)
!
!           Deleted unused variable references.
!           Added header for ZZDSKCHK.
!
!           Cleaned up in-line comments.
!
!        19-SEP-2014 (NJB)
!
!           Specification change: now treats the target body as the
!           primary search key. Segments lists are associated with
!           bodies rather than surfaces.
!
!           The input argument SURFID has been replaced with BODYID.
!
!           A counter system has been implemented to enable SPICE
!           routines to detect changes in the state of the loaded
!           kernel set. The user interface entry point for checking for
!           state updates in ZZDSKCHK.
!
!        21-MAY-2013 (NJB)
!
!           Removed debugging WRITE statement from entry point
!           ZZDSKSNS. Edited headers to remove long lines.
!
!        02-APR-2010 (NJB) (RET) (IMU)
!
!           Original version.
!
!-&

! $ Index_Entries
!
!     buffer dsk segments for readers
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
      integer(ip)           intmax
      integer(ip)           isrchi
      integer(ip)           lnknfn
      integer(ip)           lnknxt
      integer(ip)           lnkprv

      logical               failed
      logical               return

!
!     Local parameters


      integer(ip)           slen
      parameter           ( slen   = 40 )

!
!     Constants used in the doubly linked list structure:
!
      integer(ip)           forwrd
      parameter           ( forwrd  = 1 )

!
!     Local variables
!

!
!     DSK loaded kernel set state change counter:
!
      integer(ip)           dskctr ( ctrsiz )

!
!     The file table contains the handle and file number of each file
!     that has been loaded for use with the DSK readers. File
!     numbers begin at one, and are incremented until they reach a
!     value of INTMAX() - 1, at which point they are mapped to the
!     range 1:NFT, where NFT is the number of loaded DSK files.
!
!     A file number is similar to a file handle, but it is assigned
!     and used exclusively by this module. The purpose of file numbers
!     is to keep track of the order in which files are loaded and the
!     order in which they are searched.
!
!     All names begin with FT.
!
!        HAN      Handle
!        NUM      File number
!
!     NFT is the number of currently loaded DSK files. NEXT is
!     incremented whenever a new file is loaded to give the file
!     number for that file. FINDEX is the index of whatever file is
!     of current interest.
!
!     New files are added at the end of the table. As files are
!     removed, succeeding files are moved forward to take up the
!     slack. This keeps the table ordered by file number.
!
      integer(ip)           nft
      integer(ip)           fthan    ( ftsize )
      integer(ip)           ftnum    ( ftsize )
      integer(ip)           next
      integer(ip)           findex

!
!     The body table contains the beginning of the list of the stored
!     segments for each body and the expense at which that list was
!     constructed. (The expense of a body list is the number of segment
!     descriptors examined during the construction of the list.) It
!     also contains the highest and lowest file numbers searched during
!     the construction of the list.
!
!     All names begin with BT.
!
!        BOD      Body ID code
!        EXP      Expense
!        HFS      Highest file (number) searched
!        LFS      Lowest  file (number) searched
!        BEG      Beginning of segment list
!
!     NBT is the number of bodies for which segments are currently
!     being stored in the table. BINDEX is the index of whatever
!     body is of current interest at any given time.
!
!     New bodies are added at the end of the table. As bodies
!     are removed, the last body is moved forward to take up the
!     slack. This keeps the entries in the table contiguous.
!
      integer(ip)           bindex
      integer(ip)           btbeg    ( btsize )
      integer(ip)           btexp    ( btsize )
      integer(ip)           bthfs    ( btsize )
      integer(ip)           btbod    ( btsize )
      integer(ip)           btlfs    ( btsize )
      integer(ip)           nbt

!
!     The segment table contains the handle, descriptor, and identifier
!     for each segment that has been found so far.
!
!     The segment table is implemented as a set of arrays indexed by
!     a SPICE doubly linked list structure.  For each body
!     in the body table, there is a segment table list; each
!     node of a list points to data associated with a segment.  In
!     each list, the head node corresponds to the highest-priority
!     segment in that list, and segment priority decreases in the
!     forward direction.
!
!     All names begin with ST.
!
!        DLAD     DLA segment descriptor
!        DSKD     DSK segment descriptor
!        HAN      Handle
!        POOL     Doubly linked list pool.
!
!     New segments are added to the front or end of an body list
!     as appropriate, according to the rules spelled out under
!     entry point ZZDSKSNS.
!

      real(wp)              stdskd  ( dskdsz,     stsize )

      integer(ip)           sthan   (             stsize )
      integer(ip)           stdlad  ( dladsz,     stsize )
      integer(ip)           stpool ( 2,  lbpool : stsize )

!
!     Other local variables
!
      character(slen)       doing
      character(slen)       stack    ( 2 )
      character(slen)       status
      character(slen)       urgent

      real(wp)              dsklds ( dskdsz )


      integer(ip)           cheap
      integer(ip)           cost
      integer(ip)           dlalds ( dladsz )
      integer(ip)           dlanxt ( dladsz )
      integer(ip)           dlaprv ( dladsz )
      integer(ip)           head
      integer(ip)           i
      integer(ip)           j
      integer(ip)           minexp
      integer(ip)           new
      integer(ip)           node
      integer(ip)           nxtseg
      integer(ip)           p
      integer(ip)           prvnod
      integer(ip)           savep
      integer(ip)           svbody
      integer(ip)           tail
      integer(ip)           top
      logical               begsch
      logical               fnd
      logical               pass1

!
!     Saved variables
!
      save                  begsch
      save                  findex
      save                  fnd
      save                  fthan
      save                  ftnum
      save                  bindex
      save                  btbeg
      save                  btexp
      save                  bthfs
      save                  btbod
      save                  btlfs
      save                  dskctr
      save                  next
      save                  nft
      save                  nbt
      save                  pass1
      save                  savep
      save                  svbody
      save                  status
      save                  stdlad
      save                  stdskd
      save                  sthan
      save                  stpool
      save                  top

!
!     Initial values
!
      data                  nft       / 0             /
      data                  nbt       / 0             /
      data                  next      / 0             /
      data                  pass1     / .true.        /
      data                  savep     / 0             /
      data                  status    / 'BOGUS ENTRY' /


!
!     Nobody has any business calling ZZDSKBSR directly.
!
      if ( return () ) return

      call sigerr ( 'SPICE(DSKBOGUSENTRY)' )

      return


!$Procedure ZZDSKLSF ( DSK, load shape file )

      entry zzdsklsf ( fname, handle )

! $ Abstract
!
!     Load a DSK shape file for use by the DSK readers.  Return that
!     file's handle, to be used by other DSK routines to refer to the
!     file.
!
! $ Required_Reading
!
!     DSK
!     DAS
!
! $ Keywords
!
!     TOPOGRAPHY
!
! $ Declarations
!
!     CHARACTER*(*)         FNAME
!     INTEGER               HANDLE
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     FNAME      I   Name of the DSK file to be loaded.
!     HANDLE     O   Loaded file's handle.
!     FTSIZE     P   Maximum number of loaded DSK files.
!
! $ Detailed_Input
!
!     FNAME      is the name of a DSK file to be loaded.
!
! $ Detailed_Output
!
!     HANDLE     is an integer handle assigned to the file upon loading.
!                Almost every other DSK routine will subsequently use
!                this number to refer to the file.
!
! $ Parameters
!
!     FTSIZE     is the maximum number of DSK files that may
!                be loaded simultaneously under any circumstances.
!                FTSIZE is currently set to match the maximum number
!                of DAS files that may be loaded simultaneously.
!
! $ Exceptions
!
!     1) If an attempt is made to open more DAS files than is specified
!        by the parameter FTSIZE in DASAH, an error is signaled by a
!        routine in the call tree of this routine.
!
!     2) If an attempt is made to load more files than is specified
!        by the local parameter FTSIZE, and if the DAS system has
!        room to load another file, the error SPICE(DSKTOOMANYFILES)
!        signaled. The current setting of FTSIZE does not allow this
!        situation to arise:  the DAS system will trap the error
!        before this routine has the chance.
!
!     3) If the file specified by FNAME can not be opened, an error
!        is signaled by a routine that this routine calls.
!
!     4) If the file specified by FNAME has already been loaded,
!        it will become the "last-loaded" file.  The readers
!        search the last-loaded file first.
!
! $ Files
!
!     The DSK file specified by FNAME is loaded. The file is assigned
!     an integer handle by ZZDSKLSF. Other DSK routines will refer to
!     this file by its handle.
!
! $ Particulars
!
!     See Particulars above, in ZZDSKBSR.
!
!     If there is room for a new file, ZZDSKLSF opens the file for
!     reading.
!
!     DSK readers search files loaded with ZZDSKLSF in the reverse order
!     in which they were loaded.  That is, last-loaded files are
!     searched first.
!
! $ Examples
!
!     See the Example above, in ZZDSKBSR.
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
!     R.E. Thurman   (JPL)
!     I.M. Underwood (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 08-FEB-2017 (NJB)
!
!        Updated version info.
!
!        19-SEP-2014 (NJB)
!
!           Specification change: now treats the target body as the
!           primary search key. Segments lists are associated with
!           bodies rather than surfaces.
!
!           Now initializes the DSK state change counter on the first
!           pass and updates it on every subsequent call.

!       21-MAY-2013 (NJB)
!
!           Edited headers to remove long lines.
!
!       02-APR-2010 (NJB) (RET) (IMU)
!
!-&

! $ Index_Entries
!
!     load dsk shape file
!
!-&


! $ Revisions
!
!     None.
!-&

!
!     Standard SPICE error handling.
!
      if ( return () ) return


      if ( pass1 ) then
!
!        Initialize the BSR counter.
!
         call zzctrsin ( dskctr )

         pass1 = .false.

      end if

!
!     Increment the BSR counter regardless of whether
!     the load operation is successful.
!
      call zzctrinc ( dskctr )

!
!     Don't allow a search to continue after loading a file; a new
!     search should be re-started.
!
      status = 'BOGUS ENTRY'

!
!     Nothing works unless at least one file has been loaded, so
!     this is as good a place as any to initialize the free list
!     whenever the body table is empty.
!
      if ( nbt == 0 ) then
         call lnkini ( stsize, stpool )
      end if

!
!     To load a new file, first try to open it for reading.
!
      call dasopr ( fname, handle )

      if ( failed() ) return

!
!     Determine if the file is already in the table.
!
      findex = isrchi ( handle, nft, fthan )

      if ( findex > 0 ) then
!
!        The last call we made to DASOPR added another DAS link to
!        the DSK file.  Remove this link.
!
         call dascls ( handle )

!
!        Handle is already in the table.  Remove it.
!
         nft = nft - 1

         do i = findex, nft
            fthan( i ) = fthan( i + 1 )
            ftnum( i ) = ftnum( i + 1 )
         end do

!
!        Unlink any segments that came from this file.
!
         i = 1

         do while ( i <= nbt )

            p = btbeg( i )

            do while ( p > 0 )
!
!              Find the successor of P, if any.
!
               nxtseg = lnknxt ( p, stpool )

               if ( sthan( p ) == handle ) then
!
!                 The segment corresponding to node P came from
!                 the file we're unloading.  Delete the node for
!                 P from the segment list for body I; if P happens
!                 to be the head node for body I's segment list,
!                 make the successor of P the head of the list.
!
                  call lnkfsl ( p, p, stpool )

                  if ( p == btbeg(i) ) then
                     btbeg( i ) = nxtseg
                  end if

               end if
!
!              Update P.
!
               p = nxtseg

            end do

!
!           If the list for this body is now empty, shorten the
!           current table by one: put all the entries for the last
!           body in the table into the space occupied by the
!           one we've deleted.
!
            if ( btbeg( i ) <= 0 ) then

               btbod( i ) = btbod( nbt )
               btexp( i ) = btexp( nbt )
               bthfs( i ) = bthfs( nbt )
               btlfs( i ) = btlfs( nbt )
               btbeg( i ) = btbeg( nbt )

               nbt = nbt - 1

            else

               i = i + 1

            end if

         end do

      else
!
!        This is a new file.  Make sure that there are unused slots
!        in the file table.
!
         if ( nft == ftsize ) then

            call dascls ( handle )

            call setmsg ( 'Number of files loaded is at a maximum, ' // &
                          'as specified by the parameter FTSIZE, '   // &
                          'the value of which is #. You will need '  // &
                          'to load fewer files. Consider unloading ' // &
                          'any files that are not needed.'            )
            call errint ( '#', ftsize                                 )
            call sigerr ( 'SPICE(DSKTOOMANYFILES)'                    )
            return

         end if

      end if

!
!     Determine the next file number.
!
!     Programmer's note: this section is normally not reached.
!     It should be tested by temporarily setting the comparison
!     value to a smaller number, for example 2*FTSIZE.
!
      if ( next < intmax()-1 ) then

         next = next + 1

      else
!
!        The user is to be congratulated:  we've run out of file
!        numbers.
!
!        Re-set the valid file numbers so they lie in the range 1:NFT,
!        with the Ith file in the file table having file number I.
!        First update the LFS and HFS components of the body table
!        according to this mapping.
!
!        Set any body table entries that are lower than FTNUM(1)
!        to zero.
!
         do i = 1, nbt
!
!           Re-map the HFS table for the Ith body.
!
            j = isrchi ( bthfs(i), nft, ftnum )

!
!           Either the highest file searched for body I is the Jth
!           file in the file table, or the file is not in the table.
!           In both cases, J is the correct value to assign to
!           BTHFS(I).
!
            bthfs(i) = j

!
!           When the highest file searched for body I is not in the
!           file table, the highest file searched has been unloaded.
!           Note that this assignment makes all files appear to be
!           "new" when a lookup for body I is performed.
!
!           Re-map the LFS table for the Ith body.
!
            j = isrchi ( btlfs(i), nft, ftnum )

            if ( j > 0 ) then
!
!              The lowest file searched for body I is the Jth file
!              in the file table.
!
               btlfs(i) = j

            else
!
!              The lowest file searched for body I is not in the
!              file table.  This occurs when the lowest file searched
!              has been unloaded.  Zero out both the lowest and
!              highest file searched to force reconstruction of the
!              list.
!
               btlfs(i) = 0
               bthfs(i) = 0

            end if

         end do

!
!        Re-map the file number table itself.
!
         do i = 1, nft

            ftnum(i) = i

         end do

!
!        Assign a new file number.
!
         next = nft + 1

      end if

!
!     Now add this file to file table.
!
      nft        = nft  + 1
      fthan(nft) = handle
      ftnum(nft) = next

      return




!$Procedure ZZDSKUSF ( DSK, Unload shape file )

      entry zzdskusf ( handle )

! $ Abstract
!
!     Unload a DSK shape file so that it will no longer be searched
!     by the readers.
!
! $ Required_Reading
!
!     DSK
!     DAS
!
! $ Keywords
!
!     TOPOGRAPHY
!
! $ Declarations
!
!     INTEGER               HANDLE
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of DSK file to be unloaded
!
! $ Detailed_Input
!
!     HANDLE     Integer handle assigned to the file upon loading.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) Unloading a file that has not been loaded is a no-op.
!        No error is signaled.
!
! $ Files
!
!     The file referred to by HANDLE is unloaded.
!
! $ Particulars
!
!     See Particulars section above, in ZZDSKBSR.
!
!     Unloading a file with ZZDSKUSF removes that file from
!     consideration by the DSK readers. In doing so, it frees up space
!     for another file to be loaded.
!
! $ Examples
!
!     See the Example above, in ZZDSKBSR.
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
!     R.E. Thurman   (JPL)
!     I.M. Underwood (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 08-FEB-2017 (NJB)
!
!        Updated version info.
!
!        19-SEP-2014 (NJB)
!
!           Specification change: now treats the target body as the
!           primary search key. Segments lists are associated with
!           bodies rather than surfaces.
!
!           Now initializes the DSK state change counter on the first
!           pass and updates it on every subsequent call.

!        21-MAY-2013 (NJB)
!
!           Edited headers to remove long lines.
!           Removed debugging output.
!
!        02-APR-2010 (NJB) (RET) (IMU)
!
!-&

! $ Index_Entries
!
!     unload ck shape file
!
!-&



! $ Revisions
!
!     None.
!
!-&
      if ( return() ) return


      if ( pass1 ) then
!
!        Initialize the BSR counter.
!
         call zzctrsin ( dskctr )

         pass1 = .false.

      end if

!
!     Increment the BSR counter regardless of whether
!     the load operation is successful.
!
      call zzctrinc ( dskctr )


!
!     Don't allow a search to continue after unloading a file; a new
!     search should be re-started.
!
      status = 'BOGUS ENTRY'

!
!     All of the stored segments from the file must be removed
!     from the segment table (by returning the corresponding nodes
!     to the segment table pool.)
!
!     Don't do anything if the given handle is not in the file table.
!
      findex = isrchi ( handle, nft, fthan )

      if ( findex == 0 ) return
!
!
!     First get rid of the entry in the file table. Close the file
!     before wiping out the handle.
!
      call dascls ( fthan(findex) )


      nft = nft - 1

      do i = findex, nft
         fthan( i ) = fthan( i + 1 )
         ftnum( i ) = ftnum( i + 1 )
      end do

!
!     Check each body list individually. Note that the first
!     node on each list, having no predecessor, must be handled
!     specially.
!
      i = 1

      do while ( i <= nbt )

         p = btbeg( i )

         do while ( p > 0 )

            nxtseg = lnknxt ( p, stpool )

            if ( sthan(p) == handle ) then

               if ( p == btbeg(i) ) then
                  btbeg( i ) = nxtseg
               end if
!
!              Free this segment table entry.
!
               call lnkfsl ( p, p, stpool )

            end if

            p = nxtseg

         end do

!
!        If the list for this body is now empty, shorten the
!        current table by one: put all the entries for the last
!        body in the table into the space occupied by the
!        one we've deleted.
!
         if ( btbeg(i) <= 0 ) then

            if ( i /= nbt ) then

               btbod (i) = btbod (nbt)
               btexp (i) = btexp (nbt)
               bthfs (i) = bthfs (nbt)
               btlfs (i) = btlfs (nbt)
               btbeg (i) = btbeg (nbt)

            end if

            nbt = nbt - 1

         else

            i = i + 1

         end if

      end do


      return




!$Procedure ZZDSKBSS ( DSK, begin search for segment )

      entry zzdskbss ( bodyid )

! $ Abstract
!
!     Initiate search through loaded files to find segments
!     satisfying search criteria.
!
! $ Required_Reading
!
!     DSK
!     DAS
!
! $ Keywords
!
!     TOPOGRAPHY
!
! $ Declarations
!
!     INTEGER               BODYID
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     BODYID     I   ID code of body.
!
! $ Detailed_Input
!
!     BODYID     is the integer ID code of the body for which a search
!                is to be performed.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If no files have been loaded, the error
!         SPICE(NOLOADEDDSKFILES) is signaled.
!
! $ Files
!
!     All files loaded by ZZDSKLSF are potential search targets for
!     ZZDSKSNS.
!
! $ Particulars
!
!     ZZDSKBSS sets up a search for segments by ZZDSKSNS. It records
!     the body for which the search is to be conducted.
!
!     ZZDSKBSS determines the first task that ZZDSKSNS will have to
!     perform if it is called to get an applicable segment.
!
! $ Examples
!
!     See Examples in ZZDSKBSR.
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
!     R.E. Thurman   (JPL)
!     I.M. Underwood (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 23-NOV-2020 (NJB) (BVS)
!
!        Corrected typo in long error message. Corrected descriptions
!        of inputs. Corrected description of saved information in the
!        Particulars header section.
!
!-    SPICELIB Version 1.0.0, 08-FEB-2017 (NJB)
!
!        Updated version info.
!
!        19-SEP-2014 (NJB)
!
!           Specification change: now treats the target body as the
!           primary search key. Segments lists are associated with
!           bodies rather than surfaces.
!
!           Now initializes the DSK state change counter on the first
!           pass.
!
!        21-MAY-2013 (NJB)
!
!           Edited headers to remove long lines.
!
!        02-APR-2010 (NJB) (RET) (IMU)
!
!
!-&

! $ Index_Entries
!
!     begin search for dsk segment
!
!-&



! $ Revisions
!
!     None.
!
!-&

!
!     Standard SPICE error handling.
!
      if ( return () ) return



      if ( pass1 ) then
!
!        Initialize the BSR counter.
!
         call zzctrsin ( dskctr )

         pass1 = .false.

      end if


!
!     Make a saved copy of the body ID code.
!
      svbody = bodyid

!
!     There must be at least one file loaded.
!
      if ( nft == 0 ) then

         call setmsg ( 'At least one DSK file must have been loaded ' &
         //            'by ZZDSKLSF before a search can be started.' )
         call sigerr ( 'SPICE(NOLOADEDDSKFILES)'                     )
         return

      end if

!
!     The stack of suspended tasks is empty.
!
      top = 0

!
!     Is the body already in the body table?  The answer
!     determines what the first task for ZZDSKSNS will be.
!
      bindex = isrchi ( svbody, nbt, btbod )


      if ( bindex == 0 ) then

         status = 'NEW BODY'

      else
!
!        Set the status so that ZZDSKSNS will determine whether to check
!        the segment list or search new files.
!
         status = '?'

      end if

!
!     The saved segment list pointer is no longer valid.
!
      savep  = -1

      return



!$Procedure ZZDSKSNS ( DSK, Select next segment )

      entry zzdsksns ( cmpfun, handle, dladsc, dskdsc, found )

! $ Abstract
!
!     Search through loaded files to find a segment matching the
!     requested body, time, and need for angular velocity.
!     Buffer segment descriptors, identifiers, and handles in the
!     process to minimize file reads.
!
! $ Required_Reading
!
!     DSK
!     DAS
!
! $ Keywords
!
!     TOPOGRAPHY
!
! $ Declarations
!
!     LOGICAL               CMPFUN
!     EXTERNAL              CMPFUN
!     INTEGER               HANDLE
!     INTEGER               DLADSC ( * )
!     DOUBLE PRECISION      DSKDSC ( * )
!     LOGICAL               FOUND
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     CMPFUN     I   Comparison function.
!     HANDLE     O   Handle of file containing the applicable segment.
!     DLSDSC     O   DLA descriptor of the applicable segment.
!     DSKDSC     O   DSK descriptor of the applicable segment.
!     FOUND      O   True if a segment was found.
!
! $ Detailed_Input
!
!     CMPFUN
!
! $ Detailed_Output
!
!     DLADSC
!
!     DSKDS
!
!
!     HANDLE     is an integer handle of the file containing the
!                segment matching the body and time
!                specifications made in the last call to ZZDSKBSS.
!
!     FOUND      is true if an applicable segment was found.  False
!                otherwise.  If FOUND is false, the values of the
!                other arguments are meaningless.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If ZZDSKSNS is called without ZZDSKBSS ever having been
!        called, the error 'SPICE(CALLZZDSKBSSFIRST)' is signaled.
!
!     2) If no segment is found that matches the search criteria,
!        FOUND is set to false, but the values of HANDLE, DESCR,
!        and SEGID will be meaningless.
!
! $ Files
!
!     All files loaded by ZZDSKLSF are potential search targets for
!     ZZDSKSNS. The files are all referred to by their integer handles.
!
! $ Particulars
!
!     ZZDSKSNS is used to locate segments based on the search criteria
!     established by the most recent call to ZZDSKBSS.  When a segment
!     is found it will have the following characteristics:
!
!        1) Its body will match the body specified in the
!           call to ZZDSKBSS.
!
!        2) Its time interval will intersect the time interval
!
!              [SCLKDP - TOL, SCLKDP + TOL],
!
!           where SCLKDP and TOL were specified in the call to ZZDSKBSS.
!
!        3) If there is a need for angular velocity data, as specified
!           by NEEDAV in the call to ZZDSKBSS, a returned segment
!           will contain angular velocity data. If there is no need
!           for such data, the returned segment may or may not contain
!           angular velocity data.
!
!     The first call to ZZDSKSNS following a call to ZZDSKBSS starts a
!     search through loaded files and either returns the first
!     applicable segment, or indicates that no segment was found.
!
!     ZZDSKSNS searches through last-loaded files first. Individual
!     files are searched backwards, so that segments that were inserted
!     last into the file get checked first.
!
!     Subsequent calls to ZZDSKSNS pick up the search exactly where the
!     previous calls left off. If a segment is not found, future calls
!     will also indicate that no segment could be found, until a new
!     search is begun.
!
!     ZZDSKSNS also buffers segment descriptors and identifiers, to
!     attempt to minimize file reads.
!
! $ Examples
!
!     See Examples in ZZDSKBSR.
!
! $ Restrictions
!
!     1) This subroutine assumes that a search has been initiated by
!        a call to ZZDSKBSS.
!
!     2) When a DSK file is loaded or unloaded, a new search must
!        be started via a call to ZZDSKBSS before this routine may
!        be called.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!     R.E. Thurman   (JPL)
!     I.M. Underwood (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 13-OCT-2021 (NJB)
!
!        Relocated initialization of FOUND so it is always
!        executed, even if an error state is indicated by RETURN().
!
!        Corrected inline comments describing segment search criteria.
!
!-    SPICELIB Version 1.0.0, 08-FEB-2017 (NJB)
!
!        Updated version info.
!
!        31-MAR-2016 (NJB)
!
!           References to unneeded variables were removed.
!
!        19-SEP-2014 (NJB)
!
!           Specification change: now treats the target body as the
!           primary search key. Segment lists are associated with
!           bodies rather than surfaces.
!
!           Now initializes the DSK state change counter on the first
!           pass.
!
!        21-MAY-2013 (NJB)
!
!            Removed debugging WRITE statement. Edited headers to
!            remove long lines.
!
!        02-APR-2010 (NJB) (RET) (IMU)
!
!-&

! $ Index_Entries
!
!     select next dsk segment
!
!-&


! $ Revisions
!
!     None.
!
!-&

!
!     Nothing's been found yet.
!
      found = .false.

!
!     Standard SPICE error handling.
!
      if ( return () ) return



      if ( pass1 ) then
!
!        Initialize the BSR counter.
!
         call zzctrsin ( dskctr )

         pass1 = .false.

      end if

!
!     Initialize the segment list pointer to the saved value from
!     the previous pass through this routine, if any.
!
      p = savep

!
!     ZZDSKSNS buffers segment descriptors and identifiers, to
!     attempt to minimize file reads. Buffering segments involves
!     maintaining three tables:  the file table, the body table,
!     and the segment table. ZZDSKSNS is broken down into various tasks,
!     described in the code below, which perform these manipulations.
!
!     A description of the components of each table is provided in
!     the declarations section of ZZDSKBSR.
!
!     Basically, the buffering is performed as follows: once a request
!     for a segment for a particular body is made, if there are
!     no adequate entries in the buffer already, a search is made
!     through loaded files for applicable segments.  Every segment
!     pertaining to that body in a searched file is buffered,
!     before a check of the current buffer is made.  If the search
!     doesn't turn up a segment matching the specified search criteria
!     the next file is searched and new segments are added to the list,
!     and so on.
!
!     The information in the segment table (ST) is stored in a
!     doubly-linked list. Each node in the list contains several
!     individual pieces of data, which are stored in parallel
!     arrays.
!
!     In the following loop, we will try to simplify things by
!     doing exactly one thing on each pass through the loop.
!     After each pass, the status of the loop (STATUS) will be
!     adjusted to reflect the next thing that needs to be done.
!     The first task is set by ZZDSKBSS.
!
!     Occasionally, the current task will have to be interrupted
!     until another task can be carried out. (For example, when
!     collecting new segments, an interrupt might place a segment
!     at the front or end of the current body list; when placing
!     the segment on the list, a second interrupt might free
!     room in the segment table in order to allow the addition
!     to proceed.) In this case, the current task will be saved and
!     restored after the more urgent task has been completed.
!
!     The loop can terminate in only one of two ways (unless an error
!     occurs). First, if an applicable segment is found in the segment
!     table, the handle, descriptor, and identifier for the segment
!     are returned immediately.  Second, if the table does not contain
!     an applicable segment, and if no files remain to be searched,
!     the loop terminates normally, and no data are returned.
!
!     The status is saved on exit, however, so that subsequent calls
!     will resume a search exactly where previous calls left off.
!
!     Each status is described below.
!
!     'NEW BODY'
!
!        This indicates that the specified body has
!        no segments stored for it at all. It must be added to the
!        body table.  (This is followed immediately by an
!        OLD FILES search, in which every file loaded is considered an
!        old file.)
!
!     'NEW FILES'
!
!        This indicates that at least one new file has been added
!        since the last time the segment list for the specified
!        body was searched. Find the oldest of these new files,
!        and begin a NEW SEGMENTS search in forward order for
!        segments to add to the front of the list.
!
!     'NEW SEGMENTS'
!
!        Continue a NEW FILES search, adding segments for the specified
!        body to the front of the list.
!
!     'OLD FILES'
!
!        This indicates that although the list has been searched
!        and found to contain no applicable segment, some of the
!        older files remain to be searched. Find the newest of these
!        old files, and begin an OLD SEGMENTS search in backward order.
!
!     'OLD SEGMENTS'
!
!        Continue an OLD FILES search, adding segments for the specified
!        body to the end of the list.
!
!     'CHECK LIST'
!
!        This indicates that the list is ready to be searched,
!        either because no new files have been added, or because
!        segments from a new file or an old file have recently
!        been added.
!
!        The list is never checked until all new files have been
!        searched.
!
!        If an applicable segment is found, it is returned.
!
!     'MAKE ROOM' (Interrupt)
!
!        This indicates that one of the bodies must be removed,
!        along with its stored segments, to make room for another
!        body or segment.  The body (other than the
!        specified body) with the smallest expense is selected
!        for this honor.
!
!     'ADD TO FRONT' (Interrupt)
!
!        This indicates that a segment has been found (during the
!        course of a NEW FILES search) and must be added to the front
!        of the list.
!
!     'ADD TO END' (Interrupt)
!
!        This indicates that a segment has been found (during the
!        course of an OLD FILES search) and must be added to the end
!        of the list.
!
!     'SUSPEND'
!
!        This indicates that the current task (DOING) should be
!        interrupted until a more urgent task (URGENT) can be
!        carried out. The current task is placed on a stack for
!        safekeeping.
!
!     'RESUME'
!
!        This indicates that the most recently interrupted task
!        should be resumed immediately.
!
!     '?'
!
!        This indicates that the next task is not immediately
!        apparent: if new files exist, they should be searched;
!        otherwise the list should be checked.
!
!     'HOPELESS'
!
!        This indicates that the table does not contain an applicable
!        segment, and no files remain to be searched.
!
!      'BOGUS ENTRY'
!
!        This is the initial value of STATUS and indicates that no
!        call to ZZDSKBSS was ever made. If this is the case then an
!        error will be signaled.
!

      if ( status == 'BOGUS ENTRY' )  then

         call setmsg ( 'Must begin a search by calling ZZDSKBSS ' &
         //            'first.'                                   )
         call sigerr ( 'SPICE(CALLZZDSKBSSFIRST)'                 )
         return

      end if


      do while ( status /= 'HOPELESS' )
!
!        If new files have been added, they have to be searched.
!        Otherwise, go right to the list of stored segments.
!
         if ( status == '?' ) then
!
!           There are two ways to get to this point.
!
!           1)  Status may have been set to '?' by ZZDSKBSS.
!
!           2)  Status was set to '?' by the NEW SEGMENTS block
!               of code as the result of finishing the read of
!               a new file.
!

            if ( bthfs( bindex ) < ftnum( nft ) ) then

               status = 'NEW FILES'

            else

!              If the segment list for this body is empty, make
!              sure the expense is set to 0.
!
               if ( btbeg(bindex) <= 0 ) then
                  btexp(bindex) = 0
               end if

!
!              Prepare to look at the first segment in the list for
!              this body.
!
               p      =  btbeg( bindex )
               status = 'CHECK LIST'

            end if


         else if ( status == 'NEW BODY' ) then
!
!           New bodies are added to the end of the body
!           table. If the table is full, one of the current occupants
!           must be removed to make room for the new one.
!
!           Setting LFS to one more than the highest current file
!           number means the 'OLD FILES' search that follows will
!           begin with the last-loaded file.
!
!           There is one way to get here:
!
!           1)  The variable STATUS was set to NEW BODY prior
!               in ZZDSKBSS.
!
!           Find the cheapest slot in the body table to store
!           the initial information about this body.
!
!           NOTE:  This used to be handled by the MAKE ROOM section.
!           However, trying to handle this special case there was
!           just more trouble than it was worth.
!
            if ( nbt < btsize ) then
!
!              If the body table isn't full, the cheapest place is
!              just the next unused row of the table.
!
               nbt   = nbt + 1
               cheap = nbt

            else
!
!              The body table is full.  Find the least
!              expensive body in the table and remove it.
!
               cheap  = 1
               minexp = btexp(1)

               do i = 2, nbt

                  if ( btexp(i) < minexp ) then
                     cheap  = i
                     minexp = btexp(i)
                  end if

               end do

!
!              If there are any segments associated with the
!              least expensive body, we put them back on the free
!              list.
!
               head = btbeg(cheap)

               if ( head > 0 ) then

                  tail =  - lnkprv ( head, stpool )
                  call lnkfsl ( head, tail, stpool )

               end if

            end if

!
!           Set up a table entry for the new body.
!
            btbod (cheap) = svbody
            btexp (cheap) = 0
            bthfs (cheap) = ftnum(nft)
            btlfs (cheap) = ftnum(nft) + 1
            btbeg (cheap) = 0
            bindex        = cheap

!
!           Now search all of the files for segments relating to
!           this body.
!
            status = 'OLD FILES'


         else if ( status == 'NEW FILES' ) then
!
!           When new files exist, they should be searched in forward
!           order, beginning with the oldest new file not yet searched.
!           All new files must be searched before the list can be
!           checked, to ensure that the best (newest) segments are
!           being used.
!
!           Begin a forward search, and prepare to look for individual
!           segments from the file.
!
!           The only way to get here is to have STATUS set to
!           the value NEW FILES in the STATUS .EQ. '?' block
!           of the IF structure.
!
!           Find the next file to search; set FINDEX to the
!           corresponding file table entry.

            findex = 1

            do while ( bthfs(bindex) >= ftnum(findex) )

               findex = findex + 1

            end do

            bthfs( bindex ) = ftnum( findex )

!
!           Start a forward search through the current file.
!
            begsch = .true.
            call dlabfs (  fthan( findex ),  dlalds,  fnd  )

            if ( failed() ) return

            status = 'NEW SEGMENTS'

!
!           The cost of the list contributed by the new file is
!           zero so far.
!
            cost   = 0


         else if ( status == 'NEW SEGMENTS' ) then
!
!           New files are searched in forward order. Segments, when
!           found, are inserted at the front of the list.
!
!           Each segment examined, whether applicable or not, adds to
!           the expense of the list.
!
!           The only ways to get here are:
!
!               1) Enter from the NEW FILES block of the IF structure.
!               2) Re-enter from this block if there are more segments
!                  to examine in the current file and the last segment
!                  seen wasn't for the body of interest.
!               3) Enter from the RESUME state after adding a segment
!                  to the front of the list for the current body.
!
            if ( begsch ) then
!
!              We already have a FND value, and if FND is true, a
!              DLA descriptor.
!
               begsch = .false.

            else
!
!              Use the current DLA descriptor to look up the next one.
!
               call dlafns ( fthan(findex), dlalds, dlanxt, fnd )

               if ( fnd ) then
                  call movei  ( dlanxt, dladsz, dlalds )
               end if

            end if

            if ( failed() ) return

            if ( .not. fnd ) then
!
!              We're out of segments in the current file.  Decide
!              whether we need to examine another new file, or
!              whether we're ready to check the list.
!
               status = '?'
               btexp( bindex ) = btexp( bindex ) + cost

            else
!
!              Get the DSK segment descriptor for the current
!              segment.
!
               call dskgd( fthan(findex), dlalds, dsklds )

               if (  nint( dsklds(ctridx) ) == svbody ) then
!
!                 The segment is for the body of interest. Add this
!                 segment to the front of the list.
!
                  doing  = 'NEW SEGMENTS'
                  urgent = 'ADD TO FRONT'
                  status = 'SUSPEND'

               end if

               cost = cost + 1

            end if
!
!           If we haven't reset the status, we'll return for another
!           'NEW SEGMENTS' pass.
!

         else if ( status == 'OLD FILES' ) then
!
!           When old files must be searched (because the segments in
!           the list are inadequate), they should be searched in
!           backward order, beginning with the newest old file not
!           yet searched.  The segment list will be re-checked
!           after each file is searched.  If a match is found,
!           the search terminates, so some old files may not be
!           searched.
!
!           Begin a backwards search, and prepare to look for
!           individual segments from the file.
!
!           You can get to this block in two ways.
!
!           1) We can have a NEW BODY.
!
!           2) We have checked the current list (CHECK LIST) for
!              this body, didn't find an applicable segment and
!              have some files left that have not been searched.

            findex = nft

            do while ( btlfs( bindex ) <= ftnum( findex ) )
               findex = findex - 1
            end do

            begsch = .true.
            call dlabbs (  fthan( findex ),  dlalds,  fnd )

            if ( failed() ) return

            status = 'OLD SEGMENTS'

!
!           The next thing we'll do is search through all the segments
!           of this file for those that applicable to this body.
!           The cost of the list contributed by the current file is
!           zero so far.
!
            cost = 0

!
!        Old files are searched in backward order. Segments, when
!        found, are inserted at the end of the list.
!
!        Each segment examined, whether applicable or not, adds to
!        the expense of the list.
!
         else if ( status == 'OLD SEGMENTS' ) then
!
!           There is only one way to get here---from the
!           block 'OLD FILES'.  Note we do not add to the
!           expense of the list for this body until we've
!           completely searched this file.
!
            if ( begsch ) then
!
!              We already have a value of FND, and if FND is true,
!              a DLA segment from the current file.
!
               begsch = .false.

            else
!
!              Look up the previous segment from this file.
!
               call dlafps ( fthan(findex), dlalds, dlaprv, fnd )

               if ( fnd ) then

                  call movei ( dlaprv, dladsz, dlalds )

               end if

            end if


            if ( failed() ) return

            if ( .not. fnd ) then
!
!              All of the segments in this file have been exhausted.
!              Change the lowest file searched indicator for this
!              body to be the current file, and go check the
!              current list.
!
               btlfs( bindex ) =  ftnum( findex )
               btexp( bindex ) =  btexp( bindex ) + cost
               p               =  btbeg( bindex )
               status          = 'CHECK LIST'

            else
!
!              Get the DSK descriptor for this segment.
!
               call dskgd ( fthan(findex), dlalds, dsklds )

               if ( failed() ) return

               if (  nint( dsklds(ctridx) )  ==  svbody  ) then
!
!                 This is a segment for the body of interest.
!
                  doing  = 'OLD SEGMENTS'
                  urgent = 'ADD TO END'
                  status = 'SUSPEND'

               end if

               cost = cost + 1

            end if

         else if ( status == 'CHECK LIST' ) then
!
!           Okay, all the new files (and maybe an old file or two)
!           have been searched. Time to look at the list of segments
!           stored for the body, to see if there is one that satisfies
!           the search criteria.
!
!           If so, return it.  If not, try another old file.  If there
!           are no more old files, give up the ghost.
!
!           There are two ways to get to this point.
!
!           1) From the '?' block.
!           2) From the 'OLD SEGMENTS' block.
!
!           For every segment examined, adjust the re-use interval
!           associated with the current body.
!
!           P always points to the current segment in the list. Reject
!           a segment if there is a need for angular velocity data and
!           the segment doesn't have it.
!
!
            do while ( p > 0 )

               if ( cmpfun( sthan(p), stdlad(1,p), stdskd(1,p) )  ) then

                  call movei ( stdlad(1,p), dladsz, dladsc )

                  call moved ( stdskd(1,p), dskdsz, dskdsc )

                  handle =  sthan ( p )
                  found  = .true.
!
!                 Go ahead and move the pointer up before returning
!                 so that the search for the next applicable segment
!                 will start at the right place.
!
                  savep = stpool ( forwrd, p )

                  return

               end if


!
!              Get the next node.  We avoid LNKNXT here in order
!              to speed up the operation.
!
               p = stpool ( forwrd, p )

            end do

!
!           If we're still here we didn't have information for this
!           body in the segment list.
!
!           If there are more files, search them.
!           Otherwise, things are hopeless, set the status that way.
!
            if ( btlfs( bindex ) > ftnum( 1 ) ) then
               status = 'OLD FILES'
            else
               status = 'HOPELESS'
            end if


         else if ( status == 'MAKE ROOM' ) then
!
!           When adding a new segment to a full table, one of the
!           current bodies must be dropped.  The ideal
!           candidate is the one whose list was constructed at the
!           lowest expense.  The candidate should be removed from
!           the body table, and its list transferred to the
!           segment table pool.
!
!           There is ``room'' if the segment table pool contains at
!           least one free node.
!
!           It is possible that a single body requires more than the
!           entire segment table for its own segments. Two things might
!           happen in such a case:
!
!              1) If the list under consideration was being added to at
!                 the end, then a search is continued without buffering
!                 any segments.
!
!              2) If the list was being added to at the beginning, then
!                 that means there was a NEW FILES search going on, and
!                 so a brand new list is constructed for the body,
!                 much as in a 'NEW BODY' task.
!
!           There are two different ways to get to this point.
!
!              1) From 'ADD TO FRONT' if the segment table pool is full.
!              2) From 'ADD TO END' if the segment table pool is full.
!
!           Try to make room by deleting a segment list.  CHEAP will
!           be the index of the "cheapest" segment list in the
!           body table.
!
            minexp = intmax()
            cheap  = 0



            do i = 1, nbt

               if ( i /= bindex ) then

                  if (      ( btexp(i) < minexp   ) &
                       .or. ( cheap    == 0        )  )then
!
!                    This list is the cheapest seen so far,
!                    possibly because it's the first one
!                    considered.  At the moment, it's as good
!                    a candidate for removal as any.
!
                     cheap  = i
                     minexp = btexp(i)

                  end if

               end if

            end do


            if ( cheap == 0 ) then
!
!              If there are no deletable segments, the Thing To
!              Do depends on the task that was suspended before
!              entering MAKE ROOM.
!
               if ( stack(top) == 'ADD TO END' ) then
!
!                 The segment meta-data from the current file cannot
!                 be buffered.
!
!                 In the corresponding SPK and CK cases, we would
!                 search the partial list of segments from this file,
!                 then proceed to search the rest of the file and any
!                 other old files. In this case, we don't support
!                 searching unbuffered segments, so this is the
!                 end of the line.
!
!                 We must clean up the segment list for the current
!                 body before we signal an error. All segments from the
!                 file we're currently searching must be deleted from
!                 the list. If we delete the head node of the list, the
!                 body table pointer to the list must be updated. If
!                 the segment list becomes empty, the body must be
!                 deleted from the body table.
!
                  head =   btbeg ( bindex )
                  tail = - lnkprv( head, stpool )

                  node =   tail

                  do while ( node > 0 )
!
!                    Let PRVNOD be the predecessor of NODE. PRVNOD may
!                    be negative (actually, a pointer to the list tail).
!
                     prvnod = lnkprv( node, stpool )

                     if ( sthan(node) == fthan(findex) ) then
!
!                       This segment is from the file we were
!                       searching when we ran out of room. Free
!                       the segment list entry at index NODE.

                        call lnkfsl ( node, node, stpool )

                        if ( node == head ) then
!
!                          We just deleted the last remaining node in
!                          the list for the current body. We can delete
!                          this body from the body table. However, the
!                          body table contains no other bodies at this
!                          point, since we would have deleted them in
!                          the attempt to make room for the current
!                          body. So we don't need to compress the body
!                          table; we just indicate that it's empty.
!
                           nbt = 0

                        end if
!
!                       This is the end of the block that handles the
!                       head node case.
!
                     end if
!
!                    This is the end of the block that handles the
!                    matching file case.
!
!                    Process the previous node. If the node is
!                    non-positive, the loop will terminate.
!
                     node = prvnod

                  end do
!
!                 The segment table entries for the current body that
!                 are associated with the current file have been
!                 deleted.
!
!                 Make sure that a new search is started before this
!                 routine is called again.
!
                  status = 'HOPELESS'
                  top    = 0
!
!                 It's finally time to signal the error.
!
                  call setmsg ( 'ZZDSKSNS ran out of segment table ' &
                  //            'room while trying to append to the ' &
                  //            'tail of the segment list for body ' &
                  //            '#. Current state is ADD TO END.'    )
                  call errint ( '#', svbody                          )
                  call sigerr ( 'SPICE(BUFFEROVERFLOW)'              )
                  return

               else
!
!                 STACK(TOP) is set to 'ADD TO FRONT'.
!
!                 If there is no room left in the table in the middle
!                 of an attempt to add to the front of the list, just
!                 start from scratch by effectively initiating a 'NEW
!                 BODY' task.
!
!                 Return the current list to the segment table pool.
!                 Note this list is non-empty.
!
                  p    =   btbeg ( bindex )
                  tail = - lnkprv( p, stpool )

                  call lnkfsl ( p, tail, stpool )
!
!                 Re-initialize the table for this body, and
!                 initiate an 'OLD FILES' search, just as in 'NEW
!                 BODY'.
!
                  btexp( bindex ) = 0
                  bthfs( bindex ) = ftnum( nft )
                  btlfs( bindex ) = ftnum( nft ) + 1

                  status = 'OLD FILES'

               end if

!
!              Unwind the stack; we've set the target states already.
!
               top = 0

            else
!
!              Return this cheapest list to the segment pool.  This
!              list could be empty.
!
               head = btbeg( cheap )

               if ( head > 0 ) then

                  tail = - lnkprv ( head, stpool )

                  call lnkfsl ( head, tail, stpool )

               end if

!
!              Fill the deleted body's space in the table with
!              the final entry in the table.
!
               if ( cheap /= nbt ) then

                  btbod (cheap) = btbod (nbt)
                  btexp (cheap) = btexp (nbt)
                  bthfs (cheap) = bthfs (nbt)
                  btlfs (cheap) = btlfs (nbt)
                  btbeg (cheap) = btbeg (nbt)

               end if

               if ( bindex == nbt ) then
                  bindex = cheap
               end if

!
!              One less body now.
!
               nbt    = nbt - 1
               status = 'RESUME'

            end if
!
!           At this point, we either made room by freeing a non-empty
!           segment list, or we're going to re-build the list for the
!           current body, starting with the highest-priority segments.
!           In the former case, the state is 'RESUME'; in the latter,
!           it's 'OLD FILES'.
!


         else if ( status == 'ADD TO FRONT' ) then
!
!           The current segment information should be linked in at
!           the head of the segment list for the current body,
!           and the pertinent body table entry should point
!           to the new head of the list.
!
!           The only way to get here is from the block NEW SEGMENTS
!           after suspending that task.

            if ( lnknfn(stpool) == 0 ) then

               doing  = 'ADD TO FRONT'
               urgent = 'MAKE ROOM'
               status = 'SUSPEND'

            else
!
!              Allocate a node and link it to the front of the list
!              for the current body.
!
               call lnkan ( stpool, new )

               sthan( new ) = fthan( findex )
!
!              Store the DLA and DSK descriptors for this segment in
!              the segment table.
!
               call movei ( dlalds, dladsz, stdlad(1,new) )
               call moved ( dsklds, dskdsz, stdskd(1,new) )

               if ( failed() ) return

!
!              If the current list is empty, this append operation
!              is a no-op.
!
               call lnkilb ( new, btbeg(bindex), stpool )
               btbeg( bindex ) = new

               status  = 'RESUME'

            end if


         else if ( status == 'ADD TO END' ) then
!
!           The current segment information should be linked in at
!           the tail of the segment list for the current body.
!
!           The only way to get to this task is from the OLD SEGMENTS
!           block after suspending that task.
!
            if ( lnknfn(stpool) == 0 ) then

               doing  = 'ADD TO END'
               urgent = 'MAKE ROOM'
               status = 'SUSPEND'

            else
!
!              Allocate a new node in the segment table pool.
!
               call lnkan ( stpool, new )

               sthan( new ) = fthan( findex )

!
!              Store the DLA and DSK descriptors for this segment in
!              the segment table.
!
               call movei ( dlalds, dladsz, stdlad(1,new) )
               call moved ( dsklds, dskdsz, stdskd(1,new) )

               if ( failed() ) return

               if ( btbeg(bindex) <= 0 ) then
!
!                 This is the first node in the list for this
!                 body.
!
                  btbeg( bindex ) = new

               else
!
!                 Link the new node to the tail of the list.
!
                  tail = - lnkprv ( btbeg(bindex), stpool )
                  call lnkila ( tail, new, stpool )

               end if

               status = 'RESUME'

            end if


         else if ( status == 'SUSPEND' ) then
!
!           When a task is suspended, the current activity is placed on
!           a stack, to be restored later. Two levels are provided,
!           since some interrupts can be interrupted by others.
!
            top          = top + 1
            stack( top ) = doing
            status       = urgent

         else if ( status == 'RESUME' ) then

            status = stack( top )
            top    = top - 1

         end if


      end do

!
!     Can only get here if status is 'HOPELESS', in which case a
!     segment was not found.
!
      found = .false.


      return




!$Procedure ZZDSKCHK ( DSK, check for file updates )

      entry zzdskchk ( usrctr, update )

! $ Abstract
!
!     Indicate to a calling routine whether any DSK load or unload
!     operations have occurred since the local state counter had
!     a particular, caller-supplied value.
!
! $ Required_Reading
!
!     DSK
!     DAS
!
! $ Keywords
!
!     TOPOGRAPHY
!
! $ Declarations
!
!     INTEGER               USRCTR ( * )
!     LOGICAL               UPDATE
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     USRCTR    I-O  User counter.
!     UPDATE     O   Update flag.
!
! $ Detailed_Input
!
!     USRCTR    is, on input, the value of a counter maintained by
!               a calling routine. This counter is to be compared to
!               a local counter.
!
! $ Detailed_Output
!
!     USRCTR    is, on output, the value of the local DSK load/unload
!               counter.
!
!     UPDATE    is a logical flag that is .TRUE. if and only if the
!               local DSK load/unload counter differs from the input
!               value of USRCTR. If the counters differ, a DSK load or
!               unload call has been made since the local counter last
!               had the value provided in USRCTR on input.
!
! $ Parameters
!
!     See zzctr.inc.
!
! $ Exceptions
!
!     1)  This routine will fail if the total count of DSK load and
!         unload operations exceeds the maximum accommodated by a
!         two-integer counter. The maximum count is about 4e19,
!         presuming a two-integer counter.
!
! $ Files
!
!     This routine does not access DSK files, but all DSK file
!     load and unload operations affect the local counter used
!     by this routine.
!
! $ Particulars
!
!     This set of routines maintains a local counter that indicates
!     the kernel load status. When a call is made to ZZDSKLSF or
!     ZZDSKUSF, the local counter is incremented. Applications can
!     compare counter values they save to the current count by
!     calling this routine. A mismatch indicates that DSK load or
!     unload calls have been made since an application's counter
!     was set.
!
!     This type of check presumes the caller's counter is initialized
!     by a call to
!
!        ZZCTRUIN
!
!     and is updated only by calls to this routine.
!
!     This routine is used by much of the DSK subsystem to determine
!     whether locally buffered information about DSK segments remains
!     up to date.
!
! $ Examples
!
!     See usage in ZZDSKBBL.
!
! $ Restrictions
!
!     This routine assumes the number of DSK load and unload operations
!     does not exceed the maximum value of a two-integer counter.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!     B.V. Semenov   (JPL)
!     R.E. Thurman   (JPL)
!     I.M. Underwood (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 08-FEB-2017 (NJB)
!
!        Updated version info.
!
!-&

! $ Index_Entries
!
!     check for changes to set of loaded dsk files
!
!-&


! $ Revisions
!
!     None.
!
!-&


      if ( return() ) return


      call zzctrchk ( dskctr, usrctr, update )

      return
      end
