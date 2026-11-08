!$Procedure DASCUD ( DAS, create or update directories )

      subroutine dascud ( handle, type, nwords )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Create or update directories in a DAS file to reflect addition
!     of a specified number of words of a specified data type.
!
! $ Required_Reading
!
!     DAS
!
! $ Keywords
!
!     DAS
!     FILES
!     UTILITY
!
! $ Declarations

      implicit none

      integer(ip)           handle
      integer(ip)           type
      integer(ip)           nwords

      integer(ip), parameter :: char = 1

      integer(ip), parameter :: dp = 2

      integer(ip), parameter :: int = 3

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   DAS file handle.
!     TYPE       I   Data type specifier.
!     NWORDS     I   Number of words of data being added.
!     CHAR       P   Parameter indicating character data type.
!     DP         P   Parameter indicating double precision data type.
!     INT        P   Parameter indicating integer data type.
!
! $ Detailed_Input
!
!     HANDLE   is the file handle of a DAS file open for writing.
!
!     TYPE     is a data type specifier. TYPE may be any of
!              the parameters
!
!                 CHAR
!                 DP
!                 INT
!
!              which indicate `character', `double precision',
!              and `integer' respectively.
!
!     NWORDS   is the number of words of data of the data type
!              indicated by TYPE whose addition to the indicated
!              DAS file is to be accounted for.
!
! $ Detailed_Output
!
!     None. See $Particulars for a description of the action of this
!     routine.
!
! $ Parameters
!
!     CHAR,
!     DP,
!     INT      are data type specifiers which indicate
!              `character', `double precision', and `integer'
!              respectively. These parameters are used in
!              all DAS routines that require a data type
!              specifier as input.
!
! $ Exceptions
!
!     1)  If the input handle is invalid, an error is signaled by a
!         routine in the call tree of this routine.
!
!     2)  If TYPE is not recognized, the error SPICE(DASINVALIDTYPE)
!         is signaled.
!
!     3)  If NWORDS is negative, the error SPICE(VALUEOUTOFRANGE) is
!         signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine operates by side effects: the directories in the
!     indicated DAS file will be updated to reflect the addition of
!     the indicated number of words of the specified data type.
!     If necessary, a new directory record will be added to the file
!     to hold a new cluster descriptor.
!
!     In addition, the file summary for the indicated DAS file will be
!     updated with the new values of the descriptor location and last
!     logical address of the indicated type, as well as with the new
!     value of the free record pointer.
!
!     This routine is used by the DASADx routines: after each data
!     addition, they call this routine to update the directories of the
!     affected DAS file.
!
!     Normally, there will be no need for routines outside of SPICELIB
!     to call this routine directly. To add data to or update a DAS
!     file, the DASADx and DASUDx routines should be used; these
!     routines take care of directory creation and updates.
!
! $ Examples
!
!     1)  Update directories after writing N integer words to a
!         DAS file designated by HANDLE:
!
!             CALL DASCUD ( HANDLE, INT, N )
!
! $ Restrictions
!
!     1)  This routine is intended for use by the SPICELIB DAS routines.
!         Non-SPICELIB software normally will not need to call this
!         routine.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.5.0, 13-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.4.0, 07-AUG-2006 (NJB)
!
!        Bug fix: added initialization of variable LTYPE to support
!                 operation under the Macintosh Intel Fortran
!                 compiler. Note that this bug did not affect
!                 operation of this routine on other platforms.
!
!-    SPICELIB Version 1.3.0, 16-JAN-2003 (NJB)
!
!        Bug fix: fixed previous bug fix.
!
!-    SPICELIB Version 1.2.0, 10-DEC-2002 (NJB)
!
!        Bug fix: now a new, empty directory record with valid
!        backward and forward pointers is written immediately
!        when it is created.
!
!-    SPICELIB Version 1.1.1, 19-DEC-1995 (NJB)
!
!        Corrected title of permuted index entry section.
!
!-    SPICELIB Version 1.0.1, 26-OCT-1993 (KRG)
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input section of the header. This was done in order
!        to minimize documentation changes if the DAS open routines ever
!        change.
!
!        Removed an unused variable.
!
!-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
!
!-&

! $ Index_Entries
!
!     update DAS cluster directories
!
!-&

! $ Revisions
!
!-    SPICELIB Version 1.4.0 07-AUG-2006 (NJB)
!
!        Bug fix: added initialization of variable LTYPE to support
!                 operation under the Macintosh Intel Fortran
!                 compiler. Note that this bug did not affect
!                 operation of this routine on other platforms. The
!                 statement referencing the uninitialized variable
!                 was:
!
!           ELSE IF (       ( TYPE   .EQ. LTYPE )
!          .          .AND. ( DSCREC .GT. 0     )
!          .          .AND. ( LWORD  .LT. NWI   )  ) THEN
!
!        In the previous version of the code, LTYPE is uninitialized
!        when the DAS file is empty, which implies DSCREC is 0.
!        Otherwise LTYPE is initialized. So the value of the logical
!        expression is not affected by the uninitialized value of
!        LTYPE.
!
!        However, the Intel Fortran compiler for the Mac flags a runtime
!        error when the above code is exercised. So LTYPE is now
!        initialized to an invalid value prior to execution of this
!        code. If the invalid value is ever used, a runtime error
!        should result.
!
!-    SPICELIB Version 1.3.0 16-JAN-2003 (NJB)
!
!        Bug fix: fixed previous bug fix.
!
!        The offending line (#778) in previous version) of code is:
!
!           CALL DASWRI ( HANDLE, RECNO, DIRREC )
!
!        The correct line of code is:
!
!          CALL DASWRI ( HANDLE, FREE, DIRREC )
!
!-    SPICELIB Version 1.2.0 10-DEC-2002 (NJB)
!
!        Bug fix: now a new, empty directory record with valid
!        backward and forward pointers is written immediately
!        when it is created. This prevents an unsegregated file
!        from being left with an invalid forward pointer.
!
!-    SPICELIB Version 1.0.1, 26-OCT-1993 (KRG)
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input section of the header. This was done in order
!        to minimize documentation changes if the DAS open routines ever
!        change.
!
!        Removed an unused variable, PREV.
!
!-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
!
!-&

!
!     SPICELIB functions
!
      logical               return

!
!     Local parameters
!

!
!     Words per data record, for each data type:
!
      integer(ip), parameter :: nwc = 1024

      integer(ip), parameter :: nwd = 128

      integer(ip), parameter :: nwi = 256

!
!     Directory pointer locations (backward and forward):
!
      integer(ip), parameter :: bwdloc = 1

      integer(ip), parameter :: fwdloc = 2

!
!     Directory address range locations
!
      integer(ip), parameter :: chrrng = 3

      integer(ip), parameter :: dprng = chrrng + 2

      integer(ip), parameter :: intrng = dprng  + 2

!
!     Location of first type descriptor
!
      integer(ip), parameter :: begdsc = 9

!
!     Local variables
!
      integer(ip)           descr
      integer(ip)           dirrec ( nwi )
      integer(ip)           dscrec
      integer(ip)           free
      integer(ip)           i
      integer(ip)           last
      integer(ip)           lastla ( 3 )
      integer(ip)           lastrc ( 3 )
      integer(ip)           lastwd ( 3 )
      integer(ip)           lrec
      integer(ip)           loc
      integer(ip)           ltype
      integer(ip)           lword
      integer(ip)           maxadr
      integer(ip)           minadr
      integer(ip)           ncomc
      integer(ip)           ncomr
      integer(ip)           needed
      integer(ip)           nresvc
      integer(ip)           nresvr
      integer(ip)           nw
      integer(ip)           recno
      integer(ip)           rngloc
      integer(ip)           room

!
!     Saved variables
!

!
!     NEXT maps the DAS data type codes to their successors.
!
      integer(ip)           next   ( 3 )
      save                  next

!
!     Initial values
!
      data                  next   /  2,   3,   1  /

!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     Here's a preview of coming attractions:
!
!        We're going to update the directories in the indicated
!        DAS file to reflect the addition of NWORDS new data words.
!        This data is supposed to have been added to the file BEFORE
!        this routine is called.  There are several possible states
!        the file can be in at the point this routine is called.
!
!           1)  There is already a descriptor of TYPE in the file, and
!               the addition of data does not require this descriptor
!               to be modified.
!
!               We can tell that we have this case when the file
!               summary indicates that, before the addition of data,
!               there was room for NWORDS of data in the last data
!               record in the file.  Since no new data records were
!               required to accommodate the new data, the descriptor
!               for TYPE does not have to be updated.
!
!               However, even though the descriptor need not be
!               modified, the address range for TYPE covered by the
!               directory record containing this last descriptor must be
!               updated, as must be the file summary.
!
!           2)  There is already a descriptor of TYPE in the file, and
!               in order to describe the new data added to the file,
!               it suffices to update this descriptor and the address
!               range in the directory containing it.
!
!               This happens when case (1) doesn't apply, and the
!               descriptor of TYPE is the last descriptor in the last
!               directory, and the descriptor is not in the last
!               position (index NWI) of the directory.
!
!               Note that we never update the last descriptor in a
!               directory record.  The reason for this is that after
!               this descriptor is written, we build a new directory
!               record.  All subsequent additions of data are made to
!               records that follow this new directory record;
!               otherwise, the new directory would get overwritten
!               with data.
!
!           3)  A new descriptor of TYPE is needed.
!
!               This can happen in several ways:
!
!               a)  There are no directories in the file yet, in which
!                   case space has been reserved for the first
!                   directory.
!
!                   This can happen only when the file had no data at
!                   all in it before the last addition of data.
!
!                   In this case, we must fill in the first descriptor
!                   and the address range for TYPE.  We must also update
!                   the file summary, because the descriptor location,
!                   last logical address of TYPE, and the free pointer
!                   have changed.
!
!               b)  The conditions for cases (1) and (2) are not
!                   satisfied, and the current last directory record
!                   has room for a new descriptor.  In this case, if
!                   the data addition filled in the last data record
!                   described by the current last descriptor of type,
!                   (which will usually be the case), we must update
!                   the appropriate address range in the directory
!                   record containing that descriptor.  We will then
!                   add a new descriptor to the last directory record
!                   and update the address range for TYPE in that
!                   record.  The file summary must be updated as well.
!
!                   If the new descriptor we've added went into the
!                   last slot in a directory record (index NWI), we
!                   also create a new, empty directory record and
!                   update the forward pointer of the current directory
!                   to point to it.  We also update the file summary
!                   so that the free pointer points to the record
!                   following the empty directory record.
!
!               c)  The conditions for cases (1) and (2) are not
!                   satisfied, and the current last directory record
!                   has no room for a new descriptor.
!
!                   In this case, if the data addition filled in the
!                   last data record described by the current last
!                   descriptor of TYPE, (which will usually be the
!                   case), we must update the appropriate address range
!                   in the directory record containing that descriptor.
!                   We will then add a new descriptor to the empty
!                   directory record and initialize the address range
!                   for TYPE in that record.  The file summary must be
!                   updated as well.
!
!     To start out, we'll need to find out how the file is currently
!     disposed.  We'll need the location of the last descriptor of
!     TYPE, the last logical address of TYPE, and the location of
!     the last descriptor of any type.
!
!     Get the file summary.
!
      call dashfs ( handle, &
                    nresvr, &
                    nresvc, &
                    ncomr, &
                    ncomc, &
                    free, &
                    lastla, &
                    lastrc, &
                    lastwd )

!
!     Now do all of the data-type-dependent work:
!
!        -- Set the last address of the indicated data type LAST.
!
!        -- Set the physical record of the last descriptor of TYPE.
!
!        -- Set the number of words of data of the specified type per
!           physical record NW.
!
!        -- Set the address range location used to pick address ranges
!           out of directory records.
!
!     Note that the address and descriptor location information from
!     the file summary is assumed NOT to take into account the latest
!     data addition.
!
      last    =  lastla( type )
      dscrec  =  lastrc( type )

      if ( type == dp ) then

         nw      =  nwd
         rngloc  =  dprng

      else if ( type == int ) then

         nw      =  nwi
         rngloc  =  intrng

      else if ( type == char ) then

         nw      =  nwc
         rngloc  =  chrrng

      else

         call setmsg ( 'Invalid data type: #. ' )
         call errint ( '#',  type               )
         call sigerr ( 'SPICE(DASINVALIDTYPE)'  )
         return

      end if

!
!     Make sure that NWORDS is something sensible.
!
      if ( nwords < 0 ) then

         call setmsg ( 'NWORDS was #; should be non-negative.' )
         call errint ( '#',  nwords                            )
         call sigerr ( 'SPICE(VALUEOUTOFRANGE)'                )
         return

      end if

!
!     Find the record and word positions LREC and LWORD of the last
!     descriptor in the file, and also find the type of the descriptor
!     LTYPE.
!
      call maxai  ( lastrc,  3,  lrec,  loc )
      lword  =  0
      ltype  =  0

      do i = 1, 3

         if (       ( lastrc(i) == lrec  ) &
              .and. ( lastwd(i) > lword )  ) then

            lword = lastwd(i)
            ltype = i

         end if

      end do

!
!     LREC, LWORD, and LTYPE are now the record, word, and data type
!     of the last descriptor in the file.  If LREC is zero, there are
!     no directories in the file yet.  In this case, LWORD and
!     LTYPE are both zero.
!

!
!     Compute the number of words we have room for in the current
!     last data record of the indicated type.
!
      if ( last > 0 ) then
         room  =  nw   -   (  last  -  ( (last-1) / nw ) * nw  )
      else
         room  =  0
      end if

!
!     Compute the number of additional data records needed to
!     accommodate (NWORDS - ROOM) additional words of data of type
!     TYPE.
!
      needed  =  ( nwords - room + nw - 1 ) / nw

!
!     Now, update the descriptor directories.
!

      if (  ( room >= nwords ) .and. ( dscrec > 0 )  ) then
!
!        This is case (1).
!
!        There is already a descriptor of TYPE in the file.  The data
!        fits in the current record, so no descriptors have to change.
!
!        Update the address range in the directory record containing
!        the last descriptor of TYPE.
!
         maxadr  =  last + nwords

         call dasuri ( handle, dscrec, rngloc+1, rngloc+1, maxadr )

!
!        The last logical address of TYPE is now MAXADR.
!
         lastla( type )  =  maxadr

!
!        Write out the updated file summary.
!
         call dasufs ( handle, &
                       nresvr, &
                       nresvc, &
                       ncomr, &
                       ncomc, &
                       free, &
                       lastla, &
                       lastrc, &
                       lastwd )

      else if (       ( type   == ltype ) &
                .and. ( dscrec > 0     ) &
                .and. ( lword  < nwi   )  ) then
!
!        This is case (2).
!
!        The descriptor of TYPE is the last descriptor in the
!        file but is not in the last location (index NWI) of a
!        directory record.  All we have to do is update this last
!        descriptor to reflect the addition of the number of needed
!        data records.
!
!        Get the old descriptor, since we're going to update it.
!
         call dasrri ( handle, dscrec, lword, lword, descr )

!
!        Update the descriptor and write it back into the file.
!
         if ( descr < 0 ) then
            descr = descr - needed
         else
            descr = descr + needed
         end if

         call dasuri ( handle, dscrec, lword, lword, descr )

!
!        Update the address range for this type.
!
         maxadr = last + nwords

         call dasuri ( handle, dscrec, rngloc+1, rngloc+1, maxadr )

!
!        The last logical address of TYPE is now MAXADR.  The first
!        free record follows the last data record in use.
!
         lastla( type )  =  maxadr
         free            =  free  +  needed

!
!        Write out the updated file summary.
!
         call dasufs ( handle, &
                       nresvr, &
                       nresvc, &
                       ncomr, &
                       ncomc, &
                       free, &
                       lastla, &
                       lastrc, &
                       lastwd )

      else
!
!        This is case (3).  We need a new descriptor.
!

         if ( lrec == 0 ) then
!
!           This is case (3a).  We have a virgin directory record.
!           Set the number of this record.
!
            recno  =  nresvr + ncomr + 2

!
!           Start with an empty directory record.
!
            call cleari ( nwi, dirrec )

!
!           Add a new descriptor to the directory.  The record
!           count is the number of new records required:  NEEDED.
!
            dirrec ( begdsc     )  =  type
            dirrec ( begdsc + 1 )  =  needed

!
!           Fill in the address range for TYPE covered by this
!           directory.
!
            dirrec ( rngloc     )  =  1
            dirrec ( rngloc + 1 )  =  nwords

!
!           Write out this directory.
!
            call daswri ( handle, recno, dirrec )

!
!           Update the file summary:  the location of the descriptor
!           and the last logical address for this type must be set.
!           The count portion of the descriptor goes after the initial
!           data type indicator; this data type indicator is not
!           considered to be part of the descriptor.
!
!           The first free record follows the last data record in use.
!
            free            =  recno  + needed + 1
            lastla( type )  =  nwords
            lastrc( type )  =  recno
            lastwd( type )  =  begdsc + 1

            call dasufs ( handle, &
                          nresvr, &
                          nresvc, &
                          ncomr, &
                          ncomc, &
                          free, &
                          lastla, &
                          lastrc, &
                          lastwd )

         else if ( lword < nwi ) then
!
!           This is case (3b).  We have room for another descriptor
!           in the current directory record.
!
!           Before adding the new descriptor, we must update the
!           directory containing the current last descriptor of TYPE,
!           if the range of addresses covered by the cluster it
!           describes was increased by the last data addition.  Of
!           course, this update is required only if there IS such a
!           descriptor, and if it is in a record that precedes LREC.
!
            if (       ( dscrec > 0    ) &
                 .and. ( dscrec < lrec ) &
                 .and. ( room   > 0    )   )  then
!
!              Update the address range for TYPE in record DSCREC.
!              The upper bound is increased by ROOM, since that many
!              words of TYPE were added to the last record in the
!              last cluster of TYPE described by that directory.
!
               maxadr = last + room

               call dasuri (handle, dscrec, rngloc+1, rngloc+1, maxadr )

            end if

!
!           Make up the new descriptor and write it to the last
!           directory, following the current last descriptor.  The
!           sign of the new descriptor is a function of the type of
!           the current last descriptor.
!
            if (  type  ==  next(ltype)  ) then
!
!              TYPE is the successor in the type sequence of the type
!              of the previous descriptor; use a positive count.
!
               descr  =   needed

            else
               descr  =  -needed

            end if

            call dasuri ( handle, lrec, lword+1,  lword+1,  descr  )

!
!           Update the address range for this type.  Some care is needed
!           when updating the minimum address:  this value should be
!           assigned only if this is the first descriptor of TYPE in
!           this directory record.
!
            if ( dscrec < lrec ) then

               minadr = last + room + 1

               call dasuri ( handle, lrec, rngloc, rngloc, minadr )

            end if

            maxadr = last + nwords

            call dasuri ( handle, lrec, rngloc+1, rngloc+1, maxadr )

!
!           Update the file summary:  the location of the descriptor
!           and the last logical address for this type must be set.
!
!           The first free record follows the last data record in use.
!
            free            =  free  +  needed
            lastla( type )  =  last  +  nwords
            lastrc( type )  =  lrec
            lastwd( type )  =  lword +  1

!
!           Before writing out the summary, see whether we'll need
!           a new directory; this will decide whether the first free
!           record changes.
!
!           If we just filled in the last descriptor in a directory,
!           it's time to add a new directory record to the file.
!           All we have to do at the moment is make room for it, and
!           set the forward pointer of the current directory record
!           to point to the saved record.  Initialize the pointers
!           of the new directory record to make the linked list valid.
!

            if ( lword+1 == nwi ) then
!
!              Update the previous directory to point forward to the
!              next one.
!
               call dasuri ( handle, lrec, fwdloc, fwdloc, free )

!
!              Prepare the new directory record: clear it, set the
!              backward pointer, and write the record.
!
               call cleari ( nwi, dirrec )

               dirrec ( bwdloc )  =  lrec

               call daswri ( handle, free, dirrec )

!
!              Update the free record number.
!
               free = free + 1

            end if

!
!           Now write out the file summary.
!
            call dasufs ( handle, &
                          nresvr, &
                          nresvc, &
                          ncomr, &
                          ncomc, &
                          free, &
                          lastla, &
                          lastrc, &
                          lastwd )

         else
!
!           This is case (3c).  We must put the new descriptor in
!           the last directory record, which is currently empty.
!
!           As in case (3b), we may have to update the directory
!           containing the current last descriptor of TYPE, if the
!           range of addresses covered by the cluster it describes was
!           increased by the last data addition.  Of course, this
!           update is required only if there IS such a descriptor.
!
            if (  ( dscrec > 0 ) .and. ( room > 0 )  )  then
!
!              Update the address range for TYPE in record DSCREC.
!              The upper bound is increased by ROOM, since that many
!              words of TYPE were added to the last record in the
!              last cluster of TYPE described by that directory.
!
               maxadr = last + room

               call dasuri (handle, dscrec, rngloc+1, rngloc+1, maxadr )

            end if

!
!           Obtain the record number for this directory.
!
            call dasrri ( handle, lrec, fwdloc, fwdloc, recno )

!
!           Now fill in the new directory record.  Start with a clean
!           record.
!
            call cleari ( nwi, dirrec )

!
!           Set the backward pointer, the address range for TYPE,
!           initial data type, and record count.
!
            dirrec ( bwdloc     )  =  lrec
            dirrec ( rngloc     )  =  last   +  room    +  1
            dirrec ( rngloc + 1 )  =  last   +  nwords
            dirrec ( begdsc     )  =  type
            dirrec ( begdsc + 1 )  =  needed

!
!           Write out the record.
!
            call daswri ( handle, recno, dirrec )

!
!           Update the file summary to reflect the new record and word
!           offsets of the last descriptor of the indicated type.  The
!           last address of TYPE has increased also.  The first free
!           record lies after the added data records.
!
            free            =  free   +  needed
            lastla( type )  =  last   +  nwords
            lastrc( type )  =  recno
            lastwd( type )  =  begdsc +  1

            call dasufs ( handle, &
                          nresvr, &
                          nresvc, &
                          ncomr, &
                          ncomc, &
                          free, &
                          lastla, &
                          lastrc, &
                          lastwd )

         end if

      end if

      return
      end
