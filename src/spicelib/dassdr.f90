!$Procedure DASSDR ( DAS, segregate data records )

      subroutine dassdr ( handle )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Segregate the data records in a DAS file into clusters, using
!     one cluster per data type present in the file.
!
! $ Required_Reading
!
!     DAS
!
! $ Keywords
!
!     DAS
!     FILES
!     ORDER
!     SORT
!
! $ Declarations

      implicit none

      integer(ip)           handle

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   DAS file handle.
!
! $ Detailed_Input
!
!     HANDLE   is a file handle of a DAS file opened for writing.
!
! $ Detailed_Output
!
!     None. See $Particulars for a description of the effect of this
!     routine.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input file handle is invalid, an error is signaled by a
!         routine in the call tree of this routine.
!
!     2)  If a Fortran read attempted by this routine fails, an error is
!         signaled by a routine in the call tree of this routine. The
!         state of the DAS file undergoing re-ordering will be
!         indeterminate.
!
!     3)  If a Fortran write attempted by this routine fails, an error
!         is signaled by a routine in the call tree of this routine. The
!         state of the DAS file undergoing re-ordering will be
!         indeterminate.
!
!     4)  If any other I/O error occurs during the re-arrangement of the
!         records in the indicated DAS file, the error is signaled by a
!         routine in the call tree of this routine.
!
! $ Files
!
!     See the description of the argument HANDLE in $Detailed_Input.
!
! $ Particulars
!
!     Normally, there should be no need for routines outside of
!     SPICELIB to call this routine.
!
!     The effect of this routine is to re-arrange the data records
!     in a DAS file so that the file contains a single cluster for
!     each data type present in the file: in the general case, there
!     will be a single cluster of each of the integer, double
!     precision, and character data types.
!
!     The relative order of data records of a given type is not
!     affected by this re-ordering. After the re-ordering, the DAS
!     file contains a single directory record that has one descriptor
!     for each cluster. After that point, the order in the file of the
!     sets of data records of the various data types will be:
!
!        +-------+
!        |  CHAR |
!        +-------+
!        |  DP   |
!        +-------+
!        |  INT  |
!        +-------+
!
!     Files that contain multiple directory records will have all but
!     the first directory record moved to the end of the file when the
!     re-ordering is complete. These records are not visible to the
!     DAS system and will be overwritten if data is subsequently added
!     to the DAS file.
!
!     The purpose of segregating a DAS file's data records into three
!     clusters is to make read access more efficient: when a DAS file
!     contains a single directory with at most three cluster type
!     descriptors, mapping logical to physical addresses can be done
!     in constant time.
!
! $ Examples
!
!     1)  Segregate data records in a DAS file designated by
!         HANDLE:
!
!            CALL DASSDR ( HANDLE )
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
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!     H.A. Neilan        (JPL)
!     M.J. Spencer       (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.0, 12-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.0.1, 19-DEC-1995 (NJB)
!
!        Corrected title of permuted index entry section.
!
!-    SPICELIB Version 2.0.0, 17-NOV-1993 (KRG)
!
!        Added test of FAILED after each DAS call, or sequence of calls,
!        which returns immediately if FAILED is .TRUE. This fixes a bug
!        where DASOPS signals an error and then DASSDR has a
!        segmentation fault.
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input section of the header. This was done in order
!        to minimize documentation changes if the DAS open routines ever
!        change.
!
!-    SPICELIB Version 1.2.0, 07-OCT-1993 (NJB) (HAN) (MJS)
!
!        Bug fix: call to CLEARD replaced with call to
!        CLEARI.
!
!-    SPICELIB Version 1.1.0, 08-JUL-1993 (NJB) (MJS)
!
!        Bug fix: extraneous commas removed from argument lists
!        in calls to DASADI.
!
!-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT)
!
!-&


! $ Index_Entries
!
!     segregate the data records in a DAS file
!
!-&


! $ Revisions
!
!-    SPICELIB Version 2.0.0, 17-NOV-1993 (KRG)
!
!        Added test of failed after each DAS call, or sequence of calls,
!        which returns immediately if FAILED is .TRUE. This fixes a bug
!        where DASOPS signals an error and then DASSDR has a
!        segmentation fault.
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input section of the header. This was done in order
!        to minimize documentation changes if the DAS open routines ever
!        change.
!
!-    SPICELIB Version 1.2.0, 07-OCT-1993 (NJB) (HAN) (MJS)
!
!        Bug fix: call to CLEARD replaced with call to
!        CLEARI.
!
!-    SPICELIB Version 1.1.0, 08-JUL-1993 (NJB)
!
!        Bug fix: extraneous commas removed from argument lists
!        in calls to DASADI. This bug had no visible effect on
!        VAX and Sun systems, but generated a compile error under
!        Lahey Fortran.
!
!-&


!
!     SPICELIB functions
!
      integer(ip)           sumai

      logical               failed
      logical               return

!
!     Local parameters
!
      integer(ip)           nwc
      parameter           ( nwc   = 1024 )

      integer(ip)           nwd
      parameter           ( nwd   =  128 )

      integer(ip)           nwi
      parameter           ( nwi   =  256 )

!
!     Data type parameters
!
      integer(ip)           char
      parameter           ( char   =  1  )

      integer(ip)           dp
      parameter           ( dp     =  2  )

      integer(ip)           int
      parameter           ( int    =  3  )

      integer(ip)           dir
      parameter           ( dir    =  4  )

!
!     Directory pointer locations (backward and forward):
!
      integer(ip)           bwdloc
      parameter           ( bwdloc = 1 )

      integer(ip)           fwdloc
      parameter           ( fwdloc = 2 )

!
!     Directory address range location base
!
      integer(ip)           rngbas
      parameter           ( rngbas  =  2  )


!
!     Location of first type descriptor
!
      integer(ip)           begdsc
      parameter           ( begdsc = 9 )

!
!     Local variables
!
      character*(nwc)       crec
      character*(nwc)       savec

      real(wp)              drec   ( nwd )
      real(wp)              saved  ( nwd )

      integer(ip)           base
      integer(ip)           count  ( 4 )
      integer(ip)           dest
      integer(ip)           drbase
      integer(ip)           free
      integer(ip)           i
      integer(ip)           irec   ( nwi    )
      integer(ip)           j
      integer(ip)           lastla ( 3 )
      integer(ip)           lastrc ( 3 )
      integer(ip)           lastwd ( 3 )
      integer(ip)           loc
      integer(ip)           lrec
      integer(ip)           ltype
      integer(ip)           lword
      integer(ip)           maxadr
      integer(ip)           minadr
      integer(ip)           n
      integer(ip)           ncomc
      integer(ip)           ncomr
      integer(ip)           nresvc
      integer(ip)           nresvr
      integer(ip)           offset
      integer(ip)           pos
      integer(ip)           prvtyp
      integer(ip)           recno
      integer(ip)           savei  ( nwi    )
      integer(ip)           savtyp
      integer(ip)           scrhan
      integer(ip)           start
      integer(ip)           total
      integer(ip)           type
      integer(ip)           unit

      logical               more



!
!     Saved variables
!

!
!     NEXT and PREV map the DAS data type codes to their
!     successors and predecessors, respectively.
!
      integer(ip)           next   ( 3 )
      save                  next

      integer(ip)           prev   ( 3 )
      save                  prev

!
!     Initial values
!
      data                  next   /  2,   3,   1  /
      data                  prev   /  3,   1,   2  /




!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'DASSDR' )
      end if

!
!     Before starting, make sure that this DAS file is open for
!     writing.
!
      call dassih ( handle, 'WRITE' )

!
!     Get the logical unit for this file.
!
      call dashlu ( handle, unit )

      if ( failed() ) then
         call chkout ( 'DASSDR' )
         return
      end if

!
!     Write out any buffered records that belong to the file.
!
      call daswbr ( handle )

      if ( failed () ) then
         call chkout ( 'DASSDR' )
         return
      end if

!
!     We're going to re-order the physical records in the DAS file,
!     starting with the first record after the first directory.
!     The other directory records are moved to the end of the file
!     as a result of the re-ordering.
!
!     The re-ordering algorithm is based on that used in the REORDx
!     routines.  To use this algorithm, we'll build an order vector
!     for the records to be ordered; we'll construct this order vector
!     in a scratch DAS file.  First, we'll traverse the directories
!     to build up a sort of inverse order vector that tells us the
!     final destination and data type of each data record;  from this
!     inverse vector we can easily build a true order vector.  The
!     cycles of the true order vector can be traversed without
!     repetitive searching, and with a minimum of assignment of the
!     contents of data records to temporary variables.
!

!
!     Allocate a scratch DAS file to keep our vectors in.
!
      call dasops ( scrhan )

      if ( failed () ) then
         call chkout ( 'DASSDR' )
         return
      end if

!
!     Now build up our `inverse order vector'.   This array is an
!     inverse order vector only in loose sense:  it actually consists
!     of an integer array that contains a sequence of pairs of integers,
!     the first of which indicates a data type, and the second of which
!     is an ordinal number.  There is one pair for each data record in
!     the file.  The ordinal number gives the ordinal position of the
!     record described by the number pair, relative to the other records
!     of the same type.  Directory records are considered to have type
!     `directory', which is represented by the code DIR.
!
!     We also must maintain a count of records of each type.
!
      call cleari ( 4, count )

!
!     Get the file summary for the DAS file to be segregated.
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

      if ( failed () ) then
         call chkout ( 'DASSDR' )
         return
      end if
!
!     Find the record and word positions LREC and LWORD of the last
!     descriptor in the file, and also find the type of the descriptor
!     LTYPE.
!
      call maxai  ( lastrc,  3,  lrec,  loc )
      lword = 0

      do i = 1, 3

         if (       ( lastrc(i) .eq. lrec  ) &
              .and. ( lastwd(i) .gt. lword )  ) then

            lword = lastwd(i)
            ltype = i

         end if

      end do

!
!     The first directory starts after the last comment record.
!
      recno  =  nresvr + ncomr + 2


      do while (  ( recno .le. lrec ) .and. ( recno .gt. 0 )  )
!
!        Read the directory record.
!
         call dasrri ( handle, recno, 1, nwi, irec )

         if ( failed () ) then
             call chkout ( 'DASSDR' )
           return
         end if
!
!        Increment the directory count.
!
         count(dir)  =  count(dir) + 1

!
!        Add the data type (`directory') and count (1) of the current
!        record to the inverse order vector.
!
         call dasadi ( scrhan,  1,  dir        )
         call dasadi ( scrhan,  1,  count(dir) )

         if ( failed () ) then
            call chkout ( 'DASSDR' )
            return
         end if
!
!        Set up our `finite state machine' that tells us the data
!        types of the records described by the last read directory.
!
         type    =  irec ( begdsc )
         prvtyp  =  prev ( type   )

!
!        Now traverse the directory and update the inverse order
!        vector based on the descriptors we find.
!
         more  =  .true.
         i     =   begdsc + 1


         do while ( more )
!
!           Obtain the count for the current descriptor.
!
            n  =  abs (  irec(i)  )

!
!           Update our inverse order vector to describe the positions
!           of the N records described by the current descriptor.
!
            do j  =  1, n

               call dasadi (  scrhan,  1,  type           )
               call dasadi (  scrhan,  1,  count(type)+j  )

               if ( failed () ) then
                  call chkout ( 'DASSDR' )
                  return
               end if

            end do

!
!           Adjust the count of records of data type TYPE.
!
            count(type)  =  count(type)   +   n

!
!           Find the next type.
!
            i = i + 1

            if (      (  i     .gt. nwi                        ) &
                 .or. ( (recno .eq. lrec) .and. (i .gt. lword) )  ) then

               more = .false.

            else

               if ( irec(i) .gt. 0 ) then
                  type = next(type)

               else if ( irec(i) .lt. 0 ) then
                  type = prev(type)

               else
                  more = .false.

               end if

            end if

         end do

!
!        The forward pointer in this directory tells us where the
!        next directory record is.  When there are no more directory
!        records, this pointer will be zero.
!
         recno = irec ( fwdloc )

      end do

!
!     At this point, the inverse order vector is set up.  The array
!     COUNT contains counts of the number of records of each type we've
!     seen.  Set TOTAL to the total number of records that we've going
!     to permute.
!
      total =  sumai ( count, 4 )

!
!     The next step is to build a true order vector.  Let BASE be
!     the base address for the order vector; this address is the
!     last logical address of the inverse order vector.
!
      base  =  2 * total

!
!     We'll store the actual order vector in locations BASE + 1
!     through BASE + TOTAL.  In addition, we'll build a parallel array
!     that contains, for each element of the order vector, the type of
!     data corresponding to that element.  This type vector will
!     reside in locations BASE + TOTAL + 1 through BASE + 2*TOTAL.
!
!     Before setting the values of the order vector and its parallel
!     type vector, we'll allocate space in the scratch DAS file by
!     zeroing out the locations we plan to use.  After this, locations
!     BASE+1 through BASE + 2*TOTAL can be written to in random access
!     fashion using DASUDI.
!
!
      do i = 1, 2*total

         call dasadi ( scrhan, 1, 0 )

      end do

      if ( failed () ) then
         call chkout ( 'DASSDR' )
         return
      end if
!
!     We note that the way to construct the inverse of a permutation
!     SIGMA in a single loop is suggested by the relation
!
!             -1
!        SIGMA   (  SIGMA(I)  )   =   I
!
!     We'll use this method.  In our case, our order vector plays
!     the role of
!
!             -1
!        SIGMA
!
!     and the `inverse order vector' plays the role of SIGMA.  We'll
!     exclude the first directory from the order vector, since it's
!     an exception:  we wish to reserve this record.  Since the first
!     element of the order vector (logically) contains the index 1, we
!     can ignore it.
!
!
      do i = 2, total

         call dasrdi ( scrhan,  (2*i)-1,  (2*i)-1,  type  )
         call dasrdi ( scrhan,   2*i,      2*i,     dest  )

         if ( failed () ) then
            call chkout ( 'DASSDR' )
            return
         end if
!
!        Set DEST to the destination location, measured as an offset
!        from the last comment record, of the Ith record by adding
!        on the count of the predecessors of the block of records of
!        TYPE.
!
         do j = 1, 3

            if ( type .gt. j ) then
               dest = dest + count(j)
            end if

         end do

!
!        The destination offset of each record should be incremented to
!        allow room for the first directory record.  However, we don't
!        need to do this for directory records; they'll already have
!        this offset accounted for.
!
         if ( type .ne. dir ) then
            dest = dest + 1
         end if

!
!        The value of element DEST of the order vector is I.
!        Write this value to location BASE + DEST.
!
         call dasudi ( scrhan, base+dest, base+dest, i )
!
!        We want the ith element of the order vector to give us the
!        number of the record to move to position i (offset from the
!        last comment record),  but we want the corresponding element
!        of the type array to give us the type of the record currently
!        occupying position i.
!
         call dasudi ( scrhan, base+i+total, base+i+total, type )

         if ( failed () ) then
            call chkout ( 'DASSDR' )
            return
         end if

      end do

!
!     Ok, here's what we've got in the scratch file that's still of
!     interest:
!
!        -- In integer logical addresses BASE + 1 : BASE + TOTAL,
!           we have an order vector.  The Ith element of this
!           vector indicates the record that should be moved to
!           location DRBASE + I in the DAS file we're re-ordering,
!           where DRBASE is the base address of the data records
!           (the first directory record follows the record having this
!           index).
!
!
!        -- In integer logical addresses BASE + TOTAL + 1  :  BASE +
!           2*TOTAL, we have data type indicators for the records to
!           be re-ordered.  The type for the Ith record in the file,
!           counted from the last comment record, is located in logical
!           address BASE + TOTAL + I.
!
!
      drbase  =  nresvr + ncomr + 1

!
!     As we traverse the order vector, we flip the sign of elements
!     we've accessed, so that we can tell when we encounter an element
!     of a cycle that we've already traversed.
!
!     Traverse the order vector.  The variable START indicates the
!     first element to look at.  Ignore the first element; it's a
!     singleton cycle.
!
!
      start = 2

      do while ( start .lt. total )
!
!        Traverse the current cycle of the order vector.
!
!        We `make a hole' in the file by saving the record in position
!        START, then we traverse the cycle in reverse order, filling in
!        the hole at the ith position with the record whose number is
!        the ith element of the order vector.  At the end, we deposit
!        the saved record into the `hole' left behind by the last
!        record we moved.
!
!        We're going to read and write records to and from the DAS file
!        directly, rather than going through the buffering system.
!        This will allow us to avoid any untoward interactions between
!        the buffers for different data types.
!
         call dasrdi ( scrhan, &
                       base + total + start, &
                       base + total + start, &
                       savtyp                )

         call dasrdi ( scrhan,  base + start,  base + start,  offset )

!
!        Save the record at the location DRBASE + START.
!
         if ( savtyp .eq. char ) then

            call dasioc ( 'READ', unit, drbase+start, savec )

         else if ( savtyp .eq. dp ) then

            call dasiod ( 'READ', unit, drbase+start, saved )

         else

            call dasioi ( 'READ', unit, drbase+start, savei )

         end if

         if ( failed () ) then
            call chkout ( 'DASSDR' )
            return
         end if
!
!        Let I be the index of the record that we are going to move
!        data into next.  I is an offset from the last comment record.
!
         i  =  start

         do while ( offset .ne. start )
!
!           Mark the order vector element by writing its negative
!           back to the location it came from.
!
            call dasudi ( scrhan,  base+i,  base+i,  -offset )
!
!           Move the record at location
!
!              DRBASE + OFFSET
!
!           to location
!
!              DRBASE + I
!
!           There is no need to do anything about the corresponding
!           elements of the type vector; we won't need them again.
!
!           The read and write operations, as well as the temporary
!           record required to perform the move, are dependent on the
!           data type of the record to be moved.
!
            call dasrdi ( scrhan, &
                          base + total + offset, &
                          base + total + offset, &
                          type                    )

            if ( failed () ) then
               call chkout ( 'DASSDR' )
               return
            end if
!
!           Only pick records up if we're going to put them down in
!           a location other than their original one.
!
            if ( i .ne. offset ) then

               if ( type .eq. char ) then

                  call dasioc ( 'READ',  unit, drbase+offset, crec )
                  call dasioc ( 'WRITE', unit, drbase+i,      crec )

               else if ( type .eq. dp ) then

                  call dasiod ( 'READ',  unit, drbase+offset, drec )
                  call dasiod ( 'WRITE', unit, drbase+i,      drec )

               else

                  call dasioi ( 'READ',  unit, drbase+offset, irec )
                  call dasioi ( 'WRITE', unit, drbase+i,      irec )

               end if

               if ( failed () ) then
                  call chkout ( 'DASSDR' )
                  return
               end if

            end if

!
!           OFFSET is the index of the next order vector element to
!           look at.
!
            i  =  offset

            call dasrdi ( scrhan, base+i,       base+i,       offset )
            call dasrdi ( scrhan, base+i+total, base+i+total, type   )

            if ( failed () ) then
               call chkout ( 'DASSDR' )
               return
            end if

         end do

!
!        The last value of I is the location in the cycle that element
!        START followed.  Therefore, the saved record corresponding
!        to index START should be written to this location.
!
         if ( savtyp .eq. char ) then

            call dasioc ( 'WRITE',  unit,  drbase+i,  savec )

         else if ( savtyp .eq. dp ) then

            call dasiod ( 'WRITE',  unit,  drbase+i,  saved )

         else

            call dasioi ( 'WRITE',  unit,  drbase+i,  savei )

         end if
!
!        Mark the order vector element by writing its negative
!        back to the location it came from.
!
         call dasudi ( scrhan,  base+i,  base+i, -start )

         if ( failed () ) then
            call chkout ( 'DASSDR' )
            return
         end if
!
!        Update START so that it points to the first element of a cycle
!        of the order vector that has not yet been traversed.  This will
!        be the first positive element of the order vector in a location
!        indexed higher than the current value of START.  Note that
!        this way of updating START guarantees that we don't have to
!        backtrack to find an element in the next cycle.
!

         offset = -1

         do while ( ( offset .lt. 0 ) .and. ( start .lt. total ) )

            start  =  start  + 1

            call dasrdi (  scrhan,  base+start,  base+start,  offset  )

            if ( failed () ) then
               call chkout ( 'DASSDR' )
               return
            end if

         end do

!
!        At this point, START is the index of an element in the order
!        vector that belongs to a cycle where no routine has gone
!        before, or else START is the last index in the order vector,
!        in which case we're done.
!

      end do

!
!     At this point, the records in the DAS are organized as follows:
!
!        +----------------------------------+
!        |           File record            |  ( 1 )
!        +----------------------------------+
!        |         Reserved records         |  ( 0 or more )
!        |                                  |
!        +----------------------------------+
!        |          Comment records         |  ( 0 or more )
!        |                                  |
!        |                                  |
!        +----------------------------------+
!        |      First directory  record     |  ( 1 )
!        +----------------------------------+
!        |      Character data records      |  ( 0 or more )
!        |                                  |
!        +----------------------------------+
!        |   Double precision data records  |  ( 0 or more )
!        |                                  |
!        +----------------------------------+
!        |       Integer data records       |  ( 0 or more )
!        |                                  |
!        +----------------------------------+
!        |   Additional directory records   |  ( 0 or more )
!        |                                  |
!        +----------------------------------+
!
!
!     Not all of the indicated components must be present; only the
!     file record and first directory record will exist in all cases.
!     The `additional directory records' at the end of the file serve
!     no purpose; if more data is appended to the file, they will be
!     overwritten.
!
!     The last step in preparing the file is to fill in the first
!     directory record with the correct information, and to update
!     the file summary.
!
!
      recno  =  drbase + 1

      call cleari ( nwi, irec )

!
!     Set the logical address ranges in the directory record, for each
!     data type.
!

      do type = 1, 3

         maxadr  =  lastla(type)

         if ( maxadr .gt. 0 ) then
            minadr = 1
         else
            minadr = 0
         end if

         irec (  rngbas + (2*type) - 1  )  =  minadr
         irec (  rngbas + (2*type)      )  =  maxadr

      end do

!
!     Set the descriptors in the directory.  Determine which type
!     comes first:  the order of priority is character, double
!     precision, integer.
!
      pos = begdsc

      do type = 1, 3

         if (  lastla(type)  .gt.  0  ) then

            if ( pos .eq. begdsc ) then
!
!              This is the first type for which any data is present.
!              We must enter a type code at position BEGDSC in the
!              directory, and we must enter a count at position
!              BEGDSC+1.
!
               irec   ( begdsc       )  =  type
               irec   ( begdsc + 1   )  =  count(type)
               lastrc ( type )          =  recno
               lastwd ( type )          =  begdsc + 1
               pos                      =  pos + 2
               prvtyp                   =  type

            else
!
!              Place an appropriately signed count at location POS in
!              the directory.
!
               if ( type .eq. next(prvtyp) ) then
                  irec ( pos )  =   count(type)
               else
                  irec ( pos )  =  -count(type)
               end if

               lastrc ( type )  =  recno
               lastwd ( type )  =  pos
               pos              =  pos + 1
               prvtyp           =  type

            end if

         end if

      end do

!
!     Since we've done away with all but the first directory, the first
!     free record is decremented by 1 less than the directory count.
!
      free  =  free  -  count( dir )  +  1

!
!     Write out the new directory record.  Don't use the DAS buffered
!     write mechanism; this could trash the file by dumping buffered
!     records in the wrong places.
!
      call dasioi ( 'WRITE',  unit,  recno,  irec  )
!
!     Write out the updated file summary.
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
!
!     Clean up the DAS data buffers:  we don't want buffered scratch
!     file records hanging around there.  Then get rid of the scratch
!     file.
!
      call daswbr ( scrhan )
      call dasllc ( scrhan )


      call chkout ( 'DASSDR' )
      return
      end
