!$Procedure LOCATI ( Locate an identifier in a list )

      subroutine locati ( id, idsz, list, pool, at, presnt )

! $ Abstract
!
!     Find a given identifier, which consists of an integer array,
!     within a list of such identifiers, or insert the identifier
!     into the list. Return the location of the identifier and a flag
!     indicating whether or not the identifier was already present.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations

      implicit none

      integer               lbpool
      parameter           ( lbpool = -5 )

      integer               id   (            * )
      integer               idsz
      integer               list ( idsz,      * )
      integer               pool ( 2, lbpool: * )
      integer               at
      logical               presnt

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ID         I   An array of integers that comprise an identifier.
!     IDSZ       I   The number of integer components per identifier.
!     LIST      I-O  A list of known identifiers.
!     POOL      I-O  A doubly linked list used for search LIST.
!     AT        I-O  Location of the ID in the LIST.
!     PRESNT     O   Flag indicating if ID was already in LIST.
!
! $ Detailed_Input
!
!     ID       is an integer array that serves as an identifier
!              for some object. For example it might be a SPICE
!              id code for a planet or satellite; it might be the
!              instrument id and mode of operation of an instrument.
!              See the $Examples section for more details.
!
!     IDSZ     is the number of components in the array ID.
!
!     LIST     is an array containing several ID's. The array
!              should be declared so as to have the same upper
!              bound at least as large as the upper bound used
!              in the declaration of POOL.
!
!     POOL     is a linked list pool that gives the search order
!              for examining LIST to locate ID's. The declaration
!              of POOL and LIST need to be compatible. Normally,
!              the declaration should look like this:
!
!                 INTEGER   LIST (IDSZ,         LSTSIZ )
!                 INTEGER   POOL (   2, LBPOOL: LSTSIZ )
!
!              If POOL is declared with the statement
!
!                 INTEGER   POOL (   2, LBPOOL: PSIZE  )
!
!              then you must make sure that PSIZE is less than
!              or equal to LSTSIZ.
!
!              POOL should be initialized before the first
!              call to this routine with the SPICE routine
!              LNKINI.
!
!     AT       is a value that is set by this routine and that
!              you should never reset yourself. It points
!              to the head of the linked list used for
!              searching LIST. Changing AT will destroy the
!              link between POOL and LIST.
!
!              There is one exception to these restrictions.
!              The first call to this routine that occurs after
!              initializing POOL, AT may have any value. It will
!              be set upon output and from that time on, you should
!              not alter its value except by calling this routine
!              to do so.
!
! $ Detailed_Output
!
!     AT       on output AT points to the location in LIST
!              of ID.
!
!     PRESNT   is a logical flag. It indicates whether or not
!              ID was already present in the LIST when this
!              routine was called. If ID was already in LIST
!              PRESNT is returned with the value .TRUE. Otherwise
!              it is returned with the value .FALSE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the value of AT is less than zero or greater than
!         the declared size of POOL (except immediately after
!         initializing or re-initializing POOL), the
!         error SPICE(ADDRESSOUTOFBOUNDS) is signaled.
!
!     2)  If the linked list pool POOL is corrupted by a higher
!         level routine, a diagnosis of the problem will be
!         made by a routine called by this one.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine serves as a utility for managing the bookkeeping
!     needed when using a local buffering scheme which removes
!     the last used item when the local buffer becomes full.
!
!     It is primarily a programming utility. Unless you are dealing
!     with a problem very similar to the one just described, you
!     probably shouldn't be using this routine.
!
!     The example below illustrates the intended use of this
!     routine.
!
! $ Examples
!
!     Consider the following programming situation.
!
!     Suppose that a routine is being written that will
!     access large amounts of data stored in the SPICE
!     kernel pool. Kernel pool access requires overhead that
!     may be prohibitive under some circumstances. Buffering
!     data locally and only fetching data from the kernel pool
!     when it has not been buffered locally, may substantially
!     improve the performance of the routine being written.
!
!     However, since FORTRAN does not allow dynamic memory allocation
!     the local data storage must be set at compile time. As
!     a result the local data buffer might become full during
!     an execution of your program. If data for an item needs
!     to be fetched from the kernel pool once the buffer has become
!     full, you must either repeatedly call the kernel pool to fetch
!     the new data or overwrite some of the data in your local buffer.
!
!     This routine helps with the decisions of which items to
!     overwrite. In addition it always moves the last requested
!     item to the head of the index used for searching the buffered
!     ID's. In this way if the same item is needed many times
!     in succession, there will be very little overhead associated
!     with finding the item. Thus the routine spends its time
!     in computing the desired quantities, not in looking up the
!     parameters needed for the computation.
!
!     Below is a fragment of code that illustrates how this routine
!     should be used. In the situation outlined above. We'll suppose
!     that we are fetching MDLSIZ double precision numbers from the
!     kernel pool that are associated with the item
!
!         'BODYid_MAGMODEL'
!
!     And that we are computing something with this model data.
!
!
!        INTEGER               MDLSIZ
!        PARAMETER           ( MDLSIZ = xxxxxx )
!
!        We'll create room to buffer this data for 8 bodies.
!
!
!        INTEGER               PSIZE
!        PARAMETER           ( PSIZE = 8 )
!
!
!        The ID's we shall be using are 1-dimensional. They are body
!        ID's for planets or and their satellites.
!
!        INTEGER               IDSZ
!        PARAMETER           ( IDSZ = 1 )
!
!        INTEGER               AT
!        INTEGER               DIM
!        INTEGER               LIST   (   IDSZ,  PSIZE        )
!        INTEGER               POOL   (      2,  LBPOOL:PSIZE )
!
!        DOUBLE PRECISION      MAGMDL ( MDLSIZ,  PSIZE        )
!        DOUBLE PRECISION      MODEL  ( MDLSIZ                )
!
!        LOGICAL               FIRST
!        LOGICAL               PRESNT
!
!        SAVE
!
!        DATA                  FIRST / .TRUE. /
!
!
!        The block below handles initializing the linked list pool.
!
!        IF ( FIRST ) THEN
!
!           FIRST = .FALSE.
!
!           CALL LNKINI ( PSIZE, POOL )
!
!        END IF
!
!        See if the data associated with ID has already been
!        buffered.
!
!        CALL LOCATI ( ID, IDSZ, LIST, POOL, AT, PRESNT )
!
!        IF ( .NOT. PRESNT ) THEN
!
!           The data has not yet been buffered, look it up. Normally
!           you might want to check to see if the data exists and
!           handle things appropriately if it doesn't but this is just
!           to give you the idea...
!
!           CALL BODVCD ( ID, 'MAGMODEL', 3, DIM, MAGMDL ( 1, AT ) )
!
!        END IF
!
!        Put the model data into the array MODEL for ease of
!        reading the rest of the code.
!
!        CALL MOVED ( MAGMDL(1,AT), MDLSIZ, MODEL )
!
!
!        Now do whatever processing is needed ....
!
!     There are a few things to note about the code fragment above.
!     First the handling of the buffering of data was very easy.
!     Second, if this routine is called again using the same ID,
!     the buffer will already contain the needed model. Moreover
!     the routine LOCATI will return very quickly because the
!     ID will already be at the head of the list indexed by POOL.
!
!     You can also easily add an entry point to this routine that
!     will force it to look up data from the kernel pool on the
!     next call. All that needs to be done is re-initialize the
!     linked list pool.
!
!        ENTRY DOLOOK
!
!        CALL LNKINI ( PSIZE, POOL )
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
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.2, 26-OCT-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Updated
!        PRESNT short description in $Brief_I/O section.
!
!-    SPICELIB Version 1.0.1, 24-OCT-2005 (NJB)
!
!        Header update: changed reference to BODVAR to reference
!        to BODVCD.
!
!-    SPICELIB Version 1.0.0, 09-APR-1997 (WLT)
!
!-&


! $ Index_Entries
!
!     Locate an item in a linked list indexed list of items
!     Remove least recently used item buffering
!
!-&


!
!     Spicelib functions
!

      integer               lnknfn
      integer               lnksiz

!
!     Linked list parameters
!
      integer               next
      parameter           ( next = 1 )

      integer               prev
      parameter           ( prev = 2 )

!
!     Local Variables.
!

      integer               last
      integer               nfree
      integer               psize
      integer               i
      integer               head
      integer               new

      logical               more
      logical               same



      call chkin ( 'LOCATI' )
!
!     We begin by looking through the list of items at our disposal.
!     One way or the other we will need the number of free nodes
!     in the linked list.
!
      nfree = lnknfn(pool)
      psize = lnksiz(pool)

      if ( nfree .eq. psize ) then
!
!        There's nothing in the list so far. Allocate a
!        node and begin a list.
!
         call lnkan ( pool, at )

         do i = 1, idsz
            list(i,at) = id(i)
         end do

         presnt = .false.

         call chkout ( 'LOCATI' )
         return

      end if

      if ( at .le. 0 .or. at .gt. psize ) then

         call setmsg ( 'The input value for the head of the ID ' &
         //            'address linked list is out of bounds. ' &
         //            'It should be between 0 and #. The value ' &
         //            'supplied was #.' )
         call errint( '#', psize )
         call errint( '#', at    )
         call sigerr ( 'SPICE(ADDRESSOUTOFBOUNDS)'  )
         call chkout ( 'LOCATI' )
         return

      end if

!
!     If we are still here then there is actually something in
!     the list.  We begin at start and traverse the list.
!     Since we are unlikely to ever have large ID's (their purpose
!     after all is to be a label for something more complex) we
!     will handle the cases where IDSZ is 1 or 2 as special
!     cases since the tests for equality are a lot easier.
!
      same = .false.

      head =  at



      if ( idsz .eq. 1 ) then

         same = id(1) .eq. list(1,at)
         more = at .gt. 0 .and. .not. same

         do while ( more )

            at   =  pool ( next, at )

            if ( at .gt. 0 ) then
               same =  id(1) .eq. list(1,at)
               more = .not. same
            else
               more = .false.
            end if

         end do

      else if ( idsz .eq. 2 ) then

         same =       id(1) .eq. list(1,at) &
                .and. id(2) .eq. list(2,at)

         more = at .gt. 0 .and. .not. same

         do while ( more )

            at  = pool ( next, at )

            if ( at .gt. 0 ) then
               if (      id(1) .eq. list(1,at) ) then
                  same = id(2) .eq. list(2,at)
                  more = .not. same
               end if
            else
               more = .false.
            end if

         end do

      else

         i      =  1
         same   = .true.

         do while ( same .and. i .lt. idsz )
            same = same .and. id(i) .eq. list(i,at)
            i    = i + 1
         end do

         more = at .gt. 0 .and. .not. same

         do while ( more )

            at = pool ( next, at )

            if ( at .gt. 0 ) then
               i      =  1
               same   = .true.

               do while ( same .and. i .lt. idsz )
                  same = same .and. id(i) .eq. list(i,at)
                  i    = i + 1
               end do

               more = .not. same
            else
               more = .false.
            end if

         end do

      end if

!
!     The hunting is over either we found it or we need to
!     allocate space to put this ID into storage.
!
      if ( same ) then

         presnt = .true.
         last   =  pool(prev,at)


!
!        If AT is not already at the head of the list, we
!        move this node to the front of the list.
!
         if ( last .gt. 0 ) then
            call lnkxsl ( at, at,   pool )
            call lnkilb ( at, head, pool )
         end if

         call chkout ( 'LOCATI' )
         return

      end if

!
!     If we got to this point, we traversed the entire linked
!     list and did not find a matching ID.  AT is negative
!     and -AT points to the head of the list.
!
      presnt =  .false.
!
!     We'll put it in the list. First see if there are any free nodes.
!
      if ( nfree .gt. 0 ) then
!
!        Allocate a free node and put our ID at the NEW address.
!
         call lnkan ( pool, new )
         do i = 1, idsz
            list(i,new) = id(i)
         end do
!
!        Put the new node at the head of the linked list.
!
         call lnkilb ( new, head, pool )
         at =  new

      else

!
!        The last item in the list is pointed to as being the
!        previous item to the head of the list. But we have to
!        change the sign to get a legitimate address.  Overwrite
!        the ID information in this last slot of the list.
!
         last = -pool(prev,head)

         do i = 1, idsz
            list(i,last) = id(i)
         end do
!
!        Extract the last item as a sublist and insert it before
!        the current head of the list.
!
         call lnkxsl ( last,   last,   pool )
         call lnkilb ( last,   head,   pool )

         at    = last

      end if

      call chkout ( 'LOCATI' )
      return

      end
