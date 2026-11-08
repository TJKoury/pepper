!$Procedure ZZSRFINI ( Private --- Surface-Code Hash Initialization )

      subroutine zzsrfini ( nornam, codes,  bodies, &
                            nvals,  maxval, snmhls, snmpol, &
                            snmidx, sidhls, sidpol, sididx )
      use pepper_kinds, only: ip

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Initialize the name-based and ID-based hashes used for efficient
!     access to surface-name mapping arrays. This routine should be
!     called by ZZSRFTRN and ZZSRFKER only.
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

      include              'srftrn.inc'
      include              'zzsrftrn.inc'

      character*(*)         nornam (          * )
      integer(ip)           codes  (          * )
      integer(ip)           bodies (          * )
      integer(ip)           nvals
      integer(ip)           maxval
      integer(ip)           snmhls (          * )
      integer(ip)           snmpol ( lbsngl : * )
      integer(ip)           snmidx (          * )
      integer(ip)           sidhls (          * )
      integer(ip)           sidpol ( lbsngl : * )
      integer(ip)           sididx (          * )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     NORNAM     I   Array of normalized surface names
!     CODES      I   Array of surface ID codes for NAMES/NORNAM
!     BODIES     I   Array of body ID codes
!     NVALS      I   Length of NAMES, NORNAM, and CODES arrays
!     MAXVAL     I   Size of the hash arrays
!     SNMHLS     O   Surface name-based hash head node pointer list
!     SNMPOL     O   Surface name-based hash node collision list
!     SNMIDX     O   Surface name-based hash index storage array
!     SIDHLS     O   Surface ID-based hash head node pointer list
!     SIDPOL     O   Surface ID-based hash node collision list
!     SIDIDX     O   Surface ID-based hash index storage array
!     LBSNGL     P   Lower bound of hash pool arrays
!     SFNMLN     P   Maximum length of surface name strings
!
! $ Detailed_Input
!
!     NORNAM    is the array of normalized surface names, made from
!               elements of NAMES by upper-casing, left-justifying, and
!               compressing groups of spaces to a single space. This
!               represents the canonical member of the equivalence
!               class to which each parallel entry in NAMES belongs.
!
!               This array is parallel to CODES and BODIES.
!
!     CODES     is the array of surface codes extracted. This array is
!               parallel to NAMES and NORNAM.
!
!     BODIES    is the array of body ID codes associated with the input
!               surface names.
!
!     NVALS     is the number of items contained in NAMES, NORNAM,
!               CODES.
!
!     MAXVAL    is the output hash size.
!
! $ Detailed_Output
!
!     All output arrays must be declared with the dimension MAXVAL.
!     MAXVAL must be greater than or equal to NVALS.
!
!     SNMHLS
!     SNMPOL    are the surface name-based hash head node pointer and
!               collision lists. Together with the arrays SNMIDX,
!               NORNAM and BODIES, they enable mapping pairs of
!               normalized surface names and body ID codes to surface
!               ID codes.
!
!     SNMIDX    is the surface name-based hash index storage array. It
!               maps nodes in the name collision list to entries in the
!               parallel NORNAM and BODIES arrays.
!
!     SIDHLS
!     SIDPOL    are the surface ID-based hash head node pointer and
!               collision lists. Together with the arrays SIDIDX,
!               CODES and BODIES, they enable mapping pairs of
!               surface ID codes and body ID codes to surface
!               names.
!
!     SIDIDX    is the surface ID-based hash index storage array. It
!               maps nodes in the ID collision list to entries in the
!               parallel CODES and BODIES arrays.
!
! $ Parameters
!
!     LBSNGL    is the lower bound of the hashes' collision list array.
!
!     SFNMLN    is the maximum length of a surface name. Defined in the
!               include file 'srftrn.inc'.
!
! $ Exceptions
!
!     This routine is meant to signal no errors under normal
!     circumstances. The only errors it can signal would be caused by
!     bugs.
!
!     1) If the input number of bodies NVALS is not less than or equal
!        to the size of the output hash, the error 'SPICE(BUG1)' will be
!        signaled.
!
!     2) If registering an ID in the output ID-based hash fails, the
!        error 'SPICE(BUG2)' will be signaled.
!
!     3) If registering a name in the output name-based hash fails,
!        the error 'SPICE(BUG3)' will be signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This is a utility routine used for initializing the hashes
!     facilitating efficient surface name-ID translation in ZZSRFTRN.
!
!     The order of mappings in the input arrays determines their
!     priority, with the mapping having the lowest priority being first
!     and the mapping with the highest priority being last.
!
!     If more than one entry with a particular normalized name and body
!     ID is present in the input arrays, only the latest entry is
!     registered in the name-based hash.
!
!     If more than one entry with a particular surface ID and body ID
!     is present in the input arrays, only the latest entry that maps
!     to a not-yet-registered normalized name is registered in the
!     ID-based hash. Registering IDs only for not-yet-registered names
!     achieves masking all IDs with the lower priority in cases when a
!     single normalized name and body ID map to more than one surface
!     ID.
!
! $ Examples
!
!     See the routine ZZSRFTRN.
!
! $ Restrictions
!
!     1)  This routine is intended only for use by ZZSRFTRN and
!         ZZSRFKER.
!
!     2)  All output hash arrays must be declared with the same
!         dimension which is greater than or equal to MAXVAL.
!
!     3)  The order of mappings in the input arrays determines the
!         priority, with the mapping with the lowest priority being the
!         first and the mapping with the highest priority being the
!         last.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     B.V. Semenov       (JPL)
!     M.J. Spencer       (JPL)
!     W.L. Taber         (JPL)
!     F.S. Turner        (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 05-NOV-2021 (NJB)
!
!        Made minor formatting changes to code.
!        Updated inline comments to clarify logic.
!
!-    SPICELIB Version 1.0.0, 03-DEC-2015 (NJB) (BVS) (EDW)
!
!-&


!
!     SPICELIB functions.
!
      integer(ip)           zzhash2
      integer(ip)           zzhashi

!
!     Hash control area items.
!
      integer(ip)           sizidx
      parameter           ( sizidx =   0 )

      integer(ip)           freidx
      parameter           ( freidx =  -1 )

!
!     Local Variables
!
      character*(sfnmln)    sqshnm

      integer(ip)           head
      integer(ip)           i
      integer(ip)           itemat
      integer(ip)           lookat
      integer(ip)           node

      logical               full
      logical               idnew
      logical               lfound
      logical               namnew

!
!     This routine uses discovery check-in.
!
!
!     Consistency check.
!
      if ( maxval .lt. nvals ) then

         call chkin  ( 'ZZSRFINI'                                )
         call setmsg ( 'There is an inconsistency between the ' &
         //            'number of input bodies and the size of ' &
         //            'the output hashes. The number of input ' &
         //            'bodies was #. The size of the output ' &
         //            'hashes was #.'                           )
         call errint ( '#', nvals                                )
         call errint ( '#', maxval                               )
         call sigerr ( 'SPICE(BUG1)'                             )
         call chkout ( 'ZZSRFINI'                                )
         return

      end if

!
!     Initialize output hashes. Set all collision list pointers
!     to 0, which is the null value.
!
      call zzhsiini ( maxval, sidhls, sidpol )
      call zzhscini ( maxval, snmhls, snmpol )

      call cleari ( sidpol(0), sidpol(1))
      call cleari ( snmpol(0), snmpol(1))

!
!     Loop through the input arrays to populate hashes. We do it
!     backwards to pick and register only the highest priority (latest)
!     values for each pair of normalized surface name and body ID code.
!
!     If multiple surface ID codes are associated with a name, only
!     the highest priority ID is associated with the name in the
!     mapping data structures.
!
      do i = nvals, 1, -1
!
!        Register this normalized surface name and body ID, but only if
!        the name from this pair is not already in the hash.
!
!        We must traverse the collision list for the normalized surface
!        name "manually," since we have to check the body ID for each
!        matching name.
!
!        Use hash function to get index of the head node. We apply the
!        hash function to a version of the normalized name that has
!        all blanks compressed out. (A normalized name may have single
!        blanks separating words comprising the name.)
!
         call cmprss ( ' ', 0, nornam(i), sqshnm )

         lookat = zzhash2( sqshnm, snmpol(sizidx) )

         head   = snmhls ( lookat )

!
!        Indicate name and body were not found to begin with.
!
         lfound  = .false.
         itemat  = 0
         namnew  = .true.

!
!        See if this normalized name and corresponding body ID are,
!        respectively, in the normalized name list and body ID list.
!        Note that the body ID list is not a parallel array to the
!        normalized name array: we use the name pool pointer array
!        SNMIDX to indicate the location of the body ID corresponding
!        to a name.
!
         node = head

         if ( node .gt. 0 ) then
!
!           Start at the head node and check each normalized name saved
!           for this hash value until we find a name and body ID that
!           match or run out of items in the collision list.
!
            do while (  ( node .gt. 0 ) .and. ( .not. lfound )  )

               lfound =       ( nornam( snmidx(node) ) .eq. nornam(i) ) &
                        .and. ( bodies( snmidx(node) ) .eq. bodies(i) )

               itemat = node
               node   = snmpol ( node )

            end do

            namnew = .not. lfound

         end if

!
!        ITEMAT is the node at which a name match was found by the
!        above loop, if a match was found. ITEMAT is the tail node of
!        the list if no match was found. It is 0 if the list is empty.
!
!        ITEMAT will be used below only if the list is non-empty and
!        no match was found, in which case ITEMAT is non-zero.
!
         if ( namnew ) then
!
!           We need to add the current normalized name and BODY ID
!           to the hash. Make sure there's room.
!
            full = ( snmpol(freidx) .gt. snmpol(sizidx) )

            if ( full ) then

               call chkin  ( 'ZZSRFINI'                          )
               call setmsg ( 'Could not add name # body ID # ' &
               //            'to the hash.'                      )
               call errch  ( '#', nornam(i)                      )
               call errint ( '#', bodies(i)                      )
               call sigerr ( 'SPICE(BUG2)'                       )
               call chkout ( 'ZZSRFINI'                          )
               return

            else
!
!              Store the item at the first free location in
!              the collision pool.
!
               node           = snmpol(freidx)
               snmpol(freidx) = snmpol(freidx) + 1

               if ( head .gt. 0 ) then
!
!                 Link the new entry at the tail of the applicable
!                 collision list. The index of the tail node is ITEMAT.
!
                  snmpol(itemat) = node

               else
!
!                 Insert the new head node into the head list.
!
                  snmhls(lookat) = node

               end if
!
!              Set the index in the data arrays for the new pool
!              entry.
!
               snmidx(node) = i

            end if

!
!           NAMNEW indicates that the Ith normalized name and body ID
!           pair was not in the hash prior to the above block of code.
!
!           We may have a situation when a single normalized surface
!           name and body ID pair maps to more than one surface ID. In
!           such cases we want to completely mask all surface IDs with
!           the lower priority. This is easy to do by simply not
!           attempting to register any more surface IDs if the name was
!           already registered due to a higher-indexed assignment.
!
!           Register this surface ID and body ID pair, but only if it
!           is not already in the hash.
!
!           We must traverse the collision list for the normalized
!           surface name "manually," since we have to check the body ID
!           for each matching surface ID.
!
!           Use hash function to get index of the head node.
!
            lookat = zzhashi( codes(i), sidpol(sizidx) )

            head   = sidhls ( lookat )

!
!           Indicate surface ID and body were not found to begin with.
!
            lfound  = .false.
            itemat  = 0
            idnew   = .true.

!
!           See if this surface ID and corresponding body ID are,
!           respectively, in the surface ID list and body ID list.
!
            node = head

            if ( node .gt. 0 ) then
!
!              Start at the head node and check each surface ID saved
!              for this hash value until we find a surface ID and body
!              ID that match or run out of items in this collision
!              list.
!
               do while (  ( node .gt. 0 ) .and. ( .not. lfound )  )

                  lfound =      ( codes (sididx(node)) .eq. codes (i) ) &
                          .and. ( bodies(sididx(node)) .eq. bodies(i) )

                  itemat = node
                  node   = sidpol ( node )

               end do

               idnew = .not. lfound

            end if

!
!           ITEMAT is the node at which a surface ID code match was
!           found by the above loop, if a match was found. ITEMAT is
!           the tail node of the list if no match was found. It is 0 if
!           the list is empty.
!
!           ITEMAT will be used below only if the list is non-empty and
!           no match was found, in which case ITEMAT is non-zero.
!
            if ( idnew ) then
!
!              We need to add the current surface ID and BODY ID
!              to the hash. Make sure there's room.
!
               full = ( sidpol(freidx) .gt. sidpol(sizidx) )

               if ( full ) then

                  call chkin  ( 'ZZSRFINI'                             )
                  call setmsg ( 'Could not add surface ID # body ID # ' &
                  //            'to the hash.'                         )
                  call errint ( '#', codes(i)                          )
                  call errint ( '#', bodies(i)                         )
                  call sigerr ( 'SPICE(BUG3)'                          )
                  call chkout ( 'ZZSRFINI'                             )
                  return

               else
!
!                 Store the item at the first free location in the
!                 collision pool.
!
                  node           = sidpol(freidx)
                  sidpol(freidx) = sidpol(freidx) + 1

                  if ( head .gt. 0 ) then
!
!                    Link the new entry at the tail of the applicable
!                    collision list. The index of the tail node is
!                    ITEMAT.
!
                     sidpol(itemat) = node

                  else
!
!                    Insert the new head node into the head list.
!
                     sidhls(lookat) = node

                  end if
!
!                 Set the index in the data arrays for the new pool
!                 entry.
!
                  sididx(node) = i

               end if

            end if
!
!           We've processed the new (surface ID, body ID) pair.
!
         end if
!
!        We've processed the Ith mapping between (surface name, body
!        ID) and (surface ID, body ID).
!
      end do

      return
      end
