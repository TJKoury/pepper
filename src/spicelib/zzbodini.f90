!$Procedure ZZBODINI ( Private --- Body-Code Hash Initialization )

      subroutine zzbodini (  names,  nornam, codes,  nvals, maxval, &
                             bnmlst, bnmpol, bnmnms, bnmidx, &
                             bidlst, bidpol, bidids, bididx  )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Initialize the name-based and ID-based hashes used for efficient
!     access to body-name mapping arrays. This routine should be called
!     by ZZBODTRN and ZZBODKER only.
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

      include              'zzbodtrn.inc'

      integer               lbpool
      parameter           ( lbpool = -5 )

      character*(maxl)      names  (          * )
      character*(maxl)      nornam (          * )
      integer               codes  (          * )
      integer               nvals
      integer               maxval
      integer               bnmlst (          * )
      integer               bnmpol ( lbpool : * )
      character*(maxl)      bnmnms (          * )
      integer               bnmidx (          * )
      integer               bidlst (          * )
      integer               bidpol ( lbpool : * )
      integer               bidids (          * )
      integer               bididx (          * )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     NAMES      I   Array of names
!     NORNAM     I   Array of normalized names
!     CODES      I   Array of ID codes for NAMES/NORNAM
!     NVALS      I   Length of NAMES, NORNAM, and CODES arrays
!     MAXVAL     I   Size of the hash arrays
!     BNMLST     O   Body name-based hash head node pointer list
!     BNMPOL     O   Body name-based hash node collision list
!     BNMNMS     O   Body name-based hash item list
!     BNMIDX     O   Body name-based hash index storage array
!     BIDLST     O   Body ID-based hash head node pointer list
!     BIDPOL     O   Body ID-based hash node collision list
!     BIDIDS     O   Body ID-based hash item list
!     BIDIDX     O   Body ID-based hash index storage array
!     LBPOOL     P   Lower bound of hash pool arrays
!     MAXL       P   Maximum length of body name strings
!
! $ Detailed_Input
!
!     NAMES     is the array of body names. This array is parallel to
!               NORNAM and CODES.
!
!     NORNAM    is the array of normalized body names, made from
!               elements of NAMES by upper-casing, left-justifying, and
!               compressing groups of spaces to a single space. This
!               represents the canonical member of the equivalence
!               class each parallel entry in NAMES belongs.
!
!     CODES     is the array of body codes extracted. This array is
!               parallel to NAMES and NORNAM.
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
!     BNMLST
!     BNMPOL
!     BNMNMS    are the body name-based hash head node pointer, node
!               collision, and item lists. Together they return the
!               index of the element in the BNMIDX index storage array
!               that stores the index of the body items in the input
!               storage arrays.
!
!     BNMIDX    is the body name-based hash index storage array
!               containing at the index determined by the hash for a
!               given normalized name the index corresponding to this
!               name in the NAMES, NORNAM, and CODES arrays.
!
!     BIDLST
!     BIDPOL
!     BIDIDS    are the body ID-based hash head node pointer, node
!               collision, and item lists. Together they return the
!               index of the element in the BNMIDX index storage array
!               that stores the index of the body items in the input
!               storage arrays.
!
!     BIDIDX    is the body ID-based hash index storage array
!               containing at the index determined by the hash for a
!               given ID the index corresponding to the same ID in the
!               NAMES, NORNAM, and CODES arrays.
!
! $ Parameters
!
!     LBPOOL    is the lower bound of the hashes' collision list array.
!
!     MAXL      is the maximum length of a body name. Defined in the
!               include file 'zzbodtrn.inc'.
!
! $ Exceptions
!
!     1) If the input number of bodies NVALS is not less than or equal
!        to the size of the output hash, the error 'SPICE(BUG1)' will be
!        signaled.
!
!     2) If registering an ID in the output ID-based hash fails, the
!        error 'SPICE(BUG2)' will be signaled.
!
!     3) If registering an name in the output name-based hash fails,
!        the error 'SPICE(BUG3)' will be signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This is a utility routine used for initializing the hashes
!     facilitating efficient body name-ID translation in ZZBODTRN.
!
!     The order of mapping in the input arrays determines the priority,
!     with the mapping with the lowest priority being first and the
!     mapping with the highest priority being last.
!
!     If more than one entry with a particular normalized name is
!     present in the input arrays, only the latest entry is registered
!     in the name-based hash.
!
!     If more than one entry with a particular ID is present in the
!     input arrays, only the latest entry that maps to a not-yet
!     registered normalized name is registered in the ID-based hash.
!     Registering IDs only for not-yet registered names achieves masking
!     all IDs with the lower priority in cases when a single normalized
!     name maps to more than one ID.
!
! $ Examples
!
!     See the routine ZZBODTRN.
!
! $ Restrictions
!
!     1) This routine is intended only for use by ZZBODTRN and
!        ZZBODKER.
!
!     2) All output hash arrays must be declared with the same dimension
!        which is greater than or equal to MAXVAL.
!
!     3) The order of mappings in the input arrays determines the
!        priority, with the mapping with the lowest priority being
!        the first and the mapping with the highest priority being
!        the last.
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
!-    SPICELIB Version 4.0.0, 16-SEP-2013 (BVS)
!
!        Changed routine's calling sequence by dropping name and ID
!        order vectors and adding name- and ID-based hashes and
!        modified it to initialize hashes instead of the order arrays.
!
!-    SPICELIB Version 3.0.0, 23-AUG-2002 (FST)
!
!        Implemented changes to support the new precedence
!        system.
!
!        Altered the calling sequence of ZZBODINI to remove
!        unused arguments.  This routine also no longer computes
!        NORNAM from NAMES, since it is used in a more general
!        capacity.
!
!        Updated module header and comments to document additional
!        assumptions this module now makes about its inputs.
!
!        This routine is now error free.
!
!-    SPICELIB Version 2.1.1, 07-MAR-2002 (EDW)
!
!        Modified error logic to allow duplicate
!        NAME -> CODE mappings without signaling an error.
!        The mapping operation is a no-op, but might
!        cause a user problems if an error signals.
!
!-    SPICELIB Version 2.1.0, 12-AUG-2001 (EDW)
!
!        Modified logic for all ZZBOD routines to function with
!        equivalence class concept. A body name now exists
!        as a member of an equivalence class named by the
!        normalized form of the body name. To facilitate this
!        concept, an addition name vector, NORNAM, and
!        order vector, ORDNOM, now exist.
!
!-    SPICELIB Version 1.0.0, 25-SEP-1995 (BVS) (WLT)
!
!        Renamed to ZZBODINI and filled out the comments on what this
!        routine does and how it works.
!
!-&

!
!     Local Variables
!
      integer               i
      integer               item

      logical               new

!
!     Consistency check.
!
      if ( maxval .lt. nvals ) then
         call chkin  ( 'ZZBODINI' )
         call setmsg ( 'There is an inconsistency between the '  // &
                       'number of input bodies and the size of ' // &
                       'the output hashes. The number of input ' // &
                       'bodies was #. The size of the output '   // &
                       'hashes was #.' )
         call errint ( '#', nvals  )
         call errint ( '#', maxval )
         call sigerr ( 'SPICE(BUG1)' )
         call chkout ( 'ZZBODINI' )
         return
      end if

!
!     Initialize output hashes.
!
      call zzhsiini ( maxval, bidlst, bidpol )
      call zzhscini ( maxval, bnmlst, bnmpol )

!
!     Loop through the input arrays to populate hashes. We do it
!     backwards to pick and register only the highest priority (latest)
!     values for each normalized name.
!
      do i = nvals, 1, -1

!
!        Register this normalized name, but only if it is not already
!        in the hash.
!
         call zzhscadd ( bnmlst, bnmpol, bnmnms, nornam(i), item, new )
         if ( new ) then
            if ( item .ne. 0 ) then
               bnmidx( item ) = i
            else
               call chkin  ( 'ZZBODINI' )
               call setmsg ( 'Could not add name # to the hash.' )
               call errch  ( '#', nornam(i) )
               call sigerr ( 'SPICE(BUG3)' )
               call chkout ( 'ZZBODINI' )
            end if
         end if

!
!        We may have a situation when a single normalized name maps to
!        more than one ID. In such cases we want to completely mask all
!        IDs with the lower priority. This is easy to do by simply not
!        attempting to register any more IDs if the name is already
!        registered.
!
         if ( new ) then

!
!           Register this ID, but only if it is not already in the hash.
!
            call zzhsiadd ( bidlst, bidpol, bidids, codes(i), item, new)
            if ( new ) then
               if ( item .ne. 0 ) then
                  bididx( item ) = i
               else
                  call chkin  ( 'ZZBODINI' )
                  call setmsg ( 'Could not add ID # to the hash.' )
                  call errint ( '#', codes(i) )
                  call sigerr ( 'SPICE(BUG2)' )
                  call chkout ( 'ZZBODINI' )
                  return
               end if
            end if

         end if

      end do

      return
      end
