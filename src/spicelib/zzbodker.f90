!$Procedure ZZBODKER ( Private --- Process Body-Name Kernel Pool Maps )

      subroutine zzbodker ( names,  nornam, codes,  nvals,  extker, &
                            bnmlst, bnmpol, bnmnms, bnmidx, &
                            bidlst, bidpol, bidids, bididx  )
      use pepper_kinds, only: ip

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     This routine processes the kernel pool vectors NAIF_BODY_NAME
!     and NAIF_BODY_CODE into the lists and hashes required by ZZBODTRN
!     to successfully compute code-name mappings.
!
! $ Required_Reading
!
!     NAIF_IDS
!
! $ Keywords
!
!     BODY
!
! $ Declarations

      implicit none

      include               'zzbodtrn.inc'

      integer(ip)           lbpool
      parameter           ( lbpool = -5 )

      character*(maxl)      names  (          nroom )
      character*(maxl)      nornam (          nroom )
      integer(ip)           codes  (          nroom )
      integer(ip)           nvals
      logical               extker
      integer(ip)           bnmlst (          nroom )
      integer(ip)           bnmpol ( lbpool : nroom )
      character*(maxl)      bnmnms (          nroom )
      integer(ip)           bnmidx (          nroom )
      integer(ip)           bidlst (          nroom )
      integer(ip)           bidpol ( lbpool : nroom )
      integer(ip)           bidids (          nroom )
      integer(ip)           bididx (          nroom )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     NAMES      O   Array of kernel pool assigned names.
!     NORNAM     O   Array of normalized kernel pool assigned names.
!     CODES      O   Array of ID codes for NAMES/NORNAM.
!     NVALS      O   Length of NAMES, NORNAM, and CODES arrays.
!     EXTKER     O   Logical indicating presence of kernel pool names.
!     BNMLST     O   Body name-based hash head node pointer list
!     BNMPOL     O   Body name-based hash node collision list
!     BNMNMS     O   Body name-based hash item list
!     BNMIDX     O   Body name-based hash index storage array
!     BIDLST     O   Body ID-based hash head node pointer list
!     BIDPOL     O   Body ID-based hash node collision list
!     BIDIDS     O   Body ID-based hash item list
!     BIDIDX     O   Body ID-based hash index storage array
!     LBPOOL     P   Lower bound of hash pool arrays
!     MAXL       P   Maximum length of body name strings.
!     NROOM      P   Maximum length of kernel pool data vectors.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     NAMES     is the array of names extracted from the kernel pool
!               vector NAIF_BODY_NAME. This array is parallel to
!               NORNAM and CODES.
!
!     NORNAM    the array of names extracted from the kernel pool
!               vector NAIF_BODY_NAME.  After extraction, each entry is
!               converted to uppercase, and groups of spaces are
!               compressed to a single space. This represents the
!               canonical member of the equivalence class each parallel
!               entry in NAMES belongs.
!
!     CODES     the array of codes extracted from the kernel pool
!               vector NAIF_BODY_CODE.  This array is parallel to NAMES
!               and NORNAM.
!
!     NVALS     the number of items contained in NAMES, NORNAM, and
!               CODES.
!
!     EXTKER    is a logical that indicates to the caller whether any
!               kernel pool name-code maps have been defined. If EXTKER
!               is .FALSE., then the kernel pool variables
!               NAIF_BODY_CODE and NAIF_BODY_NAME are empty and only
!               the built-in and ZZBODDEF code-name mappings need
!               consideration. If .TRUE., then the values returned by
!               this module need consideration.
!
!     BNMLST
!     BNMPOL
!     BNMNMS    are the body name-based hash head node pointer, node
!               collision, and item lists. Together they return the
!               index of the element in the BNMIDX index storage array
!               that stores the index of the body items in the NAMES,
!               NORNAM, and CODES arrays.
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
!               that stores the index of the body items in the
!               NAMES, NORNAM, and CODES arrays.
!
!     BIDIDX    is the body ID-based hash index storage array
!               containing at the index determined by the hash for a
!               given ID the index corresponding to this ID in the
!               NAMES, NORNAM, and CODES arrays.
!
! $ Parameters
!
!     LBPOOL    is the lower bound of the hashes' collision list array.
!
!     MAXL      is the maximum length of a body name.  Defined in the
!               include file 'zzbodtrn.inc'.
!
!     NROOM     is the maximum number of kernel pool data items that
!               can be processed from the NAIF_BODY_CODE and
!               NAIF_BODY_NAME lists.
!
! $ Exceptions
!
!     1) The error SPICE(MISSINGKPV) is signaled when one of the
!        NAIF_BODY_CODE and NAIF_BODY_NAME keywords is present in the
!        kernel pool and the other is not.
!
!     2) The error SPICE(KERVARTOOBIG) is signaled if one or both of
!        the NAIF_BODY_CODE and NAIF_BODY_NAME kernel pool vectors
!        have a cardinality that exceeds NROOM.
!
!     3) The error SPICE(BADDIMENSIONS) is signaled if the cardinality
!        of the NAIF_BODY_CODE and NAIF_BODY_NAME kernel pool vectors do
!        not match.
!
!     4) The error SPICE(BLANKNAMEASSIGNED) is signaled if an entry
!        in the NAIF_BODY_NAME kernel pool vector is a blank string.
!        ID codes may not be assigned to a blank string.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine examines the contents of the kernel pool, ingests
!     the contents of the NAIF_BODY_CODE and NAIF_BODY_NAME keywords,
!     and produces name/code lists and hashes that ZZBODTRN requires to
!     resolve code to name and name to code mappings.
!
!     The NAMES and CODES arrays stored all values provided in the
!     corresponding POOL variables. No attempt to remove duplicates,
!     change order, or do any other alterations to these arrays is made
!     by this routine.
!
!     The order of mapping in the NAMES, NORNAM, and CODES arrays
!     determines the priority, with the mapping with the lowest
!     priority being first and the mapping with the highest priority
!     being last.
!
!     If more than one entry with a particular normalized name is
!     present in the NORNAM array, only the latest entry is registered
!     in the name-based hash.
!
!     If more than one entry with a particular ID is present in the
!     CODES array, only the latest entry that maps to a not-yet
!     registered normalized name is registered in the ID-based hash.
!     Registering IDs only for not-yet registered names achieves masking
!     all IDs with the lower priority in cases when a single normalized
!     name maps to more than one ID.
!
! $ Examples
!
!     None.
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
!     B.V. Semenov   (JPL)
!     F.S. Turner    (JPL)
!     E.D. Wright    (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.0, 16-SEP-2013 (BVS)
!
!        Changed routine's calling sequence by dropping name and ID
!        order vectors and adding name- and ID-based hashes and
!        modified it to initialize hashes instead of the order arrays.
!
!-    SPICELIB Version 1.0.0, 23-AUG-2002 (EDW) (FST)
!
!-&

!
!     SPICELIB Functions
!
      logical               return
      logical               failed

!
!     Local Parameters
!
      integer(ip)           keylen
      parameter           ( keylen = 32 )

!
!     Local Variables
!
      character*(keylen)    nbc
      character*(keylen)    nbn
      character*(1)         type   ( 2 )

      integer(ip)           i
      integer(ip)           nsiz   ( 2 )
      integer(ip)           num    ( 2 )

      logical               found
      logical               plfind ( 2 )

!
!     Saved Variables
!
      save                  nbc
      save                  nbn

!
!     Data Statements
!
      data                  nbc    / 'NAIF_BODY_CODE' /
      data                  nbn    / 'NAIF_BODY_NAME' /

!
!     Standard SPICE error handling.
!
      if ( return() ) then
         return
      else
         call chkin ( 'ZZBODKER' )
      end if

!
!     Until the code below proves otherwise, we shall assume
!     we lack kernel pool name/code mappings.
!
      extker = .false.

!
!     Check for the external body ID variables in the kernel pool.
!
      call gcpool ( nbn, 1, nroom, num(1), names, plfind(1) )
      call gipool ( nbc, 1, nroom, num(2), codes, plfind(2) )

      if ( failed() ) then
         call chkout ( 'ZZBODKER' )
         return
      end if

!
!     Examine PLFIND(1) and PLFIND(2) for problems.
!
      if ( plfind(1) .neqv. plfind(2) ) then

!
!        If they are not both present or absent, signal an error.
!
         call setmsg ( 'The kernel pool vector, #, used in ' &
         //            'mapping between names and ID-codes ' &
         //            'is absent, while # is not.  This is ' &
         //            'often due to an improperly constructed ' &
         //            'text kernel.  Check loaded kernels for ' &
         //            'these keywords.'                         )

         if ( plfind(1) ) then
            call errch ( '#', nbc )
            call errch ( '#', nbn )
         else
            call errch ( '#', nbn )
            call errch ( '#', nbc )
         end if

         call sigerr ( 'SPICE(MISSINGKPV)' )
         call chkout ( 'ZZBODKER'          )
         return

      else if ( .not. plfind(1) ) then

!
!        Return if both keywords are absent.
!
         call chkout ( 'ZZBODKER' )
         return

      end if

!
!     If we reach here, then both kernel pool variables are present.
!     Perform some simple sanity checks on their lengths.
!
      call dtpool ( nbn, found, nsiz(1), type(1) )
      call dtpool ( nbc, found, nsiz(2), type(2) )

      if ( failed() ) then
         call chkout ( 'ZZBODKER' )
         return
      end if

      if ( ( nsiz(1) .gt. nroom ) .or. ( nsiz(2) .gt. nroom ) ) then

         call setmsg ( 'The kernel pool vectors used to ' &
         //            'define the names/ID-codes mapping' &
         //            'exceeds the max size. The size of ' &
         //            'the NAME vector is #1. The size of ' &
         //            'the CODE vector is #2. The max ' &
         //            'number allowed of elements is #3.'   )
         call errint ( '#1', nsiz(1)                         )
         call errint ( '#2', nsiz(2)                         )
         call errint ( '#3', nroom                           )
         call sigerr ( 'SPICE(KERVARTOOBIG)'                 )
         call chkout ( 'ZZBODKER'                            )
         return

      else if ( nsiz(1) .ne. nsiz(2) ) then

         call setmsg ( 'The kernel pool vectors used for ' &
         //            'mapping between names and ID-codes ' &
         //            'are not the same size.  The size ' &
         //            'of the name vector, NAIF_BODY_NAME ' &
         //            'is #. The size of the ID-code ' &
         //            'vector, NAIF_BODY_CODE is #. You ' &
         //            'need to examine the ID-code kernel ' &
         //            'you loaded and correct the mismatch.' )
         call errint ( '#', nsiz(1)                           )
         call errint ( '#', nsiz(2)                           )
         call sigerr ( 'SPICE(BADDIMENSIONS)'                 )
         call chkout ( 'ZZBODKER'                             )
         return

      end if

!
!     Compute the canonical member of the equivalence class of NAMES,
!     NORNAM. This normalization compresses groups of spaces into a
!     single space, left justifies the string, and upper-cases the
!     contents.  While passing through the NAMES array, look for any
!     blank strings and signal an appropriate error.
!
      nvals = num(1)

      do i = 1, nvals

!
!        Check for blank strings.
!
         if ( names(i) .eq. ' ' ) then

            call setmsg ( 'An attempt to assign the code, #, to ' &
            //            'a blank string was made.  Check loaded ' &
            //            'text kernels for a blank string in ' &
            //            'the NAIF_BODY_NAME array.'               )
            call errint ( '#', i                                    )
            call sigerr ( 'SPICE(BLANKNAMEASSIGNED)'                )
            call chkout ( 'ZZBODKER'                                )
            return

         end if

!
!        Compute the canonical member of the equivalence class.
!
         call ljucrs ( 1, names(i), nornam(i) )

      end do

!
!     Populate hashes required by ZZBODTRN.
!
      call zzbodini ( names,  nornam, codes,  nvals,  nroom, &
                      bnmlst, bnmpol, bnmnms, bnmidx, &
                      bidlst, bidpol, bidids, bididx  )

      if ( failed() ) then
         call chkout ( 'ZZBODKER' )
         return
      end if


!
!     We're on the home stretch if we make it to this point. Set EXTKER
!     to .TRUE., check out and return.
!
      extker = .true.

      call chkout ( 'ZZBODKER' )
      return

      end
