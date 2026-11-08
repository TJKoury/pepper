!$Procedure ZZBODKER ( Private --- Process Body-Name Kernel Pool Maps )

      SUBROUTINE ZZBODKER ( NAMES,  NORNAM, CODES,  NVALS,  EXTKER, &
                            BNMLST, BNMPOL, BNMNMS, BNMIDX, &
                            BIDLST, BIDPOL, BIDIDS, BIDIDX  )

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
!     NAIF_IDS
!
! $ Keywords
!
!     BODY
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE               'zzbodtrn.inc'

      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = -5 )

      CHARACTER*(MAXL)      NAMES  (          NROOM )
      CHARACTER*(MAXL)      NORNAM (          NROOM )
      INTEGER               CODES  (          NROOM )
      INTEGER               NVALS
      LOGICAL               EXTKER
      INTEGER               BNMLST (          NROOM )
      INTEGER               BNMPOL ( LBPOOL : NROOM )
      CHARACTER*(MAXL)      BNMNMS (          NROOM )
      INTEGER               BNMIDX (          NROOM )
      INTEGER               BIDLST (          NROOM )
      INTEGER               BIDPOL ( LBPOOL : NROOM )
      INTEGER               BIDIDS (          NROOM )
      INTEGER               BIDIDX (          NROOM )

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
      LOGICAL               RETURN
      LOGICAL               FAILED

!
!     Local Parameters
!
      INTEGER               KEYLEN
      PARAMETER           ( KEYLEN = 32 )

!
!     Local Variables
!
      CHARACTER*(KEYLEN)    NBC
      CHARACTER*(KEYLEN)    NBN
      CHARACTER*(1)         TYPE   ( 2 )

      INTEGER               I
      INTEGER               NSIZ   ( 2 )
      INTEGER               NUM    ( 2 )

      LOGICAL               FOUND
      LOGICAL               PLFIND ( 2 )

!
!     Saved Variables
!
      SAVE                  NBC
      SAVE                  NBN

!
!     Data Statements
!
      DATA                  NBC    / 'NAIF_BODY_CODE' /
      DATA                  NBN    / 'NAIF_BODY_NAME' /

!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZBODKER' )
      END IF

!
!     Until the code below proves otherwise, we shall assume
!     we lack kernel pool name/code mappings.
!
      EXTKER = .FALSE.

!
!     Check for the external body ID variables in the kernel pool.
!
      CALL GCPOOL ( NBN, 1, NROOM, NUM(1), NAMES, PLFIND(1) )
      CALL GIPOOL ( NBC, 1, NROOM, NUM(2), CODES, PLFIND(2) )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZBODKER' )
         RETURN
      END IF

!
!     Examine PLFIND(1) and PLFIND(2) for problems.
!
      IF ( PLFIND(1) .NEQV. PLFIND(2) ) THEN

!
!        If they are not both present or absent, signal an error.
!
         CALL SETMSG ( 'The kernel pool vector, #, used in ' &
         //            'mapping between names and ID-codes ' &
         //            'is absent, while # is not.  This is ' &
         //            'often due to an improperly constructed ' &
         //            'text kernel.  Check loaded kernels for ' &
         //            'these keywords.'                         )

         IF ( PLFIND(1) ) THEN
            CALL ERRCH ( '#', NBC )
            CALL ERRCH ( '#', NBN )
         ELSE
            CALL ERRCH ( '#', NBN )
            CALL ERRCH ( '#', NBC )
         END IF

         CALL SIGERR ( 'SPICE(MISSINGKPV)' )
         CALL CHKOUT ( 'ZZBODKER'          )
         RETURN

      ELSE IF ( .NOT. PLFIND(1) ) THEN

!
!        Return if both keywords are absent.
!
         CALL CHKOUT ( 'ZZBODKER' )
         RETURN

      END IF

!
!     If we reach here, then both kernel pool variables are present.
!     Perform some simple sanity checks on their lengths.
!
      CALL DTPOOL ( NBN, FOUND, NSIZ(1), TYPE(1) )
      CALL DTPOOL ( NBC, FOUND, NSIZ(2), TYPE(2) )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZBODKER' )
         RETURN
      END IF

      IF ( ( NSIZ(1) .GT. NROOM ) .OR. ( NSIZ(2) .GT. NROOM ) ) THEN

         CALL SETMSG ( 'The kernel pool vectors used to ' &
         //            'define the names/ID-codes mapping' &
         //            'exceeds the max size. The size of ' &
         //            'the NAME vector is #1. The size of ' &
         //            'the CODE vector is #2. The max ' &
         //            'number allowed of elements is #3.'   )
         CALL ERRINT ( '#1', NSIZ(1)                         )
         CALL ERRINT ( '#2', NSIZ(2)                         )
         CALL ERRINT ( '#3', NROOM                           )
         CALL SIGERR ( 'SPICE(KERVARTOOBIG)'                 )
         CALL CHKOUT ( 'ZZBODKER'                            )
         RETURN

      ELSE IF ( NSIZ(1) .NE. NSIZ(2) ) THEN

         CALL SETMSG ( 'The kernel pool vectors used for ' &
         //            'mapping between names and ID-codes ' &
         //            'are not the same size.  The size ' &
         //            'of the name vector, NAIF_BODY_NAME ' &
         //            'is #. The size of the ID-code ' &
         //            'vector, NAIF_BODY_CODE is #. You ' &
         //            'need to examine the ID-code kernel ' &
         //            'you loaded and correct the mismatch.' )
         CALL ERRINT ( '#', NSIZ(1)                           )
         CALL ERRINT ( '#', NSIZ(2)                           )
         CALL SIGERR ( 'SPICE(BADDIMENSIONS)'                 )
         CALL CHKOUT ( 'ZZBODKER'                             )
         RETURN

      END IF

!
!     Compute the canonical member of the equivalence class of NAMES,
!     NORNAM. This normalization compresses groups of spaces into a
!     single space, left justifies the string, and upper-cases the
!     contents.  While passing through the NAMES array, look for any
!     blank strings and signal an appropriate error.
!
      NVALS = NUM(1)

      DO I = 1, NVALS

!
!        Check for blank strings.
!
         IF ( NAMES(I) .EQ. ' ' ) THEN

            CALL SETMSG ( 'An attempt to assign the code, #, to ' &
            //            'a blank string was made.  Check loaded ' &
            //            'text kernels for a blank string in ' &
            //            'the NAIF_BODY_NAME array.'               )
            CALL ERRINT ( '#', I                                    )
            CALL SIGERR ( 'SPICE(BLANKNAMEASSIGNED)'                )
            CALL CHKOUT ( 'ZZBODKER'                                )
            RETURN

         END IF

!
!        Compute the canonical member of the equivalence class.
!
         CALL LJUCRS ( 1, NAMES(I), NORNAM(I) )

      END DO

!
!     Populate hashes required by ZZBODTRN.
!
      CALL ZZBODINI ( NAMES,  NORNAM, CODES,  NVALS,  NROOM, &
                      BNMLST, BNMPOL, BNMNMS, BNMIDX, &
                      BIDLST, BIDPOL, BIDIDS, BIDIDX  )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZBODKER' )
         RETURN
      END IF


!
!     We're on the home stretch if we make it to this point. Set EXTKER
!     to .TRUE., check out and return.
!
      EXTKER = .TRUE.

      CALL CHKOUT ( 'ZZBODKER' )
      RETURN

      END
