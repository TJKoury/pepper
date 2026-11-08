!$Procedure      ZZCLN ( Private --- clean up )

      SUBROUTINE ZZCLN ( LOOKAT, NAMEAT, &
                         NAMLST, DATLST, NMPOOL, CHPOOL, DPPOOL )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     This routine cleans up changes to the kernel pool that were
!     made prior to the detection of a parsing error.  It is purely
!     a utility for use only by ZZRVAR.
!
! $ Required_Reading
!
!      None.
!
! $ Keywords
!
!       PRIVATE UTILITY
!
! $ Declarations

      IMPLICIT NONE
      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = -5 )

      INTEGER               LOOKAT
      INTEGER               NAMEAT
      INTEGER               NAMLST ( * )
      INTEGER               DATLST ( * )
      INTEGER               NMPOOL ( 2, LBPOOL:* )
      INTEGER               CHPOOL ( 2, LBPOOL:* )
      INTEGER               DPPOOL ( 2, LBPOOL:* )


! $ Brief_I/O
!
!      VARIABLE  I/O  DESCRIPTION
!      --------  ---  --------------------------------------------------
!
!      LOOKAT     I   The hash value of some name.
!      NAMEAT     I   The actual node where the name was stored
!      NAMLST    I/O  The array of heads of name lists.
!      DATLST    I/O  The array of heads of lists of values
!      NMPOOL    I/O  The linked list pool of variable names.
!      CHPOOL    I/O  The linked list pool of variable d.p. values.
!      DPPOOL    I/O  The linked list pool of variable string values.
!
!
! $ Detailed_Input
!
!     LOOKAT      is the hash value of some string.  NAMLST(LOOKAT) is
!                 the head of some collision resolution list of names.
!
!     NAMEAT      is the node in the list headed by NAMLST(LOOKAT) where
!                 some name has been stored in the kernel pool
!                 collection of NAMES. The node NAMEAT needs to be
!                 removed from its list in NMPOOL.
!
!     NAMLST      is an array of heads of collision
!                 resolution lists in NMPOOL.  If NAMLST(LOOKAT) is
!                 the same as NAMEAT, we need to adjust NAMLST(LOOKAT)
!                 so that it points to the next node in the list.
!
!     DATLST      is an array of heads of data value lists for the
!                 variables in the kernel pool.  We will need to free
!                 the data list pointed to by DATLST(NAMEAT) and
!                 zero out DATLST(NAMEAT).
!
!     NMPOOL      is a linked list pool for collision resolutions of
!                 a string hash function.  The node NAMEAT needs to
!                 be freed.
!
!     CHPOOL      is a linked list pool for string values associated
!                 with a kernel pool variable  If DATLST(NAMEAT) points
!                 into CHPOOL, then the list containing this node must
!                 be freed.
!
!     DPPOOL      is a linked list pool for d.p. values associated
!                 with a kernel pool variable. If DATLST(NAMEAT) points
!                 into DPPOOL, then the list containing this node must
!                 be freed.
!
!
! $ Detailed_Output
!
!      NAMLST    are the same structures as the input with the
!      DATLST    corrections made for the freeing of the NMPOOL
!      NMPOOL    node NAMEAT.
!      CHPOOL
!      DPPOOL
!
! $ Parameters
!
!      None.
!
! $ Files
!
!      None.
!
! $ Exceptions
!
!      None.
!
! $ Particulars
!
!     During the course of reading and parsing a kernel pool variable
!     it may happen that an error in the input text is encountered after
!     a kernel pool variable update has been initiated.  This routine
!     removes all traces of that variable from the kernel pool storage
!     structures.
!
! $ Examples
!
!     See ZZRVAR
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!      W.L. Taber      (JPL)
!
! $ Literature_References
!
!      None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 20-SEP-1995 (WLT)
!
!-&


!
!     Local Parameters and Variables
!
      INTEGER               NEXT
      PARAMETER           ( NEXT   = 1 )

      INTEGER               PREV
      PARAMETER           ( PREV   = 2 )


      INTEGER               HEAD
      INTEGER               TAIL
!
!     First perform the clean up function. This variable
!     has been corrupted so there's no point in hanging
!     on to it.
!
!     First remove the data...
!
      CALL CHKIN ( 'ZZCLN' )

      HEAD = DATLST( NAMEAT )

      IF      ( HEAD .LT. 0 ) THEN

         HEAD = -HEAD
         TAIL = -CHPOOL(PREV,HEAD)
         CALL LNKFSL ( HEAD, TAIL, CHPOOL )

      ELSE IF ( HEAD .GT. 0 ) THEN

         TAIL = -DPPOOL(PREV,HEAD)
         CALL LNKFSL ( HEAD, TAIL, DPPOOL )

      END IF
!
!     Remove the sub-list head from the data list.
!
      DATLST ( NAMEAT ) = 0
!
!     If this was a singleton list remove the pointer to
!     the head of the list.
!
      HEAD =  NAMLST(LOOKAT)
      TAIL = -NMPOOL(PREV,HEAD)

      IF ( HEAD .EQ. TAIL ) THEN
         NAMLST(LOOKAT) = 0
      ELSE IF ( NAMLST(LOOKAT) .EQ. NAMEAT ) THEN
         NAMLST(LOOKAT) = NMPOOL(NEXT,NAMEAT)
      END IF
!
!     Finally free up this node in the NMPOOL.
!
      HEAD = NAMEAT
      TAIL = NAMEAT

      CALL LNKFSL ( HEAD, TAIL, NMPOOL )

      CALL CHKOUT ('ZZCLN' )
      RETURN
      END
