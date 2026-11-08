!$Procedure      ZZCLN ( Private --- clean up )

      subroutine zzcln ( lookat, nameat, &
                         namlst, datlst, nmpool, chpool, dppool )
      use pepper_kinds, only: ip

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

      implicit none
      integer(ip)           lbpool
      parameter           ( lbpool = -5 )

      integer(ip)           lookat
      integer(ip)           nameat
      integer(ip)           namlst ( * )
      integer(ip)           datlst ( * )
      integer(ip)           nmpool ( 2, lbpool:* )
      integer(ip)           chpool ( 2, lbpool:* )
      integer(ip)           dppool ( 2, lbpool:* )


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
      integer(ip)           next
      parameter           ( next   = 1 )

      integer(ip)           prev
      parameter           ( prev   = 2 )


      integer(ip)           head
      integer(ip)           tail
!
!     First perform the clean up function. This variable
!     has been corrupted so there's no point in hanging
!     on to it.
!
!     First remove the data...
!

      head = datlst( nameat )

      if      ( head < 0 ) then

         head = -head
         tail = -chpool(prev,head)
         call lnkfsl ( head, tail, chpool )

      else if ( head > 0 ) then

         tail = -dppool(prev,head)
         call lnkfsl ( head, tail, dppool )

      end if
!
!     Remove the sub-list head from the data list.
!
      datlst ( nameat ) = 0
!
!     If this was a singleton list remove the pointer to
!     the head of the list.
!
      head =  namlst(lookat)
      tail = -nmpool(prev,head)

      if ( head == tail ) then
         namlst(lookat) = 0
      else if ( namlst(lookat) == nameat ) then
         namlst(lookat) = nmpool(next,nameat)
      end if
!
!     Finally free up this node in the NMPOOL.
!
      head = nameat
      tail = nameat

      call lnkfsl ( head, tail, nmpool )

      return
      end
