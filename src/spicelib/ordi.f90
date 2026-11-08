!$Procedure ORDI ( The ordinal position of an element in a set )

      integer function ordi ( item, set )

! $ Abstract
!
!     Return the ordinal position of a given item in a set. If the
!     item does not appear in the set, return zero.
!
! $ Required_Reading
!
!     SETS
!
! $ Keywords
!
!     SEARCH
!     SETS
!
! $ Declarations

      implicit none

      integer               lbcell
      parameter           ( lbcell = - 5 )

      integer               item
      integer               set ( lbcell:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ITEM       I   An item to locate within a set.
!     SET        I   A set to search for a given item.
!
!     The function returns the ordinal position of ITEM within the SET.
!
! $ Detailed_Input
!
!     ITEM     is an integer value to be located within a set.
!
!     SET      is a properly validated SPICE set that is to be
!              searched for the occurrence of ITEM.
!
! $ Detailed_Output
!
!     The function returns the ordinal position of ITEM within SET.
!     Ordinal positions range from 1 to N, where N is the cardinality of
!     the set.
!
!     If ITEM is not an element of SET, the function return 0.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input set has invalid cardinality, an error is signaled
!         by a routine in the call tree of this routine. ORDI returns an
!         unspecified value in this case.
!
!     2)  If the input set has invalid size, an error is signaled by a
!         routine in the call tree of this routine. ORDI returns an
!         unspecified value in this case.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     A natural ordering can be imposed upon the elements of any
!     SPICE set, be it INTEGER, CHARACTER or DOUBLE PRECISION. For
!     character strings the ASCII collating sequence serves as the
!     ordering relation, for DOUBLE PRECISION and INTEGER variables
!     the arithmetic ordering is used.
!
!     Given any element of a set, its location within this ordered
!     sequence of elements is called its ordinal position within
!     the set.
!
!     For illustrative purposes suppose that SET represents the set
!
!              { 8, 1, 2, 9, 7, 4, 10 }
!
!     The ordinal position of:     8 is 5
!                                  1 is 1
!                                  2 is 2
!                                  9 is 6
!                                  7 is 4
!                                  4 is 3
!                                 10 is 7
!
!     Given an item of the SET, this routine returns its ordinal
!     position. If the item is not in the set, this function returns
!     a value of 0.
!
! $ Examples
!
!     Suppose that you wished to find the relative position of a value
!     in a large list of values stored within an array. Say we want
!     to know the relative position of item I of ARRAY withing the
!     set of values represented in ARRAY.
!
!     The following sequence of subroutine calls would allow you
!     determine the relative position of the value ARRAY(I).
!
!        INTEGER               N
!        PARAMETER           ( N = something useful )
!
!        INTEGER               ARRAY (         N )
!        INTEGER               SET   ( LBCELL: N )
!        INTEGER               I
!
!        INTEGER               NVALID
!        INTEGER               POSITION
!
!     set the value of NVALID to be the number of valid elements in the
!     array ARRAY
!
!        CALL MOVEI  ( ARRAY, N,      SET(1) )
!        CALL VALIDI ( N,     NVALID, SET    )
!
!        POSITION = ORDI ( ARRAY(I), SET )
!
!     POSITION now contains the ordinal position of ARRAY(I) within the
!     values represented in the array.
!
! $ Restrictions
!
!     1)  SET must be a validated or empty set.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     C.A. Curzon        (JPL)
!     J. Diaz del Rio    (ODC Space)
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 26-OCT-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Extended
!        $Detailed_Output and $Exceptions section.
!
!        Removed unnecessary $Revisions section.
!
!-    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
!
!        If the value of the function RETURN is .TRUE. upon execution of
!        this module, this function is assigned a default value of
!        either 0, 0.0D0, .FALSE., or blank depending on the type of the
!        function.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!         Comment section for permuted index source lines was added
!         following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB)
!
!-&


! $ Index_Entries
!
!     the ordinal position of an element in a set
!
!-&


!
!     SPICELIB functions
!
      integer               bsrchi
      integer               cardi
      logical               return
!
!     Standard error handling:
!
      if ( return() ) then
         ordi = 0
         return
      else
         call chkin ( 'ORDI' )
      end if
!
!     Given the structure of sets, there's not much to do.
!
      ordi = bsrchi ( item, cardi(set), set(1) )

      call chkout ( 'ORDI' )
      return
      end
