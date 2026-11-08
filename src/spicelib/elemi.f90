!$Procedure ELEMI ( Element of an integer set )

      logical function elemi ( item, a )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Determine whether an item is an element of an integer set.
!
! $ Required_Reading
!
!     SETS
!
! $ Keywords
!
!     CELLS
!     SETS
!
! $ Declarations

      implicit none

      integer(ip)      lbcell
      parameter      ( lbcell = -5 )

      integer(ip)      item
      integer(ip)      a      ( lbcell:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ITEM       I   Item to be tested.
!     A          I   Set to be tested.
!
!     The function returns .TRUE. if ITEM is an element of set A.
!
! $ Detailed_Input
!
!     ITEM     is an item which may or may not be an element of
!              the input set.
!
!     A        is a SPICE set.
!
! $ Detailed_Output
!
!     The function returns .TRUE. if ITEM is a member of the set A,
!     and returns .FALSE. otherwise.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The LOGICAL functions ELEMC, ELEMD and ELEMI correspond to the
!     set operator IN in the Pascal language.
!
!     This routine uses a binary search to check for the presence in
!     the set of the specified ITEM.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Check if the elements of a list of integers belong to a given
!        integer set.
!
!
!        Example code begins here.
!
!
!              PROGRAM ELEMI_EX1
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions.
!        C
!              LOGICAL                 ELEMI
!
!        C
!        C     Local constants.
!        C
!              INTEGER                 LBCELL
!              PARAMETER             ( LBCELL = -5 )
!
!              INTEGER                 LISTSZ
!              PARAMETER             ( LISTSZ   = 6   )
!
!              INTEGER                 SETDIM
!              PARAMETER             ( SETDIM   = 8   )
!
!        C
!        C     Local variables.
!        C
!              INTEGER                 A      ( LBCELL:SETDIM )
!
!              INTEGER                 I
!              INTEGER                 ITEMS  ( LISTSZ        )
!
!        C
!        C     Set the values of the set and the list of integers.
!        C
!              DATA                  ( A(I), I=1,SETDIM)  /
!             .                           -1, 0, 1, 1, 3, 5, 0, -3 /
!
!              DATA                    ITEMS /  6, -1, 0, 2, 3, -2 /
!
!        C
!        C     Validate the set: Initialize the non-empty set, remove
!        C     duplicates and sort the elements.
!        C
!              CALL VALIDI ( SETDIM, SETDIM, A )
!
!        C
!        C     Check if the items in the list belong to the set.
!        C
!              DO I = 1, LISTSZ
!
!                 IF ( ELEMI ( ITEMS(I), A ) ) THEN
!
!                    WRITE(*,'(A,I4,A)') 'Item ', ITEMS(I),
!             .                          ' is in the set.'
!
!                 ELSE
!
!                    WRITE(*,'(A,I4,A)') 'Item ', ITEMS(I),
!             .                          ' is NOT in the set.'
!
!                 END IF
!
!              END DO
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        Item    6 is NOT in the set.
!        Item   -1 is in the set.
!        Item    0 is in the set.
!        Item    2 is NOT in the set.
!        Item    3 is in the set.
!        Item   -2 is NOT in the set.
!
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
!     C.A. Curzon        (JPL)
!     J. Diaz del Rio    (ODC Space)
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 24-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Added complete
!        code example.
!
!        Removed unnecessary $Revisions section.
!
!-    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
!
!        If the value of the function RETURN is .TRUE. upon execution of
!        this module, this function is assigned a default value of
!        either 0, 0.0D0, .FALSE., or blank depending on the type of
!        the function.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB)
!
!-&


! $ Index_Entries
!
!     element of an integer set
!
!-&


!
!     SPICELIB functions
!
      integer(ip)           bsrchi
      integer(ip)           cardi
      logical               return


!
!     Standard error handling:
!
      if ( return() ) then
         elemi = .false.
         return
      else
         call chkin ( 'ELEMI' )
      end if
!
!     Just a binary search.
!
      elemi = ( bsrchi ( item, cardi ( a ), a(1) ) /= 0 )

      call chkout ( 'ELEMI' )
      return
      end
