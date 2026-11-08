!$Procedure REORDC ( Reorder a character array )

      subroutine reordc ( iorder, ndim, array )

! $ Abstract
!
!     Reorder the elements of an array of character strings according to
!     a given order vector.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ARRAY
!     SORT
!
! $ Declarations

      implicit none

      integer          iorder ( * )
      integer          ndim
      character*(*)    array  ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     IORDER     I   Order vector to be used to re-order ARRAY.
!     NDIM       I   Dimension of ARRAY.
!     ARRAY     I-O  Array to be re-ordered.
!
! $ Detailed_Input
!
!     IORDER   is the order vector to be used to re-order the input
!              array. The first element of IORDER is the index of
!              the first item of the re-ordered array, and so on.
!
!              Note that the order imposed by REORDC is not the
!              same order that would be imposed by a sorting
!              routine. In general, the order vector will have
!              been created (by one of the ORDER routines) for
!              a related array, as illustrated in the example below.
!
!     NDIM     is the number of elements in the input array.
!
!     ARRAY    on input, is an array containing some number of
!              elements in unspecified order.
!
! $ Detailed_Output
!
!     ARRAY    on output, is the same array, with the elements
!              re-ordered as specified by IORDER.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If NDIM < 2, this routine executes a no-op. This case is
!         not an error.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     REORDC uses a cyclical algorithm to re-order the elements of
!     the array in place. After re-ordering, element IORDER(1) of
!     the input array is the first element of the output array,
!     element IORDER(2) is the input array is the second element of
!     the output array, and so on.
!
!     The order vector used by REORDC is typically created for
!     a related array by one of the ORDER routines, as shown in
!     the example below.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Sort four related arrays (containing the names, masses,
!        integer ID codes, and flags indicating whether they have
!        a ring system, for a group of planets).
!
!
!        Example code begins here.
!
!
!              PROGRAM REORDC_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local constants.
!        C
!              INTEGER                 NDIM
!              PARAMETER             ( NDIM   = 8  )
!
!              INTEGER                 STRLEN
!              PARAMETER             ( STRLEN = 7  )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(STRLEN)      NAMES  ( NDIM )
!
!              DOUBLE PRECISION        MASSES ( NDIM )
!
!              INTEGER                 CODES  ( NDIM )
!              INTEGER                 I
!              INTEGER                 IORDER ( NDIM )
!
!              LOGICAL                 RINGS  ( NDIM )
!
!        C
!        C     Set the arrays containing the names, masses (given as
!        C     ratios to of Solar GM to barycenter GM), integer ID
!        C     codes, and flags indicating whether they have a ring
!        C     system.
!        C
!              DATA                    NAMES  /
!             .            'MERCURY', 'VENUS',  'EARTH',  'MARS',
!             .            'JUPITER', 'SATURN', 'URANUS', 'NEPTUNE' /
!
!              DATA                    MASSES /
!             .                       22032.080D0,   324858.599D0,
!             .                      398600.436D0,    42828.314D0,
!             .                   126712767.881D0, 37940626.068D0,
!             .                     5794559.128D0,  6836534.065D0 /
!
!              DATA                    CODES  / 199, 299, 399, 499,
!             .                                 599, 699, 799, 899 /
!
!              DATA                    RINGS  /
!             .                   .FALSE., .FALSE., .FALSE., .FALSE.,
!             .                   .TRUE.,  .TRUE.,  .TRUE., .TRUE.   /
!
!        C
!        C     Sort the object arrays by name.
!        C
!              CALL ORDERC ( NAMES,  NDIM, IORDER )
!
!              CALL REORDC ( IORDER, NDIM, NAMES  )
!              CALL REORDD ( IORDER, NDIM, MASSES )
!              CALL REORDI ( IORDER, NDIM, CODES  )
!              CALL REORDL ( IORDER, NDIM, RINGS  )
!
!        C
!        C     Output the resulting table.
!        C
!              WRITE(*,'(A)') ' Planet   Mass(GMS/GM)  ID Code  Rings?'
!              WRITE(*,'(A)') '-------  -------------  -------  ------'
!
!              DO I = 1, NDIM
!
!                 WRITE(*,'(A,F15.3,I9,L5)') NAMES(I), MASSES(I),
!             .                              CODES(I), RINGS(I)
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
!         Planet   Mass(GMS/GM)  ID Code  Rings?
!        -------  -------------  -------  ------
!        EARTH       398600.436      399    F
!        JUPITER  126712767.881      599    T
!        MARS         42828.314      499    F
!        MERCURY      22032.080      199    F
!        NEPTUNE    6836534.065      899    T
!        SATURN    37940626.068      699    T
!        URANUS     5794559.128      799    T
!        VENUS       324858.599      299    F
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
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 27-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!        Added complete code example.
!
!        Added entry #1 in $Exceptions section.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     reorder a character array
!
!-&


!
!     Local variables
!
      integer          start
      integer          index
      integer          hold

      integer          c
      character*1      temp


!
!     If the array doesn't have at least two elements, don't bother.
!
      if ( ndim .lt. 2 ) then
           return
      end if

!
!     What follows is pretty much the same as for the other REORD
!     routines. The character routine is somewhat special in that
!     the use of a temporary variable would cause strings longer
!     than the variable to be truncated. Rather than just declare
!     a giant character string, the entire algorithm will be repeated
!     for each character in each string. That is, the first characters
!     will be ordered, then the second characters, and so on. This
!     looks messy as hell, but the same number of operations are
!     involved (more or less).
!
      do c = 1, len ( array(1) )

!
!        START is the position in the order vector that begins the
!        current cycle. When all the switches have been made, START
!        will point to the end of the order vector.
!
         start = 1

         do while ( start .lt. ndim )

!
!           Begin with the element of input vector specified by
!           IORDER(START). Move it to the correct position in the
!           array, after saving the element it replaces to TEMP.
!           HOLD indicates the position of the array element to
!           be moved to its new position. After the element has
!           been moved, HOLD indicates the position of an available
!           space within the array.
!
            index = start
            temp  = array(index)(c:c)
            hold  = iorder(index)

!
!           As each slot in the output array is filled in, the sign
!           of the corresponding element in the order vector is changed
!           from positive to negative. This way, we know which elements
!           have already been ordered when looking for the beginning of
!           the next cycle.
!
!           Keep going until HOLD points to the first array element
!           moved during the current cycle. This ends the cycle.
!
            do while ( hold .ne. start )
               array(index)(c:c)  =  array(hold)(c:c)
               index              =  hold
               hold               =  iorder(hold)
               iorder(index)      = -iorder(index)
            end do

!
!           The last element in the cycle is restored from TEMP.
!
            array(index)(c:c) =  temp
            iorder(hold)      = -iorder(hold)

!
!           Begin the next cycle at the next element in the order
!           vector with a positive sign. (That is, the next one
!           that hasn't been moved.)
!
            do while ( iorder(start) .lt. 0  .and.  start .lt. ndim )
               start = start + 1
            end do

         end do

!
!        Restore the original signs of the elements of the order
!        vector, for the next go around.
!
         do index = 1, ndim
           iorder(index) = iabs ( iorder(index) )
         end do

      end do

      return
      end
