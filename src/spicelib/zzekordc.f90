!$Procedure      ZZEKORDC ( Order of a character EK column )

      subroutine zzekordc ( cvals, nullok, nlflgs, nvals, iorder )

! $ Abstract
!
!     Determine the order of elements in a character EK column,
!     using dictionary ordering on character data values and array
!     indices.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     SORT
!
! $ Declarations

      character*(*)         cvals  ( * )
      logical               nullok
      logical               nlflgs ( * )
      integer               nvals
      integer               iorder ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     CVALS      I   Array of character string column values.
!     NULLOK     I   Logical flag indicating whether nulls are allowed.
!     NLFLGS     I   Flags indicating whether column entries are null.
!     NVALS      I   Dimension of CVALS.
!     IORDER     O   Order vector for CVALS.
!
! $ Detailed_Input
!
!     CVALS          is an array of character string EK column values,
!                    some of which may be null, if null values are
!                    permitted.  See the description of the input
!                    arguments NULLOK and NLFLGS below.
!
!     NULLOK         is a logical flag indicating whether column
!                    elements may be null.  If NULLOK is TRUE, then
!                    NLFLGS must be set to indicate the status of each
!                    element of CVALS.
!
!     NLFLGS         is an array of logical flags that indicate whether
!                    the corresponding elements of CVALS are null.
!                    NLFLGS is meaningful only when NULLOK is .TRUE.
!                    When NULLOK is .TRUE., the Ith element of CVALS is
!                    null if and only if the Ith element of NLFLGS
!                    is .TRUE.
!
!                    When NULLOK is .FALSE., all elements of CVALS are
!                    considered to be non-null.
!
!     NVALS          is the number of elements in the input array.
!
! $ Detailed_Output
!
!     IORDER         is the order vector for the input array.
!                    IORDER(1) is the index of the smallest element
!                    of CVALS; IORDER(2) is the index of the next
!                    smallest; and so on.  Null values, if allowed, are
!                    considered to be less than all non-null values.
!                    The order relation between equal values is
!                    determined by the indices of the values in the
!                    input array; values with lower indices are
!                    considered to be smaller.
!
! $ Parameters
!
!     None.
!
! $ Particulars
!
!     ZZEKORDC creates an order vector for an array of character
!     column values.  Null values are allowed.  The order relation used
!     is dictionary ordering on ordered pairs consisting of data
!     values and array indices:  if two input data values are equal,
!     the associated array indices determine the order relation of the
!     values, where the smaller index is considered to precede the
!     greater.
!
! $ Examples
!
!     1)  Sort the following list of values, some of which are null:
!
!                    Value                      Null?
!             ------------------         ---------------------
!             CVALS(1)  =  'CAT'         NLFLGS(1)  =  .FALSE.
!             CVALS(2)  =  'APT'         NLFLGS(2)  =  .FALSE.
!             CVALS(3)  =  'DOG'         NLFLGS(3)  =  .TRUE.
!             CVALS(4)  =  'EAT'         NLFLGS(4)  =  .FALSE.
!             CVALS(5)  =  'BAD'         NLFLGS(5)  =  .TRUE.
!
!
!         The subroutine call
!
!             CALL ZZEKORDC ( CVALS, .TRUE., NLFLGS, 5, IORDER )
!
!         generates the output
!
!             IORDER(1)  =  3
!             IORDER(2)  =  5
!             IORDER(3)  =  2
!             IORDER(4)  =  1
!             IORDER(5)  =  4
!
!
!
!     2)  Given the same inputs values of CVALS and NLFLGS, the
!         subroutine call
!
!             CALL ZZEKORDC ( CVALS, .FALSE., NLFLGS, 5, IORDER )
!
!         generates the output
!
!             IORDER(1)  =  2
!             IORDER(2)  =  5
!             IORDER(3)  =  1
!             IORDER(4)  =  3
!             IORDER(5)  =  4
!
! $ Restrictions
!
!      None.
!
! $ Exceptions
!
!      Error free.
!
! $ Files
!
!      None.
!
! $ Author_and_Institution
!
!      N.J. Bachman    (JPL)
!      I.M. Underwood  (JPL)
!
! $ Literature_References
!
!      None.
!
! $ Version
!
!-     Beta Version 3.0.0, 26-MAY-1995 (NJB)
!
!         Re-written to use dictionary ordering on values and input
!         array indices.
!
!-     Beta Version 2.0.0, 13-FEB-1995 (NJB)
!
!         Renamed as a private routine.
!
!-     Beta Version 1.0.0, 13-APR-1994 (NJB) (IMU)
!
!-&

! $ Index_Entries
!
!     order of a character EK column
!
!-&


!
!     Local variables
!
      integer               gap
      integer               i
      integer               i1
      integer               i2
      integer               j
      integer               jg

      logical               eq1
      logical               le1
      logical               n1
      logical               n2
      logical               ok


!
!     Statement functions
!
      logical               cmp1
      logical               cmp2
      logical               cmp3
      logical               cmp4
      logical               le


      cmp1 ( le1, eq1, i1, i2 )   =         ( le1                      ) &
                                       .or. ( eq1 .and. ( i1 .lt. i2 ) )


      cmp2 ( i1, i2, n1, n2 )     =         ( n1 .and. .not. n2  ) &
                                       .or. (      ( n1 .and. n2 ) &
                                             .and. ( i1 .lt.  i2 ) )

      cmp3 ( le1, eq1, i1, i2, n1, n2 )  =   ( .not. ( n1 .or. n2 ) ) &
                                               .and. &
                                             cmp1 ( le1, eq1, i1, i2 )


      cmp4 ( le1, eq1, i1, i2, n1, n2 )  =   cmp2 ( i1, i2, n1, n2 ) &
                                             .or. &
                                        cmp3 (le1, eq1, i1, i2, n1, n2 )


      le (  le1,  eq1,  i1,  i2,  ok,  n1,  n2  )  = &

              (  (.not. ok ) .and.  cmp1 ( le1, eq1, i1, i2 )          ) &
        .or.  (  (      ok ) .and.  cmp4 ( le1, eq1, i1, i2, n1, n2 )  )



!
!     Begin with the initial ordering.
!
      do i = 1, nvals
         iorder(i) = i
      end do

!
!     Find the smallest element, then the next smallest, and so on.
!     This uses the Shell Sort algorithm, but swaps the elements of
!     the order vector instead of the array itself.
!
      gap = nvals / 2

      do while ( gap .gt. 0 )

         do i = gap+1, nvals


            j = i - gap

            do while ( j .gt. 0 )

               jg    =   j + gap

               le1  =   lle (   cvals ( iorder(j)  ), &
                                cvals ( iorder(jg) )   )

               eq1  =   cvals ( iorder(j) ) .eq.  cvals ( iorder(jg) )

               if (   le (   le1, &
                             eq1, &
                             iorder(j), &
                             iorder(jg), &
                             nullok, &
                             nlflgs( iorder(j)  ), &
                             nlflgs( iorder(jg) )   )    ) then
!
!                 Getting here means that
!
!                    CVALS(IORDER(J)) .LE. CVALS(IORDER(JG))
!
!                 according to our order relation.
!
                  j = 0

               else

                  call swapi ( iorder(j), iorder(jg) )

               end if

               j = j - gap

            end do


         end do

         gap = gap / 2

      end do

      return
      end
