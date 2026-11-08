!$Procedure      ZZEKORDI ( Order of an integer EK column )

      subroutine zzekordi ( ivals, nullok, nlflgs, nvals, iorder )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Determine the order of elements in an integer EK column, using
!     dictionary ordering on integer data values and array indices.
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

      integer(ip)           ivals  ( * )
      logical               nullok
      logical               nlflgs ( * )
      integer(ip)           nvals
      integer(ip)           iorder ( * )

! $ Brief_I/O
!
!      VARIABLE  I/O  DESCRIPTION
!      --------  ---  --------------------------------------------------
!      IVALS      I   Array of integer column values.
!      NULLOK     I   Logical flag indicating whether nulls are allowed.
!      NLFLGS     I   Flags indicating whether column entries are null.
!      NVALS      I   Dimension of IVALS.
!      IORDER     O   Order vector for IVALS.
!
! $ Detailed_Input
!
!      IVALS          is an array of integer EK column values,
!                     some of which may be null, if null values are
!                     permitted.  See the description of the input
!                     arguments NULLOK and NLFLGS below.
!
!      NULLOK         is a logical flag indicating whether column
!                     elements may be null.  If NULLOK is TRUE, then
!                     NLFLGS must be set to indicate the status of each
!                     element of IVALS.
!
!      NLFLGS         is an array of logical flags that indicate whether
!                     the corresponding elements of IVALS are null.
!                     NLFLGS is meaningful only when NULLOK is .TRUE.
!                     When NULLOK is .TRUE., the Ith element of IVALS is
!                     null if and only if the Ith element of NLFLGS
!                     is .TRUE.
!
!                     When NULLOK is .FALSE., all elements of IVALS are
!                     considered to be non-null.
!
!      NVALS          is the number of elements in the input array.
!
! $ Detailed_Output
!
!      IORDER      is the order vector for the input array.
!                  IORDER(1) is the index of the smallest element
!                  of IVALS; IORDER(2) is the index of the next
!                  smallest; and so on.  Null values, if allowed, are
!                  considered to be less than all non-null values.  The
!                  order relation between equal values is determined
!                  by the indices of the values in the input array;
!                  values with lower indices are considered to be
!                  smaller.
!
! $ Parameters
!
!     None.
!
! $ Particulars
!
!     ZZEKORDI creates an order vector for an array of integer
!     column values.  Null values are allowed.  The order
!     relation used is dictionary ordering on ordered pairs consisting
!     of data values and array indices:  if two input data values
!     are equal, the associated array indices determine the order
!     relation of the values, where the smaller index is considered
!     to precede the greater.
!
! $ Examples
!
!      1)  Sort the following list of values, some of which are
!          null:
!
!                  Value                         Null?
!             --------------             ---------------------
!             IVALS(1)  =  3             NLFLGS(1)  =  .FALSE.
!             IVALS(2)  =  1             NLFLGS(2)  =  .FALSE.
!             IVALS(3)  =  4             NLFLGS(3)  =  .TRUE.
!             IVALS(4)  =  5             NLFLGS(4)  =  .FALSE.
!             IVALS(5)  =  2             NLFLGS(5)  =  .TRUE.
!
!
!          The subroutine call
!
!              CALL ZZEKORDI ( IVALS, .TRUE., NLFLGS, 5, IORDER )
!
!          generates the output
!
!             IORDER(1)  =  3
!             IORDER(2)  =  5
!             IORDER(3)  =  2
!             IORDER(4)  =  1
!             IORDER(5)  =  4
!
!          Note that the order of the null values is determined by
!          their indices in the input array.
!
!
!      2)  Given the same inputs values of IVALS and NLFLGS, the
!          subroutine call
!
!             CALL ZZEKORDI ( IVALS, .FALSE., NLFLGS, 5, IORDER )
!
!          generates the output
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



!
!     Local variables
!
      integer(ip)           i1
      integer(ip)           i2
      integer(ip)           i3
      integer(ip)           i4

      integer(ip)           gap
      integer(ip)           i
      integer(ip)           j
      integer(ip)           jg

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


      cmp1 ( i1, i2, i3, i4 )          =          ( i1 < i2 ) &
                                          .or. (  ( i1 == i2 ) .and. &
                                                  ( i3 < i4 )       )


      cmp2 ( i3, i4, n1, n2 )          =       ( n1 .and. .not. n2  ) &
                                          .or. (      ( n1 .and. n2 ) &
                                                .and. ( i3 < i4  ) )


      cmp3 ( i1, i2, i3, i4, n1, n2 )  =        ( .not. ( n1 .or. n2 ) ) &
                                         .and. cmp1 (  i1, i2, i3, i4  )


      cmp4 ( i1, i2, i3, i4, n1, n2 )  = cmp2 ( i3, i4, n1, n2 ) &
                                         .or. &
                                         cmp3 ( i1, i2, i3, i4, n1, n2 )


      le (  i1,  i2, i3, i4, ok,   n1,   n2  )  = &

              (  (.not. ok ) .and.  cmp1 ( i1, i2, i3, i4 )          ) &
        .or.  (  (      ok ) .and.  cmp4 ( i1, i2, i3, i4, n1, n2 )  )



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

      do while ( gap > 0 )

         do i = gap+1, nvals


            j = i - gap

            do while ( j > 0 )

               jg = j + gap

               if (   le (   ivals ( iorder(j)  ), &
                             ivals ( iorder(jg) ), &
                             iorder(j), &
                             iorder(jg), &
                             nullok, &
                             nlflgs( iorder(j)  ), &
                             nlflgs( iorder(jg) )   )    ) then
!
!                 Getting here means that
!
!                    IVALS(IORDER(J)) .LE. IVALS(IORDER(JG))
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
