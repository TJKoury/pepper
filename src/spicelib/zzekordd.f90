!$Procedure      ZZEKORDD ( Order of a double precision EK column )

      SUBROUTINE ZZEKORDD ( DVALS, NULLOK, NLFLGS, NVALS, IORDER )

! $ Abstract
!
!     Determine the order of elements in a double precision EK column,
!     using dictionary ordering on d.p. data values and array
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

      DOUBLE PRECISION      DVALS  ( * )
      LOGICAL               NULLOK
      LOGICAL               NLFLGS ( * )
      INTEGER               NVALS
      INTEGER               IORDER ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     DVALS      I   Array of d.p. column values.
!     NULLOK     I   Logical flag indicating whether nulls are allowed.
!     NLFLGS     I   Flags indicating whether column entries are null.
!     NVALS      I   Dimension of DVALS.
!     IORDER     O   Order vector for DVALS.
!
! $ Detailed_Input
!
!     DVALS          is an array of double precision EK column values,
!                    some of which may be null, if null values are
!                    permitted.  See the description of the input
!                    arguments NULLOK and NLFLGS below.
!
!     NULLOK         is a logical flag indicating whether column
!                    elements may be null.  If NULLOK is TRUE, then
!                    NLFLGS must be set to indicate the status of each
!                    element of DVALS.
!
!     NLFLGS         is an array of logical flags that indicate whether
!                    the corresponding elements of DVALS are null.
!                    NLFLGS is meaningful only when NULLOK is .TRUE.
!                    When NULLOK is .TRUE., the Ith element of DVALS is
!                    null if and only if the Ith element of NLFLGS
!                    is .TRUE.
!
!                    When NULLOK is .FALSE., all elements of DVALS are
!                    considered to be non-null.
!
!     NVALS          is the number of elements in the input array.
!
! $ Detailed_Output
!
!     IORDER         is the order vector for the input array.
!                    IORDER(1) is the index of the smallest element
!                    of DVALS; IORDER(2) is the index of the next
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
!     ZZEKORDD creates an order vector for an array of double precision
!     column values.  Null values are allowed.  The order relation used
!     is dictionary ordering on ordered pairs consisting of data
!     values and array indices:  if two input data values are equal,
!     the associated array indices determine the order relation of the
!     values, where the smaller index is considered to precede the
!     greater.
!
! $ Examples
!
!      1)  Sort the following list of values, some of which are
!          null:
!
!                  Value                         Null?
!             --------------             ---------------------
!             DVALS(1)  =  3             NLFLGS(1)  =  .FALSE.
!             DVALS(2)  =  1             NLFLGS(2)  =  .FALSE.
!             DVALS(3)  =  4             NLFLGS(3)  =  .TRUE.
!             DVALS(4)  =  5             NLFLGS(4)  =  .FALSE.
!             DVALS(5)  =  2             NLFLGS(5)  =  .TRUE.
!
!
!          The subroutine call
!
!              CALL ZZEKORDD ( DVALS, .TRUE., NLFLGS, 5, IORDER )
!
!          generates the output
!
!             IORDER(1)  =  3
!             IORDER(2)  =  5
!             IORDER(3)  =  2
!             IORDER(4)  =  1
!             IORDER(5)  =  4
!
!
!      2)  Given the same inputs values of DVALS and NLFLGS, the
!          subroutine call
!
!             CALL ZZEKORDD ( DVALS, .FALSE., NLFLGS, 5, IORDER )
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
!-     Beta Version 3.0.0, 08-SEP-1995 (NJB)
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
      DOUBLE PRECISION      D1
      DOUBLE PRECISION      D2

      INTEGER               GAP
      INTEGER               I
      INTEGER               I1
      INTEGER               I2
      INTEGER               J
      INTEGER               JG

      LOGICAL               N1
      LOGICAL               N2
      LOGICAL               OK


!
!     Statement functions
!
      LOGICAL               CMP1
      LOGICAL               CMP2
      LOGICAL               CMP3
      LOGICAL               CMP4
      LOGICAL               LE

      CMP1 ( D1, D2, I1, I2 )          =          ( D1 .LT. D2 ) &
                                          .OR. (  ( D1 .EQ. D2 ) .AND. &
                                                  ( I1 .LT. I2 )       )

      CMP2 ( I1, I2, N1, N2 )          =       ( N1 .AND. .NOT. N2  ) &
                                          .OR. (      ( N1 .AND. N2 ) &
                                                .AND. ( I1 .LT. I2  ) )


      CMP3 ( D1, D2, I1, I2, N1, N2 )  =        ( .NOT. ( N1 .OR. N2 ) ) &
                                         .AND. CMP1 (  D1, D2, I1, I2  )


      CMP4 ( D1, D2, I1, I2, N1, N2 )  = CMP2 ( I1, I2, N1, N2 ) &
                                         .OR. &
                                         CMP3 ( D1, D2, I1, I2, N1, N2 )


      LE (  D1,  D2,  I1,  I2,  OK,  N1,  N2  )  = &

              (  (.NOT. OK ) .AND.  CMP1 ( D1, D2, I1, I2 )          ) &
        .OR.  (  (      OK ) .AND.  CMP4 ( D1, D2, I1, I2, N1, N2 )  )


!
!     Begin with the initial ordering.
!
      DO I = 1, NVALS
         IORDER(I) = I
      END DO

!
!     Find the smallest element, then the next smallest, and so on.
!     This uses the Shell Sort algorithm, but swaps the elements of
!     the order vector instead of the array itself.
!
      GAP = NVALS / 2

      DO WHILE ( GAP .GT. 0 )

         DO I = GAP+1, NVALS


            J = I - GAP

            DO WHILE ( J .GT. 0 )

               JG = J + GAP

               IF (   LE (   DVALS ( IORDER(J)  ), &
                             DVALS ( IORDER(JG) ), &
                             IORDER(J), &
                             IORDER(JG), &
                             NULLOK, &
                             NLFLGS( IORDER(J)  ), &
                             NLFLGS( IORDER(JG) )   )    ) THEN
!
!                 Getting here means that
!
!                    DVALS(IORDER(J)) .LE. DVALS(IORDER(JG))
!
!                 according to our order relation.
!
                  J = 0

               ELSE

                  CALL SWAPI ( IORDER(J), IORDER(JG) )

               END IF

               J = J - GAP

            END DO


         END DO

         GAP = GAP / 2

      END DO

      RETURN
      END
