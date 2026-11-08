!$Procedure      ZZEKORDC ( Order of a character EK column )

      SUBROUTINE ZZEKORDC ( CVALS, NULLOK, NLFLGS, NVALS, IORDER )

! $ Abstract
!
!     Determine the order of elements in a character EK column,
!     using dictionary ordering on character data values and array
!     indices.
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
!     EK
!
! $ Keywords
!
!     EK
!     SORT
!
! $ Declarations

      CHARACTER*(*)         CVALS  ( * )
      LOGICAL               NULLOK
      LOGICAL               NLFLGS ( * )
      INTEGER               NVALS
      INTEGER               IORDER ( * )

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
      INTEGER               GAP
      INTEGER               I
      INTEGER               I1
      INTEGER               I2
      INTEGER               J
      INTEGER               JG

      LOGICAL               EQ1
      LOGICAL               LE1
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


      CMP1 ( LE1, EQ1, I1, I2 )   =         ( LE1                      ) &
                                       .OR. ( EQ1 .AND. ( I1 .LT. I2 ) )


      CMP2 ( I1, I2, N1, N2 )     =         ( N1 .AND. .NOT. N2  ) &
                                       .OR. (      ( N1 .AND. N2 ) &
                                             .AND. ( I1 .LT.  I2 ) )

      CMP3 ( LE1, EQ1, I1, I2, N1, N2 )  =   ( .NOT. ( N1 .OR. N2 ) ) &
                                               .AND. &
                                             CMP1 ( LE1, EQ1, I1, I2 )


      CMP4 ( LE1, EQ1, I1, I2, N1, N2 )  =   CMP2 ( I1, I2, N1, N2 ) &
                                             .OR. &
                                        CMP3 (LE1, EQ1, I1, I2, N1, N2 )


      LE (  LE1,  EQ1,  I1,  I2,  OK,  N1,  N2  )  = &

              (  (.NOT. OK ) .AND.  CMP1 ( LE1, EQ1, I1, I2 )          ) &
        .OR.  (  (      OK ) .AND.  CMP4 ( LE1, EQ1, I1, I2, N1, N2 )  )



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

               JG    =   J + GAP

               LE1  =   LLE (   CVALS ( IORDER(J)  ), &
                                CVALS ( IORDER(JG) )   )

               EQ1  =   CVALS ( IORDER(J) ) .EQ.  CVALS ( IORDER(JG) )

               IF (   LE (   LE1, &
                             EQ1, &
                             IORDER(J), &
                             IORDER(JG), &
                             NULLOK, &
                             NLFLGS( IORDER(J)  ), &
                             NLFLGS( IORDER(JG) )   )    ) THEN
!
!                 Getting here means that
!
!                    CVALS(IORDER(J)) .LE. CVALS(IORDER(JG))
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
