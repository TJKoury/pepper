!$Procedure T_URAND2 ( Mirror T_URAND )

      DOUBLE PRECISION FUNCTION T_URAND2 ( IDUM )

! $ Abstract
!
!     Alternate implementation of T_URAND.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     MATH
!     NUMBERS
!
! $ Declarations

      INTEGER               IDUM

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     IDUM      I-O  Seed for random number generation.
!
!     The function returns values that are uniformly distributed over
!     the open interval (0,1).
!
! $ Detailed_Input
!
!     IDUM           is a seed for the random number generator. On the
!                    first call to T_URAND, IDUM should be set equal to
!                    a negative number.  After the first call, IDUM
!                    should not be modified by the caller, except to
!                    start a new sequence of random numbers.
!
! $ Detailed_Output
!
!     IDUM           is the seed value to be used as input on the next
!                    call to T_URAND to produce the next random number
!                    in the sequence.
!
!     The function returns values that are approximately uniformly
!     distributed over the open interval (0, 1).
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine supports testing of T_URAND. This is simply
!     an independently coded version of the same algorithm used
!     by that routine.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     This routine is to be used only within TSPICE.
!
! $ Literature_References
!
!     [1]  "Numerical Recipes---The Art of Scientific Computing" by
!           William H. Press, Brian P. Flannery, Saul A. Teukolsky,
!           William T. Vetterling (pp 272-273).
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 17-OCT-2011 (NJB)
!
!-&



!
!     Local parameters
!
      INTEGER               IA1
      PARAMETER           ( IA1    = 40014 )

      INTEGER               IA2
      PARAMETER           ( IA2    = 40692 )


      INTEGER               IM1
      PARAMETER           ( IM1    = 2147483563 )

      INTEGER               IM2
      PARAMETER           ( IM2    = 2147483399 )

      INTEGER               IMM1
      PARAMETER           ( IMM1   = IM1 - 1 )

      INTEGER               IQ1
      PARAMETER           ( IQ1    = 53668 )

      INTEGER               IQ2
      PARAMETER           ( IQ2    = 52774 )

      INTEGER               IR1
      PARAMETER           ( IR1    = 12211 )

      INTEGER               IR2
      PARAMETER           ( IR2    = 3791 )

      INTEGER               NTAB
      PARAMETER           ( NTAB   = 32 )

      INTEGER               NDIV
      PARAMETER           ( NDIV   = 1 + (IMM1/NTAB) )



      DOUBLE PRECISION      AM
      PARAMETER           ( AM     = 1.D0/IM1 )

      DOUBLE PRECISION      EPS
      PARAMETER           ( EPS    = 1.2D-7 )

      DOUBLE PRECISION      RNMX
      PARAMETER           ( RNMX   = 1.D0 - EPS )


!
!     Local variables
!
      INTEGER               IDUM2
      INTEGER               IV     ( NTAB )
      INTEGER               IY
      INTEGER               J
      INTEGER               K

!
!     Saved variables
!
      SAVE                  IDUM2
      SAVE                  IV
      SAVE                  IY
!
!     Initial values
!
      DATA                  IDUM2 / 123456789 /
      DATA                  IV    / NTAB * 0  /
      DATA                  IY    / 0         /


!
!     Initialize the output to an invalid value.
!
      T_URAND2 = -1.D0

      IF ( IDUM .LE. 0 ) THEN
!
!        Initialize.
!
!        Be sure to prevent IDUM == 0.
!
         IDUM  = MAX ( -IDUM, 1 )

         IDUM2 = IDUM

!
!        Load the shuffle table (after 8 warm-ups).
!
         DO J = NTAB+8, 1, -1

            K    = IDUM / IQ1
            IDUM = IA1 * ( IDUM - (K * IQ1) ) - ( K * IR1 )

            IF ( IDUM .LT. 0 ) THEN
               IDUM = IDUM + IM1
            END IF

            IF ( J .LE. NTAB ) THEN
               IV(J) = IDUM
            END IF

         END DO

         IY = IV(1)

      END IF

!
!     Start here when not initializing.
!
      K    = IDUM / IQ1
!
!     Compute IDUM = MOD(IA1*IDUM,IM1) without
!     overflows by Schrage's method.
!
      IDUM = IA1 * ( IDUM - ( K * IQ1) ) -  ( K * IR1 )

      IF ( IDUM .LT. 0 ) THEN
         IDUM = IDUM + IM1
      END IF

      K     = IDUM2 / IQ2

!
!     Compute IDUM2 = MOD(IA2*IDUM2,IM2) likewise.
!
      IDUM2 = IA2 * ( IDUM2 - ( K * IQ2 ) ) - ( K * IR2 )

      IF ( IDUM2 .LT. 0 ) THEN
         IDUM2 = IDUM2 + IM2
      END IF

!
!     J will be in the range 1:NTAB
!
      J = 1 + IY/NDIV

!
!     Here IDUM is shuffled, IDUM and IDUM2 are
!     combined to generate output.
!
      IY    = IV(J) - IDUM2
      IV(J) = IDUM

      IF ( IY .LT. 1 ) THEN
         IY = IY + IMM1
      END IF

!
!     Because users don't expect endpoint values...
!
      T_URAND2 = MIN( AM * IY, RNMX )

      RETURN
      END

