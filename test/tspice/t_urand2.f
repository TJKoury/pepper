C$Procedure T_URAND2 ( Mirror T_URAND )

      DOUBLE PRECISION FUNCTION T_URAND2 ( IDUM )

C$ Abstract
C
C     Alternate implementation of T_URAND. 
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     MATH
C     NUMBERS
C
C$ Declarations

      INTEGER               IDUM

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     IDUM      I-O  Seed for random number generation.
C     
C     The function returns values that are uniformly distributed over 
C     the open interval (0,1).  
C     
C$ Detailed_Input
C                    
C     IDUM           is a seed for the random number generator. On the
C                    first call to T_URAND, IDUM should be set equal to
C                    a negative number.  After the first call, IDUM
C                    should not be modified by the caller, except to
C                    start a new sequence of random numbers.
C
C$ Detailed_Output
C
C     IDUM           is the seed value to be used as input on the next
C                    call to T_URAND to produce the next random number
C                    in the sequence.
C
C     The function returns values that are approximately uniformly
C     distributed over the open interval (0, 1).
C     
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine supports testing of T_URAND. This is simply 
C     an independently coded version of the same algorithm used
C     by that routine.
C        
C$ Examples
C
C     None.
C            
C$ Restrictions
C
C     This routine is to be used only within TSPICE.
C
C$ Literature_References
C
C     [1]  "Numerical Recipes---The Art of Scientific Computing" by
C           William H. Press, Brian P. Flannery, Saul A. Teukolsky,
C           William T. Vetterling (pp 272-273).
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    TSPICE Version 1.0.0, 17-OCT-2011 (NJB) 
C
C-&



C
C     Local parameters
C
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


C
C     Local variables
C
      INTEGER               IDUM2
      INTEGER               IV     ( NTAB )
      INTEGER               IY
      INTEGER               J
      INTEGER               K

C
C     Saved variables
C
      SAVE                  IDUM2
      SAVE                  IV
      SAVE                  IY
C
C     Initial values
C
      DATA                  IDUM2 / 123456789 /
      DATA                  IV    / NTAB * 0  /
      DATA                  IY    / 0         /


C
C     Initialize the output to an invalid value.
C
      T_URAND2 = -1.D0

      IF ( IDUM .LE. 0 ) THEN
C
C        Initialize.
C        
C        Be sure to prevent IDUM == 0.
C        
         IDUM  = MAX ( -IDUM, 1 )

         IDUM2 = IDUM

C
C        Load the shuffle table (after 8 warm-ups).
C
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

C
C     Start here when not initializing.
C
      K    = IDUM / IQ1
C
C     Compute IDUM = MOD(IA1*IDUM,IM1) without
C     overflows by Schrage's method.
C
      IDUM = IA1 * ( IDUM - ( K * IQ1) ) -  ( K * IR1 )

      IF ( IDUM .LT. 0 ) THEN
         IDUM = IDUM + IM1
      END IF

      K     = IDUM2 / IQ2

C
C     Compute IDUM2 = MOD(IA2*IDUM2,IM2) likewise.
C
      IDUM2 = IA2 * ( IDUM2 - ( K * IQ2 ) ) - ( K * IR2 )
      
      IF ( IDUM2 .LT. 0 ) THEN
         IDUM2 = IDUM2 + IM2
      END IF

C
C     J will be in the range 1:NTAB
C
      J = 1 + IY/NDIV

C
C     Here IDUM is shuffled, IDUM and IDUM2 are
C     combined to generate output.
C
      IY    = IV(J) - IDUM2
      IV(J) = IDUM

      IF ( IY .LT. 1 ) THEN
         IY = IY + IMM1
      END IF

C
C     Because users don't expect endpoint values...
C
      T_URAND2 = MIN( AM * IY, RNMX )

      RETURN
      END

