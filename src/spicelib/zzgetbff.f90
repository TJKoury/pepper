!$Procedure ZZGETBFF ( Private --- Get Binary File Format )

      SUBROUTINE ZZGETBFF ( BFFID )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Fetch binary file format.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     PRIVATE
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE              'zzddhman.inc'

      INTEGER               BFFID

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     BFFID      O   Binary file format code for this system.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     BFFID      is an integer code that indicates the binary file
!                format that is determined to be native to this
!                platform.  Possible values are:
!
!                   BIGI3E
!                   LTLI3E
!                   VAXGFL
!                   VAXDFL
!
!                as defined in the include file 'zzddhman.inc'.
!
! $ Parameters
!
!     None.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1) If the platform on which this code is compiled does not
!        produce results that match any of the known binary file
!        formats, this routine sets BFFID to 0.
!
! $ Particulars
!
!     This simple program:
!
!        PROGRAM DPTEST
!
!        DOUBLE PRECISION      DPNUM
!        INTEGER               INNUM ( 2 )
!
!        EQUIVALENCE         ( DPNUM, INNUM )
!
!        DPNUM = 7.0D0
!
!        WRITE (*,*) DPNUM
!        WRITE (*,*) INNUM ( 1 )
!        WRITE (*,*) INNUM ( 2 )
!
!        END
!
!     produces the following results on these representative platforms:
!
!        Sun-Solaris (BIGI3E):
!
!           7.000000000
!           1075576832
!           0
!
!        PC-Linux (LTLI3E):
!
!           7.000000000
!           0
!           1075576832
!
!        Alpha-Gfloat (VAXGFL):
!
!           7.000000000
!           16444
!           0
!
!        Alpha-Dfloat (VAXDFL):
!
!           7.000000000
!           16864
!           0
!
!     This routine performs exactly the same decomposition of the
!     double precision number 7.0D0 into two integers.  The results
!     are checked against those displayed here, and if a match is
!     found, returned.
!
! $ Examples
!
!     See ZZDDHOPN for sample usage.
!
! $ Restrictions
!
!     This routine derives the binary file format ID for a particular
!     platform, but for verification purposes only.  ZZPLATFM should
!     be used to obtain the binary file format for the current platform.
!
! $ Author_and_Institution
!
!     F.S. Turner     (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 06-AUG-2002 (FST)
!
!
!-&

!
!     Local Parameters
!
      DOUBLE PRECISION      DPVALU
      PARAMETER           ( DPVALU = 7.0D0 )

!
!     Local Variables
!
      INTEGER               I

      DOUBLE PRECISION      DEQUIV
      INTEGER               IEQUIV ( 2 )

      EQUIVALENCE         ( DEQUIV, IEQUIV )

      INTEGER               INT1ST ( NUMBFF )
      INTEGER               INT2ND ( NUMBFF )

      DATA                  INT1ST(BIGI3E)  / 1075576832 /
      DATA                  INT2ND(BIGI3E)  / 0          /

      DATA                  INT1ST(LTLI3E)  / 0          /
      DATA                  INT2ND(LTLI3E)  / 1075576832 /

      DATA                  INT1ST(VAXGFL)  / 16444      /
      DATA                  INT2ND(VAXGFL)  / 0          /

      DATA                  INT1ST(VAXDFL)  / 16864      /
      DATA                  INT2ND(VAXDFL)  / 0          /

      SAVE                  INT1ST
      SAVE                  INT2ND

!
!     Copy DPVALU into the equivalenced DP, DEQUIV.
!
      DEQUIV = DPVALU

!
!     Examine the integer pairs, to identify the binary
!     file format.
!
      BFFID = 0

      DO I = 1, NUMBFF

         IF ( ( IEQUIV(1) .EQ. INT1ST(I) ) .AND. &
              ( IEQUIV(2) .EQ. INT2ND(I) )       ) THEN

            BFFID = I

         END IF

      END DO

      RETURN

      END
