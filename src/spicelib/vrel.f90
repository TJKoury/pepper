!$Procedure VREL ( Vector relative difference, 3 dimensions )

      double precision function vrel ( v1, v2  )

! $ Abstract
!
!     Return the relative difference between two 3-dimensional vectors.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     MATH
!     VECTOR
!
! $ Declarations

      implicit none

      double precision      v1 ( 3 )
      double precision      v2 ( 3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     V1,
!     V2         I   Input vectors.
!
!     The function returns the relative difference between two
!     3-dimensional vectors.
!
! $ Detailed_Input
!
!     V1,
!     V2       are two 3-dimensional vectors for which the relative
!              difference is to be computed.
!
! $ Detailed_Output
!
!     The function returns the relative difference between the two input
!     3-dimensional vectors V1 and V2.
!
!     It is defined as:
!
!                          || V1 - V2 ||
!        VREL   =   ------------------------
!                    MAX ( ||V1||, ||V2|| )
!
!     where ||X|| indicates the Euclidean norm of the vector X.
!
!     VREL assumes values in the range [0,2]. If both V1 and V2 are zero
!     vectors then VREL is defined to be zero.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If both V1 and V2 are zero vectors, then VREL is defined
!         to be zero.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This function computes the relative difference between two
!     3-dimensional vectors as defined above.
!
!     The function VRELG may be used to find the relative difference
!     for two vectors of general dimension.
!
! $ Examples
!
!     This example code fragment computes the relative difference
!     between the geometric and light time corrected state of Io
!     with respect to Voyager 2 at a given UTC time.
!
!     C
!     C     The NAIF integer code for Io is 501 and the code for
!     C     Voyager 2 is -32.
!     C
!
!           INTEGER               IO
!           PARAMETER           ( IO  = 501 )
!
!           INTEGER               VG2
!           PARAMETER           ( VG2 = -32 )
!
!     C
!     C     SPICELIB function
!     C
!           DOUBLE PRECISION      VREL
!     C
!     C     Local variables
!     C
!           DOUBLE PRECISION      STATE ( 6 )
!           DOUBLE PRECISION      POS1  ( 3 )
!           DOUBLE PRECISION      POS2  ( 3 )
!           DOUBLE PRECISION      DIFF
!           DOUBLE PRECISION      LT
!           DOUBLE PRECISION      ET
!
!           INTEGER               HANDLE
!
!           CHARACTER*(20)        UTC
!
!           DATA                  UTC / '1979 JUN 25 12:00:00' /
!
!     C
!     C     Load the sample SPK ephemeris file.
!     C
!           CALL SPKLEF ( 'VG2_JUP.BSP', HANDLE )
!     C
!     C     Convert the UTC time string to ephemeris time.
!     C
!           CALL UTC2ET ( UTC, ET )
!     C
!     C     First calculate the geometric state and then the light
!     C     time corrected state.
!     C
!           CALL SPKEZ ( IO, ET, 'J2000', 'NONE', VG2, STATE, LT )
!
!           CALL VEQU  ( STATE, POS1 )
!
!           CALL SPKEZ ( IO, ET, 'J2000', 'LT', VG2, STATE, LT )
!
!           CALL VEQU  ( STATE, POS2 )
!     C
!     C     Call VREL to find the relative difference between the
!     C     two states.
!     C
!           DIFF = VREL ( POS1, POS2 )
!
!           .
!           .
!           .
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
!     J.M. Lynch         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 15-JUN-1992 (JML)
!
!-&


! $ Index_Entries
!
!     relative difference of 3-dimensional vectors
!
!-&


!
!     SPICELIB functions
!
      double precision      vnorm
      double precision      vdist


!
!     Local variables
!
      double precision      nunorm
      double precision      denorm






!
!     If the numerator is zero then set VREL equal to zero. Otherwise,
!     perform the rest of the calculation.
!
!     This handles the case where both vectors are zero vectors since
!     the distance between them will be zero.
!
      nunorm = vdist ( v1, v2 )

      if ( nunorm .eq. 0.d0 ) then

         vrel = 0.d0

      else

         denorm = max (  vnorm(v1), vnorm(v2)  )

         vrel = nunorm / denorm

       end if


      return
      end
