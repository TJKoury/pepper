!$Procedure ZZGETBFF ( Private --- Get Binary File Format )

      subroutine zzgetbff ( bffid )
      use pepper_kinds, only: ip, wp

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

      implicit none

      include              'zzddhman.inc'

      integer(ip)           bffid

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
      real(wp)              dpvalu
      parameter           ( dpvalu = 7.0_wp )

!
!     Local Variables
!
      integer(ip)           i

      real(wp)              dequiv
      integer(ip)           iequiv ( 2 )

      equivalence         ( dequiv, iequiv )

      integer(ip)           int1st ( numbff )
      integer(ip)           int2nd ( numbff )

      data                  int1st(bigi3e)  / 1075576832 /
      data                  int2nd(bigi3e)  / 0          /

      data                  int1st(ltli3e)  / 0          /
      data                  int2nd(ltli3e)  / 1075576832 /

      data                  int1st(vaxgfl)  / 16444      /
      data                  int2nd(vaxgfl)  / 0          /

      data                  int1st(vaxdfl)  / 16864      /
      data                  int2nd(vaxdfl)  / 0          /

      save                  int1st
      save                  int2nd

!
!     Copy DPVALU into the equivalenced DP, DEQUIV.
!
      dequiv = dpvalu

!
!     Examine the integer pairs, to identify the binary
!     file format.
!
      bffid = 0

      do i = 1, numbff

         if ( ( iequiv(1) .eq. int1st(i) ) .and. &
              ( iequiv(2) .eq. int2nd(i) )       ) then

            bffid = i

         end if

      end do

      return

      end
