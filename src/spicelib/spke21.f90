!$Procedure SPKE21 ( S/P Kernel, evaluate, type 21 )

      subroutine spke21 ( et, record, state )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Evaluate a single SPK data record from a segment of type 21
!     (Extended Difference Lines).
!
! $ Required_Reading
!
!     SPK
!     TIME
!
! $ Keywords
!
!     EPHEMERIS
!
! $ Declarations

      implicit none

      include 'spk21.inc'

      real(wp)              et
      real(wp)              record   ( * )
      real(wp)              state    ( 6 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ET         I   Evaluation epoch.
!     RECORD     I   Data record.
!     STATE      O   State (position and velocity).
!     MAXTRM     P   Maximum number of terms per difference table
!                    component.
!
! $ Detailed_Input
!
!     ET       is an epoch at which a state vector is to be
!              computed. The epoch is represented as seconds past
!              J2000 TDB.
!
!     RECORD   is a data record which, when evaluated at epoch ET,
!              will give the state (position and velocity) of an
!              ephemeris object, relative to its center of motion,
!              in an inertial reference frame.
!
!              The contents of RECORD are as follows:
!
!                 RECORD(1):         The difference table size per
!                                    Cartesian component. Call this
!                                    size MAXDIM; then the difference
!                                    line (MDA) size DLSIZE is
!
!                                      ( 4 * MAXDIM ) + 11
!
!                 RECORD(2)
!                    ...
!                 RECORD(1+DLSIZE):  An extended difference line.
!                                    The contents are:
!
!                    Dimension  Description
!                    ---------  ----------------------------------
!                    1          Reference epoch of difference line
!                    MAXDIM     Stepsize function vector
!                    1          Reference position vector,  x
!                    1          Reference velocity vector,  x
!                    1          Reference position vector,  y
!                    1          Reference velocity vector,  y
!                    1          Reference position vector,  z
!                    1          Reference velocity vector,  z
!                    MAXDIM,3   Modified divided difference
!                               arrays (MDAs)
!                    1          Maximum integration order plus 1
!                    3          Integration order array
!
! $ Detailed_Output
!
!     STATE    is the state resulting from evaluation of the input
!              record at ET. Units are km and km/sec.
!
! $ Parameters
!
!     MAXTRM   is the maximum number of terms allowed in
!              each component of the difference table
!              contained in the input argument RECORD.
!              See the INCLUDE file spk21.inc for the value
!              of MAXTRM.
!
! $ Exceptions
!
!     1)  If the maximum table size of the input record exceeds
!         MAXTRM, the error SPICE(DIFFLINETOOLARGE) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The exact format and structure of type 21 (difference lines)
!     segments are described in the SPK Required Reading file.
!
!     SPKE21 is a modified version of SPKE01. The routine has been
!     generalized to support variable size difference lines.
!
! $ Examples
!
!     None.
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     F.T. Krogh         (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 14-APR-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Moved SPK
!        required reading from $Literature_References to
!        $Required_Reading section.
!
!-    SPICELIB Version 1.0.0, 03-FEB-2014 (NJB) (FTK) (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     evaluate type_21 SPK segment
!
!-&


!
!     SPICELIB functions
!
      logical               return

!
!     Local parameters
!

!
!     Local variables
!
!     The names below are original to the routine. They correspond
!     roughly to the original memos written by Fred Krogh to explain
!     how all this stuff really works.
!
      real(wp)              tl
      real(wp)              g        ( maxtrm )

      real(wp)              refpos   (    3 )
      real(wp)              refvel   (    3 )

      real(wp)              dt       ( maxtrm, 3 )
      integer(ip)           kqmax1
      integer(ip)           kq       (    3 )

      real(wp)              fc       ( maxtrm )


      real(wp)              sum
      real(wp)              delta
      real(wp)              tp
      real(wp)              wc       ( maxtrm-1 )
      real(wp)              w        ( maxtrm+2 )

      integer(ip)           maxdim
      integer(ip)           mq2
      integer(ip)           ks1
      integer(ip)           ks
      integer(ip)           kqq
      integer(ip)           i
      integer(ip)           j
      integer(ip)           jx

!
!     Save everything between calls.
!
      save

!
!     Initial values
!
      data                  fc (  1 )    / 1.0_wp /

!
!     Use discovery check-in.
!
!     If the RETURN function is set, don't even bother with this.
!
      if ( return () ) then
         return
      end if
!
!     The first element of the input record is the dimension
!     of the difference table MAXDIM.
!
      maxdim = nint ( record(1) )

      if ( maxdim > maxtrm ) then

         call chkin  ( 'SPKE21'                                )
         call setmsg ( 'The input record has a maximum table ' &
         //            'dimension of #, while the maximum ' &
         //            'supported by this routine is #. It is ' &
         //            'possible that this problem is due to ' &
         //            'your SPICE Toolkit being out of date.' )
         call errint ( '#',  maxdim                            )
         call errint ( '#',  maxtrm                            )
         call sigerr ( 'SPICE(DIFFLINETOOLARGE)'               )
         call chkout ( 'SPKE21'                                )
         return

      end if

!
!     Unpack the contents of the MDA array.
!
!        Name     Dimension  Description
!        ------   ---------  -------------------------------
!        TL               1  Reference epoch of record
!        G           MAXDIM  Stepsize function vector
!        REFPOS           3  Reference position vector
!        REFVEL           3  Reference velocity vector
!        DT      MAXDIM,NTE  Modified divided difference arrays
!        KQMAX1           1  Maximum integration order plus 1
!        KQ             NTE  Integration order array
!
!     For our purposes, NTE is always 3.
!

      call moved ( record( 2),      1, tl )
      call moved ( record( 3), maxdim, g  )

!
!     Collect the reference position and velocity.
!
      refpos(1) =  record(maxdim+3)
      refvel(1) =  record(maxdim+4)

      refpos(2) =  record(maxdim+5)
      refvel(2) =  record(maxdim+6)

      refpos(3) =  record(maxdim+7)
      refvel(3) =  record(maxdim+8)

!
!     Initializing the difference table is one aspect of this routine
!     that's a bit different from SPKE01. Here the first dimension of
!     the table in the input record can be smaller than MAXTRM. So, we
!     must transfer separately the portions of the table corresponding
!     to each component.
!
      do i = 1, 3
         call moved ( record(i*maxdim+9), maxdim, dt(1,i) )
      end do

      kqmax1    = int ( record(4*maxdim + 9 ) )
      kq(1)     = int ( record(4*maxdim + 10) )
      kq(2)     = int ( record(4*maxdim + 11) )
      kq(3)     = int ( record(4*maxdim + 12) )

!
!     Next we set up for the computation of the various differences
!
      delta     = et     - tl
      tp        = delta
      mq2       = kqmax1 - 2
      ks        = kqmax1 - 1

!
!     This is clearly collecting some kind of coefficients.
!     The problem is that we have no idea what they are...
!
!     The G coefficients are supposed to be some kind of step size
!     vector.
!
!     TP starts out as the delta t between the request time and the
!     difference line's reference epoch. We then change it from DELTA
!     by the components of the stepsize vector G.
!
      do j = 1, mq2
!
!        Make sure we're not about to attempt division by zero.
!
         if ( g(j) == 0.0_wp ) then

            call chkin  ( 'SPKE21'                             )
            call setmsg ( 'A  value of zero was found at ' &
            //            'index # of the step size vector.'   )
            call errint ( '#',  j                              )
            call sigerr ( 'SPICE(ZEROSTEP)'                    )
            call chkout ( 'SPKE21'                             )
            return

         end if

         fc(j+1) = tp    / g(j)
         wc(j)   = delta / g(j)
         tp      = delta + g(j)

      end do

!
!     Collect KQMAX1 reciprocals.
!
      do j = 1, kqmax1
         w(j) = 1.0_wp/real(j, wp)
      end do

!
!     Compute the W(K) terms needed for the position interpolation
!     (Note,  it is assumed throughout this routine that KS, which
!     starts out as KQMAX1-1 (the ``maximum integration'')
!     is at least 2.
!
      jx  = 0
      ks1 = ks - 1

      do while ( ks >= 2 )

         jx  = jx+1

         do  j = 1, jx
            w(j+ks) = fc(j+1)*w(j+ks1) - wc(j)*w(j+ks)
         end do

         ks  = ks1
         ks1 = ks1 - 1

      end do

!
!     Perform position interpolation: (Note that KS = 1 right now.
!     We don't know much more than that.)
!
      do  i = 1, 3

         kqq = kq(i)
         sum = 0.0_wp

         do j = kqq, 1, -1
            sum = sum + dt(j,i)*w(j+ks)
         end do

         state(i) = refpos(i)  +  delta * ( refvel(i) + delta*sum )

      end do

!
!     Again we need to compute the W(K) coefficients that are
!     going to be used in the velocity interpolation.
!     (Note, at this point, KS = 1, KS1 = 0.)
!
      do  j = 1, jx
         w(j+ks) = fc(j+1)*w(j+ks1) - wc(j)*w(j+ks)
      end do

      ks = ks - 1

!
!     Perform velocity interpolation:
!
      do  i = 1, 3

         kqq = kq(i)
         sum = 0.0_wp

         do j = kqq, 1, -1
            sum = sum + dt(j,i)*w(j+ks)
         end do

         state(i+3) = refvel(i) + delta*sum

      end do

      return
      end
