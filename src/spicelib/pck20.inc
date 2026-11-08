! $ Abstract
!
!     Declare parameters specific to PCK type 20.
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
!     PCK
!
! $ Keywords
!
!     PCK
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman      (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 03-DEC-2013 (NJB)
!
!-&

!     MAXDEG         is the maximum allowed degree of the input
!                    Chebyshev expansions. If the value of MAXDEG is
!                    increased, the SPICELIB routine PCKMAT must be
!                    changed accordingly. In particular, the size of
!                    the record passed to PCKRnn and PCKEnn must be
!                    increased, and comments describing the record size
!                    must be changed.
!
      INTEGER               MAXDEG
      PARAMETER           ( MAXDEG = 50 )


!
!     DTOL,
!     TOLSCL         are, respectively, an absolute tolerance and
!                    a tolerance scale used for time coverage bound
!                    checking. DTOL has units of TDB seconds. TOLSCL
!                    is unitless. These two parameters are combined
!                    to produce a tolerance value via the formula
!
!                       TOL = MAX ( DTOL, TOLSCL * MAX( ABS(FIRST),
!                                                       ABS(LAST) ) )
!
!                    where FIRST and LAST are the coverage time bounds
!                    of a type 20 segment.
!
!                    The resulting parameter TOL is used as a tolerance
!                    for comparing the input segment descriptor time
!                    bounds to the first and last epoch covered by the
!                    sequence of time intervals defined by the inputs
!                    to PCKW20:
!
!                       INITJD
!                       INITFR
!                       INTLEN
!                       N

!
!     Absolute tolerance for coverage gap at endpoints
!     of segment coverage interval, in seconds:
!
      DOUBLE PRECISION      DTOL
      PARAMETER           ( DTOL   = 1.D-4  )

!
!     Relative tolerance for coverage gap at endpoints
!     of segment coverage interval:
!
      DOUBLE PRECISION      TOLSCL
      PARAMETER           ( TOLSCL = 1.D-12 )


!
!     End of include file pck20.inc.
!
