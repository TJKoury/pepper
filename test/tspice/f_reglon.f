C$Procedure F_REGLON ( REGLON tests )
 
      SUBROUTINE F_REGLON ( OK )
 
C$ Abstract
C
C     Exercise the longitude "regularization" routine REGLON.
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
C     TEST FAMILY
C
C$ Declarations
 
      IMPLICIT NONE       
      
      LOGICAL               OK
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OK         O   logical indicating test status.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     OK         is a logical that indicates the test status to the
C                caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting. The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the DSKBRIEF supporting routine REGLON.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.0.0, 21-FEB-2017 (NJB)
C     
C        Updated for consistency with new behavior of REGLON.
C        REGLON now tries to avoid splitting longitude ranges
C        into two intervals.
C     
C        Previous version 12-OCT-2016 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      TWOPI

C
C     Local Parameters 
C
      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-12 )

      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )

      INTEGER               DBSIZE
      PARAMETER           ( DBSIZE = 100 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 320 )

C
C     Local Variables 
C  
      CHARACTER*(LNSIZE)    TITLE
  
      DOUBLE PRECISION      BOUNDS ( 2, DBSIZE )
      DOUBLE PRECISION      MAXLON
      DOUBLE PRECISION      MINLON
      DOUBLE PRECISION      OUTBDS ( 2, DBSIZE )
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      XBDS   ( 2, DBSIZE )

      INTEGER               MAXN
      INTEGER               NIVALS
      INTEGER               NOUT
      INTEGER               SIZE
      INTEGER               SRCS   ( DBSIZE )
      INTEGER               XNOUT
      INTEGER               XSRCS  ( DBSIZE )

 
C
C     Saved values
C
C     Save variables in order to avoid stack room problems.
C
      SAVE

C
C     Initial values
C

C
C     Open the test family.
C
      CALL TOPEN ( 'F_REGLON' )



C**********************************************************************
C
C     REGLON normal cases
C
C**********************************************************************

C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Trivial case: one interval in [0,2pi], no '
     .//       'change needed.' 

      CALL TCASE ( TITLE )
 
      NIVALS      = 1

      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = PI()

      MAXN        = 1

      CALL REGLON ( NIVALS, BOUNDS, MAXN,   NOUT,
     .              MINLON, MAXLON, OUTBDS, SRCS )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the output range.
C
      CALL CHCKSD ( 'MINLON', MINLON, '~', 0.D0,    VTIGHT, OK )
      CALL CHCKSD ( 'MAXLON', MAXLON, '~', TWOPI(), VTIGHT, OK )

C
C     Check number of output intervals.
C
      XNOUT    = 1

      CALL CHCKSI ( 'NOUT', NOUT, '=', XNOUT, 0, OK )

C
C     Check output intervals.
C
      SIZE = 2 * NOUT
      CALL MOVED ( BOUNDS, SIZE, XBDS )

      CALL CHCKAD ( 'OUTBDS', OUTBDS, '=', XBDS, SIZE, 0.D0, OK )

C
C     Check source intervals.
C
      XSRCS(1) = 1

      CALL CHCKAI ( 'SRCS', SRCS, '=', XSRCS, 1, OK )


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Trivial case: one interval in [-pi,pi], no '
     .//       'change needed.' 

      CALL TCASE ( TITLE )
 
      NIVALS      = 1

      BOUNDS(1,1) = -PI()/2
      BOUNDS(2,1) =  PI()/2

      MAXN        = 1

      CALL REGLON ( NIVALS, BOUNDS, MAXN,   NOUT,
     .              MINLON, MAXLON, OUTBDS, SRCS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the output range.
C
      CALL CHCKSD ( 'MINLON', MINLON, '~', -PI(), VTIGHT, OK )
      CALL CHCKSD ( 'MAXLON', MAXLON, '~',  PI(), VTIGHT, OK )


C
C     Check number of output intervals.
C
      XNOUT    = 1

      CALL CHCKSI ( 'NOUT', NOUT, '=', XNOUT, 0, OK )

C
C     Check output intervals.
C
      SIZE = 2 * NOUT
      CALL MOVED ( BOUNDS, SIZE, XBDS )

      CALL CHCKAD ( 'OUTBDS', OUTBDS, '=', XBDS, SIZE, 0.D0, OK )

C
C     Check source intervals.
C
      XSRCS(1) = 1

      CALL CHCKAI ( 'SRCS', SRCS, '=', XSRCS, 1, OK )



C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'One interval, endpoints are in reverse order, ' 
     .//       'range is  [-pi, pi], bounds have mixed signs.'

      CALL TCASE ( TITLE )
 
      NIVALS      = 1

      BOUNDS(1,1) =  PI()/2
      BOUNDS(2,1) = -PI()/2

      MAXN        = 2

      CALL REGLON ( NIVALS, BOUNDS, MAXN,   NOUT,
     .              MINLON, MAXLON, OUTBDS, SRCS )
      CALL CHCKXC ( .FALSE., ' ', OK )
       

C
C     Check the output range.
C
      CALL CHCKSD ( 'MINLON', MINLON, '~', 0.D0,    VTIGHT, OK )
      CALL CHCKSD ( 'MAXLON', MAXLON, '~', TWOPI(), VTIGHT, OK )

C
C     Check number of output intervals.
C
      XNOUT    = 1

      CALL CHCKSI ( 'NOUT', NOUT, '=', XNOUT, 0, OK )

C
C     Check output intervals.
C
      TOL       = VTIGHT

      XBDS(1,1) =  PI() / 2
      XBDS(2,1) =  1.5D0*PI()

      SIZE      =  2*NOUT

      CALL CHCKAD ( 'OUTBDS', OUTBDS, '~~', XBDS, SIZE, VTIGHT, OK )

C
C     Check source intervals.
C
      XSRCS(1) = 1

      CALL CHCKAI ( 'SRCS', SRCS, '=', XSRCS, 1, OK )


C --- Case: ------------------------------------------------------
C 
      TITLE =  'One interval, endpoints are in reverse order, ' 
     .//       'range is  [-pi, pi], bounds are both negative.'

      CALL TCASE ( TITLE )
 
      NIVALS      = 1

      BOUNDS(1,1) =  -PI()/4
      BOUNDS(2,1) =  -PI()/2

      MAXN        = 2

      CALL REGLON ( NIVALS, BOUNDS, MAXN,   NOUT,
     .              MINLON, MAXLON, OUTBDS, SRCS )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
C
C     Check number of output intervals.
C
      XNOUT    = 2

      CALL CHCKSI ( 'NOUT', NOUT, '=', XNOUT, 0, OK )

C
C     Check output intervals.
C
      TOL       = VTIGHT

      XBDS(1,1) = -PI()
      XBDS(2,1) = -PI()/2

      XBDS(1,2) = -PI()/4
      XBDS(2,2) =  PI()
      
      SIZE      = 2 * NOUT

      CALL CHCKAD ( 'OUTBDS', OUTBDS, '~~', XBDS, SIZE, VTIGHT, OK )

C
C     Check source intervals.
C
      XSRCS(1) = 1
      XSRCS(2) = 1

      CALL CHCKAI ( 'SRCS', SRCS, '=', XSRCS, 2, OK )


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'One interval, endpoints are in reverse order, ' 
     .//       'range is  [-pi, pi], only interval on right '
     .//       'of lower bound is produced.'

      CALL TCASE ( TITLE )
 
      NIVALS      = 1

      BOUNDS(1,1) =  PI()/2
      BOUNDS(2,1) = -PI()

      MAXN        = 2

      CALL REGLON ( NIVALS, BOUNDS, MAXN,   NOUT,
     .              MINLON, MAXLON, OUTBDS, SRCS )
      CALL CHCKXC ( .FALSE., ' ', OK )
       

C
C     Check the output range. We expect it to have been
C     adjusted.
C
      CALL CHCKSD ( 'MINLON', MINLON, '~',  0.D0,    VTIGHT, OK )
      CALL CHCKSD ( 'MAXLON', MAXLON, '~',  TWOPI(), VTIGHT, OK )

C
C     Check number of output intervals.
C
      XNOUT    = 1

      CALL CHCKSI ( 'NOUT', NOUT, '=', XNOUT, 0, OK )

C
C     Check output intervals.
C
      TOL       = VTIGHT

      XBDS(1,1) = PI()/2
      XBDS(2,1) = PI()
      
      SIZE      = 2 * NOUT

      CALL CHCKAD ( 'OUTBDS', OUTBDS, '~~', XBDS, SIZE, VTIGHT, OK )

C
C     Check source intervals.
C
      XSRCS(1) = 1

      CALL CHCKAI ( 'SRCS', SRCS, '=', XSRCS, 1, OK )


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Two intervals, one interval has '
     .//       'endpoints in reverse order, ' 
     .//       'range is [-pi, pi]. For reversed interval, '
     .//       'only interval on right '
     .//       'of lower bound is produced.'

      CALL TCASE ( TITLE )
 
      NIVALS      = 2

      BOUNDS(1,1) =  PI()/2
      BOUNDS(2,1) = -PI()

      BOUNDS(1,2) = -PI()/2
      BOUNDS(2,2) =  PI()/2


      MAXN        = 2

      CALL REGLON ( NIVALS, BOUNDS, MAXN,   NOUT,
     .              MINLON, MAXLON, OUTBDS, SRCS )
      CALL CHCKXC ( .FALSE., ' ', OK )
       
      CALL CHCKSD ( 'MINLON', MINLON, '~',  -PI(),    VTIGHT, OK )
      CALL CHCKSD ( 'MAXLON', MAXLON, '~',   PI(), VTIGHT, OK )

C
C     Check number of output intervals.
C
      XNOUT    = 2

      CALL CHCKSI ( 'NOUT', NOUT, '=', XNOUT, 0, OK )

C
C     Check output intervals.
C
      TOL       = VTIGHT

      XBDS(1,1) = PI()/2
      XBDS(2,1) = PI()

      XBDS(1,2) = -PI()/2
      XBDS(2,2) =  PI()/2
      
      SIZE      = 2 * NOUT

      CALL CHCKAD ( 'OUTBDS', OUTBDS, '~~', XBDS, SIZE, VTIGHT, OK )

C
C     Check source intervals.
C
      XSRCS(1) = 1
      XSRCS(2) = 2

      CALL CHCKAI ( 'SRCS', SRCS, '=', XSRCS, NOUT, OK )



C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'One interval, endpoints are in reverse order, ' 
     .//       'range is  [-pi, pi], only interval on left '
     .//       'of upper bound is produced.'

      CALL TCASE ( TITLE )
 
      NIVALS      = 1

      BOUNDS(1,1) =  PI()
      BOUNDS(2,1) = -PI()/2

      MAXN        = 2

      CALL REGLON ( NIVALS, BOUNDS, MAXN,   NOUT,
     .              MINLON, MAXLON, OUTBDS, SRCS )
      CALL CHCKXC ( .FALSE., ' ', OK )
       

C
C     Check the output range.
C
      CALL CHCKSD ( 'MINLON', MINLON, '~', 0.D0,    VTIGHT, OK )
      CALL CHCKSD ( 'MAXLON', MAXLON, '~', TWOPI(), VTIGHT, OK )

C
C     Check number of output intervals.
C
      XNOUT    = 1

      CALL CHCKSI ( 'NOUT', NOUT, '=', XNOUT, 0, OK )

C
C     Check output intervals.
C
      TOL       = VTIGHT

      XBDS(1,1) =  PI()
      XBDS(2,1) =  1.5D0*PI()
      
      SIZE      = 2 * NOUT

      CALL CHCKAD ( 'OUTBDS', OUTBDS, '~~', XBDS, SIZE, VTIGHT, OK )

C
C     Check source intervals.
C
      XSRCS(1) = 1

      CALL CHCKAI ( 'SRCS', SRCS, '=', XSRCS, 1, OK )




C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Two intervals, one interval has '
     .//       'endpoints in reverse order, ' 
     .//       'range is [-pi, pi]. For reversed interval, '
     .//       'only interval on left '
     .//       'of upper bound is produced.'

      CALL TCASE ( TITLE )
 
      NIVALS      = 2

      BOUNDS(1,1) =  PI()
      BOUNDS(2,1) = -PI()/2

      BOUNDS(1,2) = -PI()/2
      BOUNDS(2,2) =  PI()/2


      MAXN        = 2

      CALL REGLON ( NIVALS, BOUNDS, MAXN,   NOUT,
     .              MINLON, MAXLON, OUTBDS, SRCS )
      CALL CHCKXC ( .FALSE., ' ', OK )
       

C
C
C
      CALL CHCKSD ( 'MINLON', MINLON, '~',  -PI(),    VTIGHT, OK )
      CALL CHCKSD ( 'MAXLON', MAXLON, '~',   PI(), VTIGHT, OK )

C
C     Check number of output intervals.
C
      XNOUT    = 2

      CALL CHCKSI ( 'NOUT', NOUT, '=', XNOUT, 0, OK )

C
C     Check output intervals.
C
      TOL       = VTIGHT

      XBDS(1,1) = -PI()
      XBDS(2,1) = -PI()/2

      XBDS(1,2) = -PI()/2
      XBDS(2,2) =  PI()/2
      
      SIZE      = 2 * NOUT

      CALL CHCKAD ( 'OUTBDS', OUTBDS, '~~', XBDS, SIZE, VTIGHT, OK )

C
C     Check source intervals.
C
      XSRCS(1) = 1
      XSRCS(2) = 2

      CALL CHCKAI ( 'SRCS', SRCS, '=', XSRCS, NOUT, OK )


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'One interval, endpoints are in reverse order, ' 
     .//       'range is  [0, 2*pi].'

      CALL TCASE ( TITLE )
 
      NIVALS      = 1

      BOUNDS(1,1) =  PI()/2
      BOUNDS(2,1) =  PI()/4

      MAXN        = 2

      CALL REGLON ( NIVALS, BOUNDS, MAXN,   NOUT,
     .              MINLON, MAXLON, OUTBDS, SRCS )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check the output range.
C
      CALL CHCKSD ( 'MINLON', MINLON, '~', -PI(), VTIGHT, OK )
      CALL CHCKSD ( 'MAXLON', MAXLON, '~',  PI(), VTIGHT, OK )

     
C
C     Check number of output intervals.
C
      XNOUT    = 2

      CALL CHCKSI ( 'NOUT', NOUT, '=', XNOUT, 0, OK )

C
C     Check output intervals.
C
      TOL       = VTIGHT

      XBDS(1,1) = -PI()
      XBDS(2,1) =  PI()/4

      XBDS(1,2) = PI()/2
      XBDS(2,2) = PI()
      
      SIZE      = 2 * NOUT

      CALL CHCKAD ( 'OUTBDS', OUTBDS, '~~', XBDS, SIZE, VTIGHT, OK )

C
C     Check source intervals.
C
      XSRCS(1) = 1
      XSRCS(2) = 1

      CALL CHCKAI ( 'SRCS', SRCS, '=', XSRCS, 2, OK )

C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'One interval, endpoints are in reverse order, ' 
     .//       'range is  [0, 2*pi], only interval on right '
     .//       'of lower bound is produced.'

      CALL TCASE ( TITLE )
 
      NIVALS      = 1

      BOUNDS(1,1) =  PI()/2
      BOUNDS(2,1) =  0.D0

      MAXN        = 2

      CALL REGLON ( NIVALS, BOUNDS, MAXN,   NOUT,
     .              MINLON, MAXLON, OUTBDS, SRCS )
      CALL CHCKXC ( .FALSE., ' ', OK )
       

C
C     Check the output range.
C
      CALL CHCKSD ( 'MINLON', MINLON, '~', 0.D0,    VTIGHT, OK )
      CALL CHCKSD ( 'MAXLON', MAXLON, '~', TWOPI(), VTIGHT, OK )

C
C     Check number of output intervals.
C
      XNOUT    = 1

      CALL CHCKSI ( 'NOUT', NOUT, '=', XNOUT, 0, OK )

C
C     Check output intervals.
C
      TOL       = VTIGHT

      XBDS(1,1) = PI()/2
      XBDS(2,1) = TWOPI()
      
      SIZE      = 2 * NOUT

      CALL CHCKAD ( 'OUTBDS', OUTBDS, '~~', XBDS, SIZE, VTIGHT, OK )

C
C     Check source intervals.
C
      XSRCS(1) = 1

      CALL CHCKAI ( 'SRCS', SRCS, '=', XSRCS, 1, OK )



C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Two intervals, one interval has '
     .//       'endpoints in reverse order, ' 
     .//       'range is [0, 2*pi]. For reversed interval, '
     .//       'only interval on right '
     .//       'of lower bound is produced.'

      CALL TCASE ( TITLE )
 
      NIVALS      = 2

      BOUNDS(1,1) =  1.5D0*PI()
      BOUNDS(2,1) =  0.D0

      BOUNDS(1,2) =  PI()
      BOUNDS(2,2) =  1.5D0*PI()


      MAXN        = 2

      CALL REGLON ( NIVALS, BOUNDS, MAXN,   NOUT,
     .              MINLON, MAXLON, OUTBDS, SRCS )
      CALL CHCKXC ( .FALSE., ' ', OK )
       
      CALL CHCKSD ( 'MINLON', MINLON, '~', 0.D0,    VTIGHT, OK )
      CALL CHCKSD ( 'MAXLON', MAXLON, '~', TWOPI(), VTIGHT, OK )

C
C     Check number of output intervals.
C
      XNOUT    = 2

      CALL CHCKSI ( 'NOUT', NOUT, '=', XNOUT, 0, OK )

C
C     Check output intervals.
C
      TOL       = VTIGHT

      XBDS(1,1) = 1.5D0*PI()
      XBDS(2,1) = TWOPI()

      XBDS(1,2) =  PI()
      XBDS(2,2) =  1.5D0*PI()
      
      SIZE      = 2 * NOUT

      CALL CHCKAD ( 'OUTBDS', OUTBDS, '~~', XBDS, SIZE, VTIGHT, OK )

C
C     Check source intervals.
C
      XSRCS(1) = 1
      XSRCS(2) = 2

      CALL CHCKAI ( 'SRCS', SRCS, '=', XSRCS, NOUT, OK )




C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'One interval, endpoints are in reverse order, ' 
     .//       'range is  [0, 2*pi], only interval on left '
     .//       'of upper bound is produced.'

      CALL TCASE ( TITLE )
 
      NIVALS      = 1

      BOUNDS(1,1) =  TWOPI()
      BOUNDS(2,1) =  PI()/2

      MAXN        = 2

      CALL REGLON ( NIVALS, BOUNDS, MAXN,   NOUT,
     .              MINLON, MAXLON, OUTBDS, SRCS )
      CALL CHCKXC ( .FALSE., ' ', OK )
       

C
C     Check the output range.
C
      CALL CHCKSD ( 'MINLON', MINLON, '~', -PI(), VTIGHT, OK )
      CALL CHCKSD ( 'MAXLON', MAXLON, '~',  PI(), VTIGHT, OK )

C
C     Check number of output intervals.
C
      XNOUT    = 1

      CALL CHCKSI ( 'NOUT', NOUT, '=', XNOUT, 0, OK )

C
C     Check output intervals.
C
      TOL       = VTIGHT

      XBDS(1,1) = 0.D0
      XBDS(2,1) = PI()/2
      
      SIZE      = 2 * NOUT

      CALL CHCKAD ( 'OUTBDS', OUTBDS, '~~', XBDS, SIZE, VTIGHT, OK )

C
C     Check source intervals.
C
      XSRCS(1) = 1

      CALL CHCKAI ( 'SRCS', SRCS, '=', XSRCS, 1, OK )



C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Two intervals, one interval has '
     .//       'endpoints in reverse order, ' 
     .//       'range is [0, 2*pi]. For reversed interval, '
     .//       'only interval on left '
     .//       'of upper bound is produced.'

      CALL TCASE ( TITLE )
 
      NIVALS      = 2

      BOUNDS(1,1) =  TWOPI()
      BOUNDS(2,1) =  PI()/2

      BOUNDS(1,2) =  PI()
      BOUNDS(2,2) =  1.5D0*PI()


      MAXN        = 2

      CALL REGLON ( NIVALS, BOUNDS, MAXN,   NOUT,
     .              MINLON, MAXLON, OUTBDS, SRCS )
      CALL CHCKXC ( .FALSE., ' ', OK )
       
      CALL CHCKSD ( 'MINLON', MINLON, '~', -PI(), VTIGHT, OK )
      CALL CHCKSD ( 'MAXLON', MAXLON, '~',  PI(), VTIGHT, OK )

C
C     Check number of output intervals.
C
      XNOUT    = 2

      CALL CHCKSI ( 'NOUT', NOUT, '=', XNOUT, 0, OK )

C
C     Check output intervals.
C
      TOL       = VTIGHT

      XBDS(1,1) =  0.D0
      XBDS(2,1) =  PI()/2

      XBDS(1,2) =  -PI()
      XBDS(2,2) =  -PI()/2
      
      SIZE      = 2 * NOUT

      CALL CHCKAD ( 'OUTBDS', OUTBDS, '~~', XBDS, SIZE, VTIGHT, OK )

C
C     Check source intervals.
C
      XSRCS(1) = 1
      XSRCS(2) = 2

      CALL CHCKAI ( 'SRCS', SRCS, '=', XSRCS, NOUT, OK )




C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Two intervals in [0,2pi], no change needed.' 

      CALL TCASE ( TITLE )
 
      NIVALS      = 2

      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = PI()

      BOUNDS(1,2) = PI()/4
      BOUNDS(2,2) = PI()/3

      MAXN        = 2

      CALL REGLON ( NIVALS, BOUNDS, MAXN,   NOUT,
     .              MINLON, MAXLON, OUTBDS, SRCS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the output range.
C
      CALL CHCKSD ( 'MINLON', MINLON, '~', 0.D0,    VTIGHT, OK )
      CALL CHCKSD ( 'MAXLON', MAXLON, '~', TWOPI(), VTIGHT, OK )

C
C     Check number of output intervals.
C
      XNOUT    = 2

      CALL CHCKSI ( 'NOUT', NOUT, '=', XNOUT, 0, OK )

C
C     Check output intervals.
C
      SIZE = 2 * NOUT
      CALL MOVED ( BOUNDS, SIZE, XBDS )

      CALL CHCKAD ( 'OUTBDS', OUTBDS, '=', XBDS, SIZE, 0.D0, OK )

C
C     Check source intervals.
C
      XSRCS(1) = 1
      XSRCS(2) = 2

      CALL CHCKAI ( 'SRCS', SRCS, '=', XSRCS, NOUT, OK )


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Two intervals in [0,2pi], one interval has ' 
     .//       'reversed bounds.'

      CALL TCASE ( TITLE )
 
      NIVALS      = 2

      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = PI()

      BOUNDS(1,2) = PI()/3
      BOUNDS(2,2) = PI()/4

      MAXN        = 3

      CALL REGLON ( NIVALS, BOUNDS, MAXN,   NOUT,
     .              MINLON, MAXLON, OUTBDS, SRCS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the output range.
C
      CALL CHCKSD ( 'MINLON', MINLON, '~', -PI(), VTIGHT, OK )
      CALL CHCKSD ( 'MAXLON', MAXLON, '~',  PI(), VTIGHT, OK )

C
C     Check number of output intervals.
C
      XNOUT    = 3

      CALL CHCKSI ( 'NOUT', NOUT, '=', XNOUT, 0, OK )

C
C     Check output intervals.
C

      XBDS(1,1) = 0.D0
      XBDS(2,1) = PI()

      XBDS(1,2) = -PI()
      XBDS(2,2) =  PI()/4

      XBDS(1,3) = PI()/3
      XBDS(2,3) = PI()

      CALL CHCKAD ( 'OUTBDS', OUTBDS, '=', XBDS, SIZE, 0.D0, OK )

C
C     Check source intervals.
C
      XSRCS(1) = 1
      XSRCS(2) = 2
      XSRCS(3) = 2

      CALL CHCKAI ( 'SRCS', SRCS, '=', XSRCS, NOUT, OK )

      

C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Two intervals in [0,2pi], both intervals have ' 
     .//       'reversed bounds.'

      CALL TCASE ( TITLE )
 
      NIVALS      = 2

      BOUNDS(1,1) = PI()/4
      BOUNDS(2,1) = PI()/8

      BOUNDS(1,2) = PI()/3
      BOUNDS(2,2) = PI()/4

      MAXN        = 4

      CALL REGLON ( NIVALS, BOUNDS, MAXN,   NOUT,
     .              MINLON, MAXLON, OUTBDS, SRCS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the output range.
C
      CALL CHCKSD ( 'MINLON', MINLON, '~', -PI(), VTIGHT, OK )
      CALL CHCKSD ( 'MAXLON', MAXLON, '~',  PI(), VTIGHT, OK )

C
C     Check number of output intervals.
C
      XNOUT    = 4

      CALL CHCKSI ( 'NOUT', NOUT, '=', XNOUT, 0, OK )

C
C     Check output intervals.
C
      XBDS(1,1) = -PI()
      XBDS(2,1) =  PI()/8

      XBDS(1,2) =  PI()/4
      XBDS(2,2) =  PI()

      XBDS(1,3) = -PI()
      XBDS(2,3) =  PI()/4

      XBDS(1,4) =  PI()/3
      XBDS(2,4) =  PI()


      CALL CHCKAD ( 'OUTBDS', OUTBDS, '=', XBDS, SIZE, 0.D0, OK )

C
C     Check source intervals.
C
      XSRCS(1) = 1
      XSRCS(2) = 1
      XSRCS(3) = 2
      XSRCS(4) = 2

      CALL CHCKAI ( 'SRCS', SRCS, '=', XSRCS, NOUT, OK )




C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Two intervals in [-pi,pi], no change needed.' 

      CALL TCASE ( TITLE )
 
      NIVALS      = 2

      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = PI()/2

      BOUNDS(1,2) = -PI()/3
      BOUNDS(2,2) = -PI()/4

      MAXN        = 2

      CALL REGLON ( NIVALS, BOUNDS, MAXN,   NOUT,
     .              MINLON, MAXLON, OUTBDS, SRCS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the output range.
C
      CALL CHCKSD ( 'MINLON', MINLON, '~', -PI(), VTIGHT, OK )
      CALL CHCKSD ( 'MAXLON', MAXLON, '~',  PI(), VTIGHT, OK )

C
C     Check number of output intervals.
C
      XNOUT    = 2

      CALL CHCKSI ( 'NOUT', NOUT, '=', XNOUT, 0, OK )

C
C     Check output intervals.
C
      SIZE = 2 * NOUT
      CALL MOVED ( BOUNDS, SIZE, XBDS )

      CALL CHCKAD ( 'OUTBDS', OUTBDS, '=', XBDS, SIZE, 0.D0, OK )

C
C     Check source intervals.
C
      XSRCS(1) = 1
      XSRCS(2) = 2

      CALL CHCKAI ( 'SRCS', SRCS, '=', XSRCS, NOUT, OK )



C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Two intervals in [-2pi, 2pi], one interval must '
     .//       'be moved into the range [-pi, pi].'

      CALL TCASE ( TITLE )
 
      NIVALS      = 2

      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = PI()

      BOUNDS(1,2) = -3*PI()/2
      BOUNDS(2,2) = -  PI()

      MAXN        = 2

      CALL REGLON ( NIVALS, BOUNDS, MAXN,   NOUT,
     .              MINLON, MAXLON, OUTBDS, SRCS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the output range.
C
      CALL CHCKSD ( 'MINLON', MINLON, '~', -PI(), VTIGHT, OK )
      CALL CHCKSD ( 'MAXLON', MAXLON, '~',  PI(), VTIGHT, OK )

C
C     Check number of output intervals.
C
      XNOUT    = 2

      CALL CHCKSI ( 'NOUT', NOUT, '=', XNOUT, 0, OK )

C
C     Check output intervals.
C
      XBDS(1,1) = 0.D0
      XBDS(2,1) = PI()

      XBDS(1,2) =  PI()/2
      XBDS(2,2) =  PI()

      SIZE      = 2 * NOUT

      CALL CHCKAD ( 'OUTBDS', OUTBDS, '=', XBDS, SIZE, 0.D0, OK )

C
C     Check source intervals.
C
      XSRCS(1) = 1
      XSRCS(2) = 2

      CALL CHCKAI ( 'SRCS', SRCS, '=', XSRCS, NOUT, OK )


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Two intervals in [-2pi, 2pi], one interval has '
     .//       'reversed bounds and must be moved into the range '
     .//       '[-pi, pi].'

      CALL TCASE ( TITLE )
 
      NIVALS      = 2

      BOUNDS(1,1) =  0.D0
      BOUNDS(2,1) = -PI()/2

      BOUNDS(1,2) = -3*PI()/2
      BOUNDS(2,2) = -  PI()

      MAXN        = 3

      CALL REGLON ( NIVALS, BOUNDS, MAXN,   NOUT,
     .              MINLON, MAXLON, OUTBDS, SRCS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the output range.
C
      CALL CHCKSD ( 'MINLON', MINLON, '~', -PI(), VTIGHT, OK )
      CALL CHCKSD ( 'MAXLON', MAXLON, '~',  PI(), VTIGHT, OK )

C
C     Check number of output intervals.
C
      XNOUT    = 3

      CALL CHCKSI ( 'NOUT', NOUT, '=', XNOUT, 0, OK )

C
C     Check output intervals.
C
      XBDS(1,1) = -PI()
      XBDS(2,1) = -PI()/2

      XBDS(1,2) =  0.D0
      XBDS(2,2) =  PI()

      XBDS(1,3) =  PI()/2
      XBDS(2,3) =  PI()

      SIZE      = 2 * NOUT

      CALL CHCKAD ( 'OUTBDS', OUTBDS, '=', XBDS, SIZE, 0.D0, OK )

C
C     Check source intervals.
C
      XSRCS(1) = 1
      XSRCS(2) = 1
      XSRCS(3) = 2

      CALL CHCKAI ( 'SRCS', SRCS, '=', XSRCS, NOUT, OK )



C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Two intervals in [-2pi, 2pi], both intervals have '
     .//       'reversed bounds and must be moved into the range '
     .//       '[-pi, pi].'

      CALL TCASE ( TITLE )
 
      NIVALS      = 2

      BOUNDS(1,1) =  0.D0
      BOUNDS(2,1) = -PI()/2

      BOUNDS(1,2) = -3*PI()/2
      BOUNDS(2,2) = -7*PI()/4

      MAXN        = 4

      CALL REGLON ( NIVALS, BOUNDS, MAXN,   NOUT,
     .              MINLON, MAXLON, OUTBDS, SRCS )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Check the output range.
C
      CALL CHCKSD ( 'MINLON', MINLON, '~', -PI(), VTIGHT, OK )
      CALL CHCKSD ( 'MAXLON', MAXLON, '~',  PI(), VTIGHT, OK )

C
C     Check number of output intervals.
C
      XNOUT    = 4

      CALL CHCKSI ( 'NOUT', NOUT, '=', XNOUT, 0, OK )

C
C     Check output intervals.
C
      XBDS(1,1) = -PI()
      XBDS(2,1) = -PI()/2

      XBDS(1,2) =  0.D0
      XBDS(2,2) =  PI()

      XBDS(1,3) =  -PI()
      XBDS(2,3) =   PI()/4

      XBDS(1,4) =  PI()/2
      XBDS(2,4) =  PI()



      CALL CHCKAD ( 'OUTBDS', OUTBDS, '=', XBDS, SIZE, 0.D0, OK )

C
C     Check source intervals.
C
      XSRCS(1) = 1
      XSRCS(2) = 1
      XSRCS(3) = 2
      XSRCS(4) = 2

      CALL CHCKAI ( 'SRCS', SRCS, '=', XSRCS, NOUT, OK )


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Two intervals in [-2pi, 2pi], both intervals have '
     .//       'reversed bounds and must be moved into the range '
     .//       '[-pi, pi]. For the first input interval, only the '
     .//       'interval to the right of the lower bound is '
     .//       'produced. For the second input interval, only the '
     .//       'interval to the left of the upper bound is '
     .//       'produced.'

      CALL TCASE ( TITLE )
 
      NIVALS      = 2



      BOUNDS(1,1) =  3*PI()/2
      BOUNDS(2,1) =    PI()

      BOUNDS(1,2) =   -PI()
      BOUNDS(2,2) = -7*PI()/4

      MAXN        = 4

      CALL REGLON ( NIVALS, BOUNDS, MAXN,   NOUT,
     .              MINLON, MAXLON, OUTBDS, SRCS )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Check the output range.
C
      CALL CHCKSD ( 'MINLON', MINLON, '~', -PI(), VTIGHT, OK )
      CALL CHCKSD ( 'MAXLON', MAXLON, '~',  PI(), VTIGHT, OK )

C
C     Check number of output intervals.
C
      XNOUT    = 2

      CALL CHCKSI ( 'NOUT', NOUT, '=', XNOUT, 0, OK )

C
C     Check output intervals.
C
      XBDS(1,1) =  -PI()/2
      XBDS(2,1) =   PI()

      XBDS(1,2) =  -PI()
      XBDS(2,2) =   PI()/4

      SIZE      = 2 * NOUT

      CALL CHCKAD ( 'OUTBDS', OUTBDS, '=', XBDS, SIZE, 0.D0, OK )

C
C     Check source intervals.
C
      XSRCS(1) = 1
      XSRCS(2) = 2

      CALL CHCKAI ( 'SRCS', SRCS, '=', XSRCS, NOUT, OK )





C**********************************************************************
C
C     REGLON error cases
C
C**********************************************************************


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Output array is too small.' )

      NIVALS      = 2

      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = PI()
      BOUNDS(1,2) =  PI()/2
      BOUNDS(2,2) = -PI()/2

      MAXN        = 1

      CALL REGLON ( NIVALS, BOUNDS, MAXN,   NOUT,
     .              MINLON, MAXLON, OUTBDS, SRCS )

      CALL CHCKXC ( .TRUE., 'SPICE(ARRAYTOOSMALL)', OK )


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Longitudes are out of range.' )

      NIVALS      = 2

      BOUNDS(1,1) = -3*PI()
      BOUNDS(2,1) = PI()
      BOUNDS(1,2) =  PI()/2
      BOUNDS(2,2) = -PI()/2

      MAXN        = 3

      CALL REGLON ( NIVALS, BOUNDS, MAXN,   NOUT,
     .              MINLON, MAXLON, OUTBDS, SRCS )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


      BOUNDS(1,1) = -2*PI()
      BOUNDS(2,1) =  3*PI()
      BOUNDS(1,2) =  PI()/2
      BOUNDS(2,2) = -PI()/2

      MAXN        = 3

      CALL REGLON ( NIVALS, BOUNDS, MAXN,   NOUT,
     .              MINLON, MAXLON, OUTBDS, SRCS )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Longitude bounds are equal.' )

      NIVALS      = 2

      BOUNDS(1,1) = -2*PI()
      BOUNDS(2,1) = -2*PI()
      BOUNDS(1,2) =  PI()/2
      BOUNDS(2,2) = -PI()/2

      MAXN        = 3

      CALL REGLON ( NIVALS, BOUNDS, MAXN,   NOUT,
     .              MINLON, MAXLON, OUTBDS, SRCS )

      CALL CHCKXC ( .TRUE., 'SPICE(ZEROBOUNDSEXTENT)', OK )


      BOUNDS(1,1) = -2*PI()
      BOUNDS(2,1) =  2*PI()
      BOUNDS(1,2) =  PI()/2
      BOUNDS(2,2) =  PI()/2

      MAXN        = 3

      CALL REGLON ( NIVALS, BOUNDS, MAXN,   NOUT,
     .              MINLON, MAXLON, OUTBDS, SRCS )

      CALL CHCKXC ( .TRUE., 'SPICE(ZEROBOUNDSEXTENT)', OK )


C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


