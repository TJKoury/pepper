! $ Abstract
!
!     Declare ZZOCCED return code parameters, comparison strings
!     and other parameters.
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
!     GF
!
! $ Keywords
!
!     ELLIPSOID
!     GEOMETRY
!     GF
!     OCCULTATION
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
!-    SPICELIB Version 1.0.0, 01-SEP-2005 (NJB)
!
!-&


!     The function returns an integer code indicating the geometric
!     relationship of the three bodies.
!
!     Codes and meanings are:
!
!        -3                    Total occultation of first target by
!                              second.
!
      INTEGER               TOTAL1
      PARAMETER           ( TOTAL1 = -3 )

!
!        -2                    Annular occultation of first target by
!                              second.  The second target does not
!                              block the limb of the first.
!
      INTEGER               ANNLR1
      PARAMETER           ( ANNLR1 = -2 )

!
!        -1                    Partial occultation of first target by
!                              second target.
!
      INTEGER               PARTL1
      PARAMETER           ( PARTL1 = -1 )

!
!         0                    No occultation or transit:  both objects
!                              are completely visible to the observer.
!
      INTEGER               NOOCC
      PARAMETER           ( NOOCC  =  0 )

!
!         1                    Partial occultation of second target by
!                              first target.
!
      INTEGER               PARTL2
      PARAMETER           ( PARTL2 =  1 )

!
!         2                    Annular occultation of second target by
!                              first.
!
      INTEGER               ANNLR2
      PARAMETER           ( ANNLR2 =  2 )

!
!         3                    Total occultation of second target by
!                              first.
!
      INTEGER               TOTAL2
      PARAMETER           ( TOTAL2 =  3 )

!
!     End include file zzocced.inc
!

