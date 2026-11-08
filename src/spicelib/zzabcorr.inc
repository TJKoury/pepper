
! $ Abstract
!
!     Include file zzabcorr.inc
!
!     SPICE private file intended solely for the support of SPICE
!     routines.  Users should not include this file directly due
!     to the volatile nature of this file
!
!     The parameters below define the structure of an aberration
!     correction attribute block.
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
! $ Parameters
!
!     An aberration correction attribute block is an array of logical
!     flags indicating the attributes of the aberration correction
!     specified by an aberration correction string.  The attributes
!     are:
!
!        - Is the correction "geometric"?
!
!        - Is light time correction indicated?
!
!        - Is stellar aberration correction indicated?
!
!        - Is the light time correction of the "converged
!          Newtonian" variety?
!
!        - Is the correction for the transmission case?
!
!        - Is the correction relativistic?
!
!    The parameters defining the structure of the block are as
!    follows:
!
!       NABCOR    Number of aberration correction choices.
!
!       ABATSZ    Number of elements in the aberration correction
!                 block.
!
!       GEOIDX    Index in block of geometric correction flag.
!
!       LTIDX     Index of light time flag.
!
!       STLIDX    Index of stellar aberration flag.
!
!       CNVIDX    Index of converged Newtonian flag.
!
!       XMTIDX    Index of transmission flag.
!
!       RELIDX    Index of relativistic flag.
!
!    The following parameter is not required to define the block
!    structure, but it is convenient to include it here:
!
!       CORLEN    The maximum string length required by any aberration
!                 correction string
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 18-DEC-2004 (NJB)
!
!-&


!     Number of aberration correction choices:
!
      INTEGER               NABCOR
      PARAMETER           ( NABCOR = 15 )

!
!     Aberration correction attribute block size
!     (number of aberration correction attributes):
!
      INTEGER               ABATSZ
      PARAMETER           ( ABATSZ = 6 )

!
!     Indices of attributes within an aberration correction
!     attribute block:
!
      INTEGER               GEOIDX
      PARAMETER           ( GEOIDX = 1 )

      INTEGER               LTIDX
      PARAMETER           ( LTIDX  = GEOIDX + 1 )

      INTEGER               STLIDX
      PARAMETER           ( STLIDX = LTIDX  + 1 )

      INTEGER               CNVIDX
      PARAMETER           ( CNVIDX = STLIDX + 1 )

      INTEGER               XMTIDX
      PARAMETER           ( XMTIDX = CNVIDX + 1 )

      INTEGER               RELIDX
      PARAMETER           ( RELIDX = XMTIDX + 1 )

!
!     Maximum length of an aberration correction string:
!
      INTEGER               CORLEN
      PARAMETER           ( CORLEN = 5 )

!
!     End of include file zzabcorr.inc
!
