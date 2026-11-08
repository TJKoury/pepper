! $ Abstract
!
!     Include file sclk.inc
!
!     SPICE private file intended solely for the support of SPICE
!     routines.  Users should not include this file directly due
!     to the volatile nature of this file
!
!     The parameters below define sizes and limits used by
!     the SCLK system.
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
!     See the declaration section below.
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
!-    SPICELIB Version 2.0.1, 20-OCT-2020 (NJB)
!
!        Increased MXCOEF to 100000.
!
!        Updated comments with reminder to keep constants declared
!        in the include file zzsc01.inc synced with constants in
!        this file.
!
!-    SPICELIB Version 2.0.0, 24-MAY-2010 (NJB)
!
!        Increased value of maximum coefficient record count
!        parameter MXCOEF from 10K to 50K.
!
!-    SPICELIB Version 1.0.0, 11-FEB-2008 (NJB)
!
!-&

!
!        NOTE: many of the declarations present here are duplicated
!        in the include file zzsc01.inc. Declarations in that file
!        must be kept in sync with those in this file. The
!        duplicated declarations are:
!
!           NDELIM
!           DELIMS
!           MXPART
!           MXCOEF
!           MXNFLD
!           DPLEN
!
!
!     Number of supported SCLK field delimiters:
!
      INTEGER               NDELIM
      PARAMETER           ( NDELIM = 5 )

!
!     Supported SCLK string field delimiters:
!
      CHARACTER*(NDELIM)    DELIMS
      PARAMETER           ( DELIMS = '.:-, ' )

!
!     Maximum number of partitions:
!
      INTEGER               MXPART
      PARAMETER           ( MXPART = 9999 )

!
!     Partition string length.
!
!     Since the maximum number of partitions is given by MXPART is
!     9999, PRTSTR needs at most 4 characters for the partition number
!     and one character for the slash.
!
      INTEGER               PARTLN
      PARAMETER           ( PARTLN = 5 )

!
!     Maximum number of coefficient records:
!
      INTEGER               MXCOEF
      PARAMETER           ( MXCOEF = 100000 )

!
!     Maximum number of fields in an SCLK string:
!
      INTEGER               MXNFLD
      PARAMETER           ( MXNFLD = 10 )

!
!     Length of strings used to represent D.P.
!     numbers:
!
      INTEGER               DPLEN
      PARAMETER           ( DPLEN = 30 )

!
!     Maximum number of supported parallel time systems:
!
      INTEGER               MXTSYS
      PARAMETER           ( MXTSYS = 2 )

!
!     End of include file sclk.inc
!
